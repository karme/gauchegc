;;;
;;; Least Recently Used (LRU) (write-back, write allocate) cache
;;;
;;;   Copyright (c) 2013 Jens Thiele <karme@karme.de>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;; todos:
;;; - use gauche's object system?
;;;
(define-module lru-cache
  ;;(use sxml.adaptor) ;; for assert
  ;;(use gauche.sequence)
  (use util.list)
  (export make-lru-cache))

(select-module lru-cache)

;; disable assert
(define-macro (assert e) )

;; todo: maybe replace with macros to improve performance
(define (make-lru-list)
  (let* ((sentinel (list 'sentinel))
         (lru-list (list sentinel))
         (last-pair lru-list)
         (size 0))
    ;; add item at end of list and return previous pair
    ;; you can use the return value with lru-remove! and lru-update!
    ;; (but see caveats of lru-remove! and lru-update! below!)
    (define (lru-add! item)
      (let1 new (list item)
        (set-cdr! last-pair new)
        (let1 r last-pair
          (set! last-pair new)
          (inc! size)
          r)))
    
    ;; remove item by specifying previous item, return item
    ;; note: this invalidates prev-item->next->next->prev-item!
    ;; (we could use lru-pop! if prev-item is sentinel
    ;; but imho it isn't worth it)
    (define (lru-remove! prev-item)
      (assert (pair? prev-item))
      (assert (not (null? prev-item)))
      (assert (> size 0))
      (let1 item (cadr prev-item)
        ;; (set! (cadr prev-item) sentinel) ;; not really necessary
        ;;(let1 s (size-of lru-list)
        ;; todo: maybe just forbid?!
        (when (eq? (cdr prev-item) last-pair)
          (set! last-pair prev-item))
        (set-cdr! prev-item (cddr prev-item))
        ;;(assert (= (- s 1) (size-of lru-list))))
        (dec! size)
        item))
    
    ;; move item to back by specifying previous item
    ;; return: new previous item
    ;; note: this invalidates prev-item->next->next->prev-item!
    (define (lru-update! prev-item)
      (assert (pair? prev-item))
      (assert (not (null? prev-item)))
      ;;(assert (member (cadr prev-item) lru-list))
      (lru-add! (lru-remove! prev-item)))
    
    (define (lru-null?)
      (assert (pair? lru-list))
      (assert (eq? sentinel (car lru-list)))
      (null? (cdr lru-list)))

    ;; remove first item in list
    ;; (but keep possible prev-item intact!)
    (define (lru-pop!)
      (assert (not (lru-null?)))
      (assert (> size 0))
      (set! lru-list (cdr lru-list))
      (let1 r (car lru-list)
        (set-car! lru-list sentinel)
        (dec! size)
        r))

    (define (lru-clear!)
      (set! lru-list (list sentinel))
      (set! last-pair lru-list)
      (set! size 0))
    
    `((add! . ,lru-add!)
      (update! . ,lru-update!)
      (null? . ,lru-null?)
      (pop! . ,lru-pop!)
      (size . ,(lambda() size))
      (list . ,(cut cdr lru-list))
      (clear! . ,lru-clear!))))

(define (make-lru-cache read-miss
                        write-back
                        :key
                        (cache-size 4)
                        (hash-table-type 'equal?))
  (let ((cache (make-hash-table 'equal?))
        (dirty (make-hash-table 'equal?))
        (lru-list (make-lru-list)))
    (let ((lru-update!     (assoc-ref lru-list 'update!))
          (lru-add!        (assoc-ref lru-list 'add!))
          (lru-pop!        (assoc-ref lru-list 'pop!))
          (lru-list        (assoc-ref lru-list 'list))
          (lru-list-size   (assoc-ref lru-list 'size))
          (lru-list-clear! (assoc-ref lru-list 'clear!)))

      (define (cache-lookup key)
        (and (hash-table-exists? cache key)
             ;; cache hit
             ;; used => move to back (if not already at back)
             (let* ((v (hash-table-get cache key))
                    (prev (cdr v)))
               (when (not (null? (cddr prev)))
                 (let1 next (caddr prev)
                   (set-cdr! v (lru-update! prev))
                   ;; note: we also have to update next->prev!
                   (set-cdr! (hash-table-get cache next) prev)))
               v)))
      
      (define (cache-miss key v)
        (hash-table-put! cache key (cons v (lru-add! key)))
        (when (> (lru-list-size) cache-size)
          ;; remove lru => front of the lru-list
          (let1 to-remove (lru-pop!)
            (when (hash-table-exists? dirty to-remove)
              (write-back to-remove (car (hash-table-get cache to-remove)))
              (hash-table-delete! dirty to-remove))
            (hash-table-delete! cache to-remove)))
        v)
      
      (define (get key)
        (let1 cache-entry (cache-lookup key)
          (cond [cache-entry
                 (car cache-entry)]
                [else
                 ;; cache miss
                 (cache-miss key (read-miss key))])))
      
      (define (put! key value)
        (if-let1 cache-entry (cache-lookup key)
          ;; cache hit
          ;; update value
          (set! (car cache-entry) value)
          ;; cache miss
          (cache-miss key value))
        ;; mark dirty
        (hash-table-put! dirty key #t)
        value)

      (define (sync)
        (for-each (lambda(k)
                    (assert (hash-table-exists? cache k))
                    (write-back k (car (hash-table-get cache k))))
                  (hash-table-keys dirty))
        (hash-table-clear! dirty))

      (define (flush)
        (sync)
        (hash-table-clear! cache)
        (lru-list-clear!)
        )
      
      `((get  . ,get)
        (put! . ,put!)
        (sync . ,sync)
        (flush . ,flush)
        (keys . ,(compose list-copy lru-list)) ;; todo: really expose? really copy?
        ))))

;; note: dict API is missing dict-clear!
;; but overhead using generic dict was too big anyway
;; => for now stay with hash-table

;; (define-method dict-clear! ((dict <dictionary>))
;;   (dict-for-each dict (lambda(k v) (dict-delete! dict k))))

;; (define-method dict-clear! ((dict <hash-table>))
;;   (hash-table-clear! dict))
