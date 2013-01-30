;;;
;;; huge sparse (persistent) bitmap
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
;;; - allow #t as default bit value?!
;;; - rename to huge-sparse-persistent-bitmap?
;;; - allow to use without filename (create temporary db?!)
;;; - or maybe constructor should take dbm object instead of filename?
;;;   (or dict object? what about sync?)
;;; - use gauche's object system?
;;;

(define-module huge-sparse-bitmap
  (use dbm.gdbm)
  (use binary.pack)
  (use gauche.collection)
  ;;(use sxml.adaptor) ;; for assert
  (use rfc.zlib)
  (use lru-cache)
  (export make-huge-sparse-bitmap
          huge-sparse-bitmap-get-bit
          huge-sparse-bitmap-set-bit!
          huge-sparse-bitmap-unset-bit!
          huge-sparse-bitmap-sync
          ))

(select-module huge-sparse-bitmap)

;; disable assert
(define-macro (assert e) )

(define (enc-ber n)
  (assert (exact? n))
  (pack "w" (list n) :to-string? #t))

(define (dec-ber s)
  (assert (string? s))
  (let1 n (car (unpack "w" :from-string s))
    (assert (exact? n))
    n))

(define (enc-zip n)
  (assert (exact? n))
  (deflate-string (number->string n 32) :window-bits -15))

(define (dec-zip s)
  (assert (string? s))
  (let1 n (string->number (inflate-string s :window-bits -15) 32)
    (assert (exact? n))
    n))

(define (enc-zip-2 n)
  (assert (exact? n))
  (deflate-string (number->string n 32) :window-bits -15 :strategy Z_RLE))

(define (dec-zip-2 s)
  (assert (string? s))
  (let1 n (string->number (inflate-string s :window-bits -15) 32)
    (assert (exact? n))
    n))

;; uh
;; maybe use something more like:
;; (with-output-to-string (lambda() (write-uint 64 255)))
(define (enc-bin n)
  (let1 bs (number->string n 2)
    (pack "b*" (list (string-append (make-string (- 8 (modulo (size-of bs) 8)) #\0)
                                    bs))
          :to-string? #t)))

(define (dec-bin s)
  (string->number (car (unpack "b*" :from-string s)) 2))

(define enc-base32 (cute number->string <> 32))
(define dec-base32 (cute string->number <> 32))

;; (define enc number->string)
;; (define dec string->number)

;; todo: not really a good encoding for us!
;;(define enc enc-ber)
;;(define dec dec-ber)

;; (define enc enc-zip)
;; (define dec dec-zip)

(define (real-pair? x)
  (and (pair? x) (not (list? x))))

(define (slot? s)
  (and (real-pair? s)
       (exact? (car s))
       (exact? (cdr s))))

(define (slot-set-bit s x v)
  (assert (slot? s))
  (let1 r (cons (car s)
                (copy-bit x (cdr s) v))
    (assert (slot? r))
    r))

(define (slot-get-bit s x)
  (assert (slot? s))
  (logbit? x (cdr s)))

(define bitstring (cut number->string <> 2))

(define (bit-stats n)
  (let1 bs (bitstring n)
    ;; todo: use integer-length/bit-count/first-set-bit/logcount?!
    (/ (size-of (filter (cut eq? #\1 <>) bs)) (size-of bs))))

(define (encode&decode name)
  (apply values
         (assoc-ref `((zip    . (,enc-zip ,dec-zip))
                      (zip-2  . (,enc-zip-2 ,dec-zip-2))
                      (ber    . (,enc-ber ,dec-ber))
                      (bin    . (,enc-bin ,dec-bin))
                      (base32 . (,enc-base32 ,dec-base32)))
                    name)))

(define (make-huge-sparse-bitmap filename
                                 :key
                                 (slot-size 1024)
                                 (key-code 'base32)
                                 (value-code 'zip-2)
                                 (cache-size 16)
                                 )
  
  ;;#?=(list slot-size key-code value-code cache-size)
  (let ((db (dbm-open <gdbm> :path filename :rw-mode :write)))
    (receive (enc-key dec-key) (encode&decode key-code)
      (receive (enc-value dec-value) (encode&decode value-code)
        (let ((read-slot-value (lambda(k)
                                 (assert (exact? k))
                                 (if-let1 v (dbm-get db (enc-key k) #f)
                                   (dec-value v)
                                   0)))
              (write-slot-value (lambda(k v)
                                  (assert (exact? k))
                                  (dbm-put! db (enc-key k) (enc-value v))
                                  )))
          (let1 cache (if (> cache-size 0)
                        (make-lru-cache read-slot-value write-slot-value :cache-size cache-size)
                        '())

            (define read-slot
              (let1 get (assoc-ref cache 'get read-slot-value)
                (lambda(sid)
                  (cons sid (get sid)))))
            
            (define write-slot!
              (let1 put! (assoc-ref cache 'put! write-slot-value)
                (lambda(s)
                  (put! (car s) (cdr s)))))
            
            (define (set-bit! b v)
              (receive (q r) (quotient&remainder b slot-size)
                (write-slot!
                 (slot-set-bit
                  (read-slot q) r v)))
              v)
            
            (define (get-bit b)
              (receive (q r) (quotient&remainder b slot-size)
                (slot-get-bit (read-slot q) r)))
            
            (define sync
              (let1 cache-sync (assoc-ref cache 'sync (lambda ()))
                (lambda()
                  (cache-sync)
                  ;; todo: generic dbm api is missing sync
                  ;; how to sync db then?
                  ;; we can only close and re-open?
                  ;; for now use gdbm specific api
                  (assert (ref db 'gdbm-file))
                  (gdbm-sync (ref db 'gdbm-file)))))
            
            `((set!  . ,set-bit!)
              (get   . ,get-bit)
              (sync  . ,sync))))))))

(define (huge-sparse-bitmap-get-bit bm b)
  ((assoc-ref bm 'get) b))

(define (huge-sparse-bitmap-set-bit! bm b :optional (v #t))
  ((assoc-ref bm 'set!) b v))

(define (huge-sparse-bitmap-unset-bit! bm b)
  ((assoc-ref bm 'set!) b #f))

(define (huge-sparse-bitmap-sync bm)
  ((assoc-ref bm 'sync)))
