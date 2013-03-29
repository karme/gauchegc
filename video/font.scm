;;;
;;; simple font module
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

(define-module font
  (use ftgl)
  ;;(use ggc.file.bdf)
  (use gauche.process)
  (use gauche.collection)
  (use gauche.sequence)
  (use gl)
  (use srfi-1)
  (export font-get-string-bbox
          font-get-string))

(select-module font)

;; load a ftgl font
;; \returns list of:
;; closure painting a string
;; closure maping a string to its bbox
(define (font-load-ftgl font-name font-size)
  (let ((font (ftgl-load-font font-name font-size)))
    (let ((font-height (ftgl-get-font-line-height font)))
      (list
       (cut ftgl-render-font font <>)
       (lambda(s)
         (let1 ret (ftgl-get-font-advance font s)
           (ftgl-check-error font)           
           (list
            ret
            font-height)))))))

;; load a bdf font
;; \returns list of:
;; closure painting a string
;; closure maping a string to its bbox
(define (font-load-bdf-2 bdf)

  ;; return number of bits needed to represent x bits in bdf font
  (define (bits-needed x)
    (* (quotient (+ x 7) 8) 8))
  
  ;; map bdf bitmap to list of (x . y) pairs (with pixels set)
  ;; (suitable as input for paint-quads)
  (define (bitmap-to-quads w h bitmap)
    (fold-right
     append
     '()
     (map-with-index 
      (lambda (y b)
	(fold-right
	 append
	 '()
	 (map
	  (lambda(x)
	    ;;	  #?=(list b w x (- w x) (logbit? (- w x) b))
	    (if (logbit? (- (bits-needed w) x) b)
              `((,x . ,(- h y)))
              '()))
	  (iota w 1 1))))
      bitmap)))

  (define (paint-quads l)
    ;; (print l)
    (when (not (null? l))
      (gl-begin GL_QUADS)
      (for-each
       (lambda(q)
         (gl-vertex    (car q)       (cdr q)   )
         (gl-vertex (+ (car q) 1)    (cdr q)   )
         (gl-vertex (+ (car q) 1) (+ (cdr q) 1))
         (gl-vertex    (car q)    (+ (cdr q) 1)))
       l)
      (gl-end)))

  ;; create display list for bdf character
  ;; return closure to display char and bbox
  ;; todo: at least use texture
  (define (char-to-displaylist char)
    (let* ((ret #?=(gl-gen-lists 1))
	   (bbx (bdf-char-bbx char))
	   (xoff (caddr bbx))
	   (yoff (cadddr bbx)))
      #?=(gl-new-list ret GL_COMPILE)
      (gl-translate xoff yoff 0)
      ;;      #?=(list (integer->char (bdf-char-encoding char))
      ;;	       (bdf-char-encoding char)
      ;;	       bbx
      ;;	       (bdf-char-dwidth char))
      (paint-quads (bitmap-to-quads (car bbx) (cadr bbx)
				    (bdf-char-bitmap char)))
      (gl-translate (- (car (bdf-char-dwidth char)) xoff)
		    (- yoff)
		    0)
      (gl-end-list)
      (list (cut gl-call-list ret)
            (cons (car bbx) (+ (cadr bbx))))))

  ;; using a lookup table because bdf-find-char-by-encoding did not work
  (let ((lut (make-hash-table))
        (bbox (bdf-fontboundingbox bdf)))
    (for-each-char
     (lambda(char)
       (hash-table-put! lut (bdf-char-encoding char) char))
     bdf)
    (list
     (lambda(s)
       (for-each
        (lambda(char)
          (let* ((ichar (char->integer char))
                 (c (hash-table-get lut ichar #f)))
            (cond
             [(procedure? c)
              (c)]
             [c
              (let ((p (car (char-to-displaylist c))))
                (hash-table-put! lut ichar p)
                (p))]
             [else ;; char not available
              ;; todo
              ])))
        s))
     (lambda(s)
       (list (ref bbox 0)
             (* (size-of s) (ref bbox 1)))))))

(define (font-load-bdf name . l)
  (font-load-bdf-2
   (if (file-exists? name)
     (with-input-from-file name read-bdf)
     (with-input-from-process 
         (string-append "fstobdf -server unix/:7100 -fn " name)
       read-bdf))))

;; store results of expensive functions in a cache
;; \todo: could this be replaced with delayed evaluation?
;; (s.a. srfi-45 and "Delayed evaluation" in gauche manual)
;; \param func the function to cache results
;; \param l optional arguments passed to make-hash-table (typically this will be equal?)
;; \return object allowing to call function func via cached-function-call
(define (make-cached-function func . l)
  (cons func (apply make-hash-table l)))

;; call a cached function
(define (cached-function-call cf . l)
  ;; like hash-table-put! but returns the value
  (define (my-hash-table-put! ht key value)
    ;; (print "my-hash-table-put! called")
    (hash-table-put! ht key value)
    ;; #?=ht
    value)
  
  (if (hash-table-exists? (cdr cf) l)
    (hash-table-get (cdr cf) l)
    (my-hash-table-put! (cdr cf) l (apply (car cf) l))))

(define (font-load impl . l)
  (apply
   (case impl
     [(ftgl) font-load-ftgl]
     [(bdf)  font-load-bdf]
     [else => (error "unknown implementation" impl)])
   l))

(define cf-font-load
  (make-cached-function font-load 'equal?))

(define (font-get . l)
  (apply cached-function-call (cons cf-font-load l)))

(define (font-get-string . font)
  (car (apply font-get font)))

(define (font-get-string-bbox . font)
  (cadr (apply font-get font)))
