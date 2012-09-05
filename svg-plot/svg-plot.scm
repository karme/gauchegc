;;;
;;; simple SVG 2d plot using gnuplot
;;;
;;;   Copyright (c) 2012 Jens Thiele <karme@karme.de>
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
(define-module svg-plot
  (use gauche.process)
  (use file.util)
  (use util.list)
  (export svg-plot))

(select-module svg-plot)

(define (with-output-to-gnuplot x)
  (let* ((p (run-process '(gnuplot) :input :pipe :output (current-output-port)))
         (r (with-output-to-port (process-input p) x)))
    (process-wait p)
    r))

(define (list->data-file l)
  ;; todo:
  ;; - better nan handling?
  ;; - what about rational/complex numbers?
  ;; - better raise an error?
  ;; - use binary data-file format?
  ;; - do not use temporary file (just use -) ?
  (for-each (lambda(x)
              (apply print
                     (intersperse " "
                                  (map (lambda(n) (if (or (not (number? n))
                                                          (and (number? n)
                                                               (nan? n)))
                                                    0
                                                    n)) x))))
            l))

(define (svg-plot l . args)
  (let-optionals* args ((title #f))
    (with-output-to-gnuplot
     (lambda()
       (print "set style data linespoints")
       (print "set terminal svg") ;; size 800, 600 fname \"Sans\" fsize 8")
       ;;(print "set grid")
       (print (string-append "plot \"-\""
                             (if title
                               #`" title \",|title|\""
                               " notitle")))
       (list->data-file l)
       (print "e")
       (print "exit")))))
