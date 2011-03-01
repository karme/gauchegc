;;;
;;;
;;;
(use ggc.file.ddf)
(use ggc.util) ; pianissimo

(with-module ggc.util (set! pp-indent-level 1))
(with-module ggc.file.ddf (set! debug #f))

(define (print-sadr sadr)
  (if (null? sadr) 
      #t
      (begin
        (format #t "(~a ~a)~%" 
                (list-ref (car sadr) 0)
                (list-ref (ca  sadr) 1))
        (print-sadr (cdr sadr)))))
;;;
;;;
;;;
(define (test-ddf)
  (receive (ddr drs) (read-ddf)
    (print "==== DDR =====")
    (pianissimo ddr)
    (pianissimo (ddr-get-labels 'ATTP ddr))
    (pianissimo (ddr-get-labels 'IREF ddr))

    (print "==== DR  =====")
    (pianissimo drs)
    
    (print "==== IREF =====")
    (pianissimo (ddf-get-value 'IREF (car drs) ddr))
    (newline)
    (print "==== XREF =====")
    (pianissimo (ddf-get-value 'XREF (car drs) ddr))
    (newline)
    (print "==== SADR =====")
    (for-each (lambda (dr)
                (print (ddf-get-value 'LINE dr ddr))
                (print-sadr (ddf-get-value 'SADR dr ddr)))
              drs)))
#|
(string->list #*"hogehoge")
(substring #*"hohoge" 2 4)
(string->symbol #*"hoge")
(string->symbol #*"0000")

#\x03p = 0x0370 (format #t "~16,'0b" #x0370)
#\x03q = 0x0371 (format #t "~16,'0b" #x0371)

(format #t "~16,'0b" #x7103)
0111 0001000 00011
5432 1098765 43210
0000 0011011 10000
(use rfc.md5)
(digest-hexify (md5-digest-string "hohsdklfjafdasdfkjho"))

|#

;;;
;;;
;;;
(define (main args)
  (if (= (length args) 2)
      (with-input-from-file (cadr args) test-ddf)
      (error "usage ddf.scm file")))

; EOF
