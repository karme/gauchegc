(use ggc.file.ddf)
(use ggc.file.ddf-sdts-dlg)
(use ggc.util)

(define (print-sadr sadr)
  (if (null? sadr) #t
      (begin
        (format #t "(~a ~a)~%" 
                (caar sadr)
                (cadar sadr))
        (print-sadr (cdr sadr)))))

(define (test name)
  (let ((sdts (make-sdts/dlg name)))
    (let ((files  (sdts/dlg-files  sdts))
          (layers (sdts/dlg-layers sdts #t)))
      
      (format #t "== FILES  ==~%")
      (pianissimo files)
      (format #t "== LAYERS ==~%")
      (pianissimo layers)

      (receive (ddr drs) (sdts/dlg-read
                          sdts 
                          (string-append (car layers) "LE01.DDF"))
        (for-each (lambda (dr)
                    (print (ddf-get-value 'LINE dr ddr))
                    (print-sadr (ddf-get-value 'SADR dr ddr)))
                  drs)
        0))))

;; (member "foo" '("moo" "uu" "foo" "bar"))

(define (main args)
  (if (= (length args) 2)
      (test (cadr args))
      (error "usage: test-ddf-sdts dir-or-tgz")))
