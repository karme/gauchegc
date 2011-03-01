(define-module ggc.file.ddf-sdts-dlg
  (use gauche.process)
  (use srfi-1)
  (use srfi-13)
  (use file.util)
  (use ggc.file.ddf)
  (export make-sdts/dlg
          sdts/dlg-files
          sdts/dlg-layers
          sdts/dlg-read
          )
  )

(select-module ggc.file.ddf-sdts-dlg)

(define (make-sdts/dlg file)
  (cond
   ((file-is-directory? file)
    (make-sdts/dlg-dir  file))
   ((file-is-regular?   file)
    (make-sdts/dlg-tgz  file))
   (else
    (error "wrong sdts data"))))

(define (make-sdts/dlg-tgz tgz)

  (define (dir)
    (with-input-from-process
        (string-append "gzip -dc " tgz " | tar t ")
      (lambda ()
        ((setter port-buffering) (current-input-port) :full)
        (port-map (lambda (x) x) read-line))))

  (define (dir-mod mod)
    (map (lambda (x) (string-drop-right x 8))
         (filter! (lambda (x)
                    (string-suffix? (string-append mod ".DDF") x))
                  (dir))))

  (lambda (m)
    (case m
      ((read)
       (lambda (f)
         (with-input-from-process
             (string-append "gzip -dc " tgz " | tar Ox " f)
           (lambda ()
             ((setter port-buffering) (current-input-port) :full)
             (read-ddf)))))

      ((files)
       (dir))

      ((layers)
       (lambda (mod)
         (if (string? mod)
             (dir-mod mod)
             (dir-mod "IREF"))))

      (else
       (error "unknown command" m)))))


(define (make-sdts/dlg-dir d)

  (define (dir-mod mod)
    (map (lambda (x) (string-drop-right x 8))
         (filter! (lambda (x)
                    (string-suffix? (string-append mod ".DDF") x))
                  (sys-readdir d))))

  (lambda (m)
    (case m
      ((read)
       (lambda (f)
         (with-input-from-file (string-append d "/" f) read-ddf)))

      ((files)     
       (remove! (lambda (x) 
                  (or (string=? x ".")
                      (string=? x "..")))
                (sys-readdir d)))

      ((layers)
       (lambda (mod)
         (if (string? mod)
             (dir-mod mod)
             (dir-mod "IREF"))))
      (else
       (error "unknown command" m)))))
      

; (string-prefix? "BD01" "BD01IREF.DDF") -> #t
; (string-suffix? "IREF.DDF" "BD01IREF.DDF") -> #t

(define (sdts/dlg-layers sdts . mod)
  (if (null? mod)
      ((sdts 'layers) #t)
      ((sdts 'layers) (car mod))))

(define (sdts/dlg-files sdts)
  (sdts 'files))

(define (sdts/dlg-read sdts file)
  ((sdts 'read) file))


(provide "ggc/file/ddf-sdts-dlg")
