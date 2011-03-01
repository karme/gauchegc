(use srfi-1)
(use srfi-13)
(use gauche.process)
(use gauche.vport)
(use file.util)
(use ggc.util.circular-list)

;;(use ggc.text.segment)
(add-load-path ".")
(load "segment.scm")
(import ggc.text.segment)

;;;
;;;
;;;
(define (file-is-readonly? file)
  (not (sys-access file W_OK)))

;;;
;;; EDITOR
;;;
(define-class <editor> ()
  ((frames :init-value '()
           :accessor frames-of)
   (buffers :init-value '()
            :accessor buffers-of)))

(define the-editor       #f)
(define (buffer-list)    (buffers-of the-editor))
(define (current-buffer) (car (buffer-list)))
(define (current-frame)  (car (frames-of the-editor)))
(define (current-window) (current-window-of (current-frame)))

(define (init-the-editor)
  (let ((emin  (make <editor>))
        (bmsg  (make <text-buffer> :name "*Messages*"))
        (frame (make <dumb-frame>))
        (wmsg  (make <text-window>)))
    (set! (buffer-of wmsg) bmsg)
    (set! (length-of wmsg) (- (height-of frame) 2))
    (window-follow-buffer-point wmsg)
    (set! (windows-of frame) (list wmsg))
    (set! (current-window-of frame) wmsg)
    (set! (buffers-of emin) (circular-list bmsg))
    (set! (frames-of  emin) (list frame))
    (set! the-editor  emin))

  ;; we can use editor commands from here
  (let ((sch (get-buffer-create "*scratch*")))
    (split-window)
    (set! (buffer-of (current-window-of (current-frame))) sch)
    (select-buffer sch)))

;;;
;;; TEXT-BUFFER CLASS
;;;
(define-class <text-buffer> ()
  ((text      :init-value   '() 
              :init-keyword :text        
              :accessor text-of)
   (point     :init-value   0 
              :init-keyword :point       
              :accessor point-of)
   (name      :init-value "untitled" 
              :init-keyword :name 
              :accessor name-of)
   (modified? :init-value #f
              :accessor is-modified?)
   (readonly? :init-value #f
              :accessor is-readonly?)
   (filename  :init-value #f 
              :init-keyword :filename
              :accessor filename-of)
   (mtime     :init-value #f  ;; mtime of the file last visited or saved.
              :accessor mtime-of)
   (history   :init-value '()
              :init-keyword :history 
              :accessor history-of)))

(define-class <input-text-buffer-port> (<virtual-input-port>)
  ((text-buffer :init-keyword :text-buffer
                :accessor text-buffer-of)))

(define-class <output-text-buffer-port> (<virtual-output-port>)
  ((text-buffer :init-keyword :text-buffer
                :accessor text-buffer-of)))
   
(define (open-input-text-buffer buf)
  (let ((port (make <input-text-buffer-port> :text-buffer buf)))
    (define (getc)
      (let ((ch (text-get-character (text-of  buf)
                                    (point-of buf))))
        (if ch (inc! (point-of buf)))
        ch))
    (slot-set! port 'getc getc)
    port))

(define (open-output-text-buffer buf)
  (let ((port (make <output-text-buffer-port> :text-buffer buf)))
    (define (putc ch)
      (insert (text-buffer-of port) (string ch)))
    (define (puts str)
      (insert (text-buffer-of port) str))
    (slot-set! port 'putc putc)
    (slot-set! port 'puts puts)
    port))

(define (make-history-element buf)    (cons (point-of buf) (text-of buf)))
(define (history-element->point hist) (car hist))
(define (history-element->text hist)  (cdr hist))

(define (buffer-push-history buf)
  (if (null? (history-of buf))
      (set! (history-of buf) (circular-list (make-history-element buf)))
      (circular-push! (history-of buf) (make-history-element buf))))

(define (buffer-pop-history buf)
  ;; so called ``undo''
  (if (null? (history-of buf))
      (error "buffer does not have history")
      (let ((hist (circular-pop! (history-of buf))))
        (set! (point-of buf) (history-element->point hist))
        (set! (text-of buf)  (history-element->text  hist)))))

(define (buffer-set-filename buf fname)
  (set! (filename-of buf) fname)
  (if (file-is-regular? fname)
      (buffer-set-mtime buf)))

(define (buffer-set-mtime buf)
  (if (filename-of buf)
      (set! (mtime-of buf)
            (sys-stat->mtime (sys-stat (filename-of buf))))
      (error "buffer is not associated with file")))

(define (buffer-match-mtime? buf)
  (and (mtime-of buf)
       (filename-of buf)
       (= (mtime-of buf)
          (sys-stat->mtime (sys-stat (filename-of buf))))))

(define (buffer-load-from-file buf fname)
  (if (and (file-exists? fname)
           (file-is-regular? fname))
      (let ((str (file->string fname)))
        (set! (text-of buf) (list str))
        (set! (point-of buf) 0)
        (set! (is-modified? buf) #f)
        (if (file-is-readonly? fname)
            (set! (is-readonly? buf) #t))
        (buffer-set-filename buf fname)
        (set! (history-of buf) '()))
      (error "Could not read file")))

(define (buffer-save-to-file buf fname)
  (buffer-flatten-text buf)
  (call-with-output-file fname
    (lambda (p) (display (car (text-of buf)) p)))
  (set! (is-modified? buf) #f)
  (buffer-set-filename buf fname)
  (format #t "Wrote ~s~%" fname))

(define (buffer-flatten-text buf)
  (set! (text-of buf) (list (text->string (text-of buf)))))

(define (buffer-erase buf)   (buffer-replace-text buf '()))

;;;
;;;  BUFFER MANAGEMENT
;;;
(define (make-unique-buffer-name name)
  (let lp ((cnd name)
           (n   1))
    (cond ((get-buffer cnd)
           (lp (format #f "~a<~d>" cnd n) (+ n 1)))
          (else cnd))))

(define (create-buffer name)
  (let ((buf (make <text-buffer> :name (make-unique-buffer-name name))))
    (circular-push! (buffer-list) buf)
    buf))

(define (select-buffer-function pred)
  (if (circular-find-and-bring-it-top! (buffer-list) pred)
      (current-buffer)
      (error "No such buffer")))

(define-method select-buffer ((name <string>))
  (select-buffer-function (lambda (e) 
                            (string=? (name-of e) name))))

(define-method select-buffer ((buf <text-buffer>))
  (select-buffer-function (lambda (e) (eq? e buf))))

(define (kill-buffer name-or-buf)
  (let ((buf (select-buffer name-or-buf)))
    (if buf
        (begin
          (set! (text-of buf)     '())
          (set! (point-of buf)     0)
          (set! (name-of buf)      #f)
          (set! (is-modified? buf) #f)
          (set! (is-readonly? buf) #t)
          (set! (filename-of buf)  #f)
          (set! (mtime-of buf)     #f)
          (set! (history-of buf)  '())
          (circular-pop! (buffer-list)))
        #f)))

(define (find-buffer pred) 
  (let ((p (circular-find (buffer-list) pred)))
    (if p (car p) #f)))

(define (get-buffer name)
  (find-buffer (lambda (e)
                 (string=? name (name-of e)))))

(define (get-buffer-filename name)
  (find-buffer (lambda (e)
                 (and (filename-of e)
                      (string=? name (filename-of e))))))

(define (get-buffer-create name)
  (let ((buf (get-buffer name)))
    (if buf buf (create-buffer name))))

(define (print-buffers)
  (define (prn buf)
    (format #t "  ~20,,,20:a ~8d   ~,,,,30:a~%"
            (if (> (string-length (name-of buf)) 20)
                (substring (name-of buf) 
                           (- (string-length (name-of buf)) 20)
                           (string-length (name-of buf)))
                (name-of buf))
            (text-size (text-of buf))
            (filename-of buf)))

  (format #t "  ~20,,,20:a ~8@a   ~,,,,30:a~%" "Name" "Size" "File")
  (let lp ((p (buffer-list)))
    (prn (car p))
    (if (eq? (cdr p) (buffer-list)) 
        #t
        (lp (cdr p)))))

(define (list-buffers)
  (let* ((buf  (get-buffer-create "*Buffer List*"))
         (port (open-output-text-buffer buf)))
    (erase buf)
    (with-output-to-port port print-buffers)
    (close-output-port port)
    (select-buffer buf)))

(define-syntax define-command
  (syntax-rules ()
    ((_ sym (buf) body ...)
     (begin
       (define-method sym ((buf <text-buffer>))
         body ...)
       (define-method sym ()
         (sym (current-buffer)))))
    ((_ sym (buf arg) body ...)
     (begin
       (define-method sym ((buf <text-buffer>) arg)
         body ...)
       (define-method sym (arg)
         (sym (current-buffer) arg))))
    ((_ sym (buf arg default) body ...)
     (begin
       (define-method sym ((buf <text-buffer>) arg)
         body ...)
       (define-method sym ((buf <text-buffer>))
         (sym buf default))
       (define-method sym (arg)
         (sym (current-buffer) arg))
       (define-method sym ()
         (sym (current-buffer) default))))))

;;;
;;; FILE
;;;
(define (find-file-noselect fname)
  (if (relative-path? fname)
      (find-file-noselect (build-path (current-directory) fname))
      (cond ((get-buffer-filename fname)
             => (lambda (x) x))
            
            ((not (file-exists? fname))
             (let ((buf (create-buffer (sys-basename fname))))
               (buffer-set-filename buf fname)
               buf))

            ((file-is-regular? fname)
             (let ((buf (create-buffer (sys-basename fname))))
               (buffer-load-from-file buf fname)
               buf))

            ((file-is-directory? fname)
             (let* ((buf (get-buffer-create fname))
                    (out (open-output-text-buffer buf)))
               (erase buf)
               (call-with-input-process (format #f "ls -l ~a" fname)
                 (lambda (in) 
                   (copy-port in out)))
               (close-output-port out)
               (beginning-of-buffer buf)
               buf))

            (else
             (else "something wrong")
             #f))))

(define (find-file fname)
  (let ((buf (find-file-noselect fname)))
    (if buf (select-buffer buf))))

(define-command save-buffer (buf)
  (cond ((not (filename-of buf))
         (error "buffer has no filename"))
        ((not (is-modified? buf))
         (print "(No changes need to be saved)"))
        ((not (mtime-of buf))  ; virgin file
         (buffer-save-to-file buf (filename-of buf)))
        ((not (buffer-match-mtime? buf))
         (error "File changed on disk"))
        (else
         (buffer-save-to-file buf (filename-of buf)))))

;;;
;;; Edit commands
;;;
(define-method error-if-readonly ((buf <text-buffer>))
  (if (is-readonly? buf) (error "Read only buffer")))

(define-method insert ((buf <text-buffer>) (str <string>))
  (error-if-readonly buf)
  (let ((new-text (text-insert (text-of buf) (point-of buf) str)))
        (buffer-push-history buf)
        (set! (is-modified? buf) #t)
        (set! (text-of buf) new-text)
        (inc! (point-of buf) (string-length str))))
(define-method insert ((buf <text-buffer>) x)
  (insert buf (x->string x)))
(define-method insert (x)  (insert (current-buffer) x))

(define-command delete (buf count)
  (error-if-readonly buf)
  (let ((new-text (text-delete (text-of buf) (point-of buf) count)))
    (buffer-push-history buf)
    (set! (is-modified? buf) #t)
    (set! (text-of buf) new-text)))

(define (buffer-replace-text buf text)
  (error-if-readonly buf)
  (buffer-push-history buf)
  (set! (is-modified? buf) #t)
  (set! (text-of buf) text)
  (set! (point-of buf) 0))

(define-command erase (buf)  (buffer-erase buf))

(define-command beginning-of-line (buf count 1)
  (set! (point-of buf) 
        (text-beginning-of-line (text-of  buf) (point-of buf) count)))
    
(define-command end-of-line (buf count 1)
  (set! (point-of buf)
        (text-end-of-line (text-of buf) (point-of buf) count)))

(define-command goto-line (buf n)
  (let* ((e (text-end-of-line (text-of buf) 0 n))
         (b (text-beginning-of-line (text-of buf) e 1)))
    (set! (point-of buf) b)))

(define-command beginning-of-buffer (buf)
  (set! (point-of buf) 0))

(define-command end-of-buffer (buf)
  (set! (point-of buf) (text-size (text-of buf))))

(define-command previous-line (buf n 1)
  (set! (point-of buf) 
        (text-previous-line (text-of buf) (point-of buf) n)))

(define-command next-line (buf n 1)
  (set! (point-of buf) 
        (text-next-line (text-of buf) (point-of buf) n)))

(define-command forward-char (buf n 1)
  (let ((size (text-size (text-of buf))))
    (inc! (point-of buf) n)
    (cond ((< (point-of buf) 0)
           (set! (point-of buf) 0)
           (print "Beginning of buffer"))
          ((< size (point-of buf))
           (set! (point-of buf) size)
           (print "End of buffer")))))

(define-command backward-char (buf n 1)  (forward-char buf (- n)))

(define-command search-forward (buf str)
  (receive (p txt) 
      (text-search-forward (text-of buf) (point-of buf) str)
    (cond (p (set! (point-of buf) (+ p (string-length str)))
             #t)
          (else (format #t "\"~a\" not found~%" str)
                #f))))

(define-command search-backward (buf str)
  (receive (p txt) 
      (text-search-backward (text-of buf) (point-of buf) str)
    (cond (p (set! (point-of buf) (if (> p 0) p 0))
             #t)
          (else
           (format #t "\"~a\" not found~%" str)
           #f))))

(define-command point (buf) (point-of buf))
(define (point-min)  0)
(define-command point-max (buf)
  (text-size (text-of (current-buffer))))

;;;
;;;  TEXT-WINDOW AND FRAME
;;;
(define-class <text-window> ()
  ((buffer :init-keyword :buffer
           :accessor buffer-of)
   (start  :init-value 1        ;; starting line number for display
           :init-keyword :start
           :accessor start-of)
   (length :init-value 23       ;; number of lines
           :init-keyword :length
           :accessor length-of)
   (point  :init-value 0        ;; cursor point
           :accessor point-of)))

(define (window-follow-buffer-point win) (set! (point-of win) -1))

(define-class <frame> ()
  ((windows :init-value '()
            :accessor windows-of)
   (current-window :init-value #f
                   :accessor current-window-of)
   (width   :init-value 80
            :init-keyword :width
            :accessor width-of)
   (height  :init-value 25
            :init-keyword :height
            :accessor height-of)))

(define-method display-frame ((frame <frame>)) #t)
(define-method display-frame ()  (display-frame (current-frame)))
(define (frame-sanity-check) #t)

(define-method split-window ((frame <frame>))
  (define (error-if-something-wrong len)
    (if (or (<= len 3) (= (quotient (- len 1) 2) 0))
        (error "Current Window is too small")))
  (let* ((win (current-window-of frame))
         (len (length-of win)))
    (error-if-something-wrong len)
    (let ((new-win (make <text-window>))
          (new-len (quotient (- len 1) 2)))
      (set! (buffer-of new-win) (buffer-of win))
      (set! (length-of new-win) new-len)
      (set! (point-of  new-win) (point-of win))
      (set! (length-of win) (- (length-of win) new-len 1))
      (let lp ((w (windows-of frame))
               (r '()))
        (cond ((null? w) (error "Current window is not belong to the frame"))
              ((eq? (car w) win)
               (set! (windows-of frame) 
                     (append (reverse r) (cons new-win w))))
              (else (lp (cdr w) (cons (car w) r)))))
      (set! (current-window-of frame) new-win)
      (frame-sanity-check)
      new-win)))
(define-method split-window ()  (split-window (current-frame)))

(define-method delete-window ((frame <frame>) (win <text-window>))
  (cond ((<= (length (windows-of frame)) 1)
         (error "Can't delete the only window"))
        ((not (memq win (windows-of frame)))
         (error "Window does not belong to the frame")))
  (inc! (length-of (next-window frame)) (+ 1 (length-of win)))
  (set! (windows-of frame) (delete! win (windows-of frame) eq?))
  (if (eq? win (current-window))
      (set! (current-window-of frame) (car (windows-of frame)))))
(define-method delete-window ((frame <frame>))
  (delete-window frame (current-window)))
(define-method delete-window ((win <text-window>))
  (delete-window (current-frame) win))
(define-method delete-window ()
  (delete-window (current-frame) (current-window)))

(define-method select-window ((frame <frame>) (win <text-window>))
  (if (not (memq win (windows-of frame)))
      (error "Window does not belong to the frame"))
  (set! (current-window-of frame) win))
(define-method select-window ((win <text-window>))
  (select-window (current-frame) win))

(define-method next-window ((frame <frame>))
  (let ((w (memq (current-window) (windows-of frame))))
    (cond ((and w (null? (cdr w)))
           (car (windows-of frame)))
          (w (cadr w))
          (else 
           (error "Current window does not belong to current frame")))))
(define-method next-window () (next-window (current-frame)))

;;;
;;; DUMB FRAME :-)
;;;
(define-class <dumb-frame> (<frame>) ())

(define (print-text text from to)
  (for-each display (text-get-text text from (- to from)))
  (newline))

(define-method print-line ((buf <text-buffer>) lino)
  (if (> lino 0)
      (let* ((text (text-of buf))
             (nol  (text-number-of-lines text 0))
             (end  (text-end-of-line text 0 lino))
             (beg  (text-beginning-of-line text end 1)))
        (if (> lino nol)
            (print "~")
            (print-text text beg end)))
      (print-text (text-of buf) 0 (text-size (text-of buf)))))
(define-method print-line (lino) (print-line (current-buffer) lino))

(define-method display-frame ((frame <dumb-frame>))
  (define (dumb-display-window win cursor?)
    (let* ((buf  (buffer-of win))
           (name (name-of  buf))
           (text (text-of  buf))
           (pos  (if (= (point-of win) -1)
                     (point-of buf)
                     (point-of win)))
           (beg  (text-beginning-of-line text pos 1))
           (ofst (- pos beg))
           (segs (length text))
           (cln  (text-line-number text pos))
           (tln  (text-number-of-lines text 0)))

      (if (<= tln (length-of win)) (set! (start-of win) 1))
      (if (< cln (start-of win))   (set! (start-of win) cln))
      (if (>= cln (+ (start-of win) (length-of win)))
          (set! (start-of win) (- cln -1 (length-of win))))
    
      (for-each (lambda (n)
                  (format #t "~4d: " n)
                  (print-line buf n)
                  (if (and cursor? (= n cln))
                      (format #t "      ~a^~%" (make-string ofst #\space))))
                (iota (length-of win) (start-of win)))

      (format #t "~80,,,'-a~%"
              (format #f "-~a ~,,,,30a  L~d P~d S~d" 
                      (cond ((is-modified? buf) #\*)
                            ((is-readonly? buf) #\%)
                            (else #\-))
                      name cln pos segs))))

  (with-output-to-port (standard-output-port)
    (lambda ()
      (for-each (lambda (win)
                  (dumb-display-window win (eq? win (current-window))))
                (windows-of frame)))))

;;;
;;;  COMMAND LOOP
;;;
(define (with-output-and-error-to-port port thunk)
  (with-error-to-port port (lambda () (with-output-to-port port thunk))))

(define (run)
  (init-the-editor)
  (sync-buffer-and-window)
  (let ((errp (open-output-text-buffer (get-buffer-create "*Messages*"))))
    (with-output-and-error-to-port errp the-loop)))

(define sync-buffer-and-window
  (let ((prev-buf #f)
        (prev-win #f))
    (lambda ()
      (if (and prev-win prev-buf)
          (begin
            (if (not (eq? prev-win (current-window)))
                (select-buffer (buffer-of (current-window))))
            (if (not (eq? prev-buf (current-buffer)))
                (set! (buffer-of (current-window)) (current-buffer)))
            (set! (point-of (current-window)) (point-of (current-buffer)))
            (set! prev-buf  (current-buffer))
            (set! prev-win  (current-window)))
          (begin
            (set! prev-buf  (current-buffer))
            (set! prev-win  (current-window))) ))))

(define (the-loop)
  (define (update-display)
    (sync-buffer-and-window)
    (display-frame))

  (with-error-handler (lambda (e)
                        (display (slot-ref e 'message)
                                 (current-error-port))
                        (newline (current-error-port))
                        (update-display))
    (lambda ()
      (let ((exp (read)))
        (if (eof-object? exp) (exit 0))
        (format (standard-output-port) "~%Cmd: ~s~%" exp)
        (write/ss (eval exp interaction-environment))
        (newline)
        (update-display))))
  (the-loop))

(define (main args)
  (if (= (length args) 2)
      (with-input-from-file (cadr args) run)
      (run)))

;;; EOF
