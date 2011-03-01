;;;
;;; glscreen.scm:   A demonstration to
;;; display ASCII and EUC characters to OpenGL Window through  
;;; virtual port.  EUC characters are presumed to have double
;;; width to ASCII characters, just as Japanese EUC.
;;;
;;; Gauche-gl/examples/glbook/example8-2.scm is used as 
;;; staring point.
;;;

(use gl)
(use gl.glut)
(use gauche.uvector)
(use gauche.vport)
(use ggc.file.bdf)

(define *screen-width* 80)
(define *screen-height* 25)
(define *screen-width-pixel* #f)
(define *screen-height-pixel* #f)
(define *font-height* #f)
(define *font-width* #f)
(define *font-h-spacing* 2)
(define *font-v-spacing* 4)
(define *ssh* 4)
(define *ssv* 4)

(define *font-offset* #f)

(define *char-width*  #f)
(define *char-height* #f)
(define *ystart* #f)
(define *screen* #f)
(define *empty-line* #f)
(define *screen-port* #f)

(define *ascii-bdf-file* "bdf/a14.bdf")
(define *euc-bdf-file*   "bdf/k14.bdf")
;;(define *ascii-bdf-file* "bdf/12x24.bdf")
;;(define *euc-bdf-file* "bdf/jiskan24.bdf")

(define (init-fonts)
  (let* ((abdf (with-input-from-file *ascii-bdf-file* read-bdf))
         (kbdf (with-input-from-file *euc-bdf-file* read-bdf))
         ;;(kbdf #f)
         (bbx  (bdf-fontboundingbox abdf)))

    (set! *font-width*  (list-ref bbx 0))
    (set! *font-height* (list-ref bbx 1))

    (set! *screen-width-pixel*  
          (+ *ssh* *ssh* 
             (* *screen-width* 
                (+ *font-h-spacing* *font-width*))))

    (set! *screen-height-pixel* 
          (+ *ssv* *ssv* 
             (* *screen-height* 
                (+ *font-v-spacing* *font-height*))))

    (set! *char-width*  (+ *font-width*  *font-h-spacing*))
    (set! *char-height* (+ *font-height* *font-v-spacing*))

    (set! *screen* (make-vector *screen-height*))
    (do ((y 0 (+ y 1)))
        ((= y *screen-height*) #t)
      (vector-set! *screen* y 
                   (make-u16vector *screen-width* 
                                   (char->integer #\space))))

    (set! *empty-line* 
          (make-u16vector *screen-width* 
                          (char->integer #\space)))

    (set! *ystart* (- *screen-height-pixel* *char-height*))

    (values abdf kbdf)))

;;;
;;; MAKE-RASTER-FONT
;;;
(define (bdf-bitmap->gl-bitmap  w bml)
  ;; FIXME! Is there cleaner way to do this?
  ;; Is it better to have this done in bdf-read?
  (cond ((<= w 8)
         (list->u8vector (reverse bml)))
        ((<= w 16)
         (let lp ((bml bml)
                  (r   '()))
           (if (null? bml)
               (list->u8vector r)
               (lp (cdr bml) 
                   (cons (bit-field (car bml) 8 16)
                         (cons (bit-field (car bml) 0 8) 
                               r))))))
        ((<= w 24)
         (let lp ((bml bml)
                  (r   '()))
           (if (null? bml)
               (list->u8vector r)
               (lp (cdr bml) 
                   (cons (bit-field (car bml) 16 24)
                         (cons (bit-field (car bml) 8 16) 
                               (cons (bit-field (car bml) 0 8)
                                     r)))))))
        (else (error "font too wide"))))

(define (make-raster-font abdf kbdf)

  (define (setfont char)
    (let* ((c   (bdf-char-encoding char))
           (dw  (bdf-char-dwidth char))
           (bbx (bdf-char-bbx char))
           (bmv (bdf-bitmap->gl-bitmap (list-ref bbx 0)
                                       (bdf-char-bitmap char))))
      (gl-new-list (+ *font-offset* c) GL_COMPILE)
      (gl-bitmap (list-ref bbx 0) (list-ref bbx 1)
                 (- (list-ref bbx 2)) 
                 (- (list-ref bbx 3))
                 (if (> c 255)
                     (/ (+ *font-h-spacing* (list-ref dw 0)) 2)
                     (+ *font-h-spacing* (list-ref dw 0)))
                 (- (list-ref dw 1)) 
                 bmv)
      (gl-end-list)))
    
  (gl-shade-model GL_FLAT)
  (let ((offset (gl-gen-lists (if kbdf 65536 256))))
    (set! *font-offset* offset)
    (gl-pixel-store GL_UNPACK_ALIGNMENT 1)
    (for-each-char setfont abdf)
    (if kbdf (for-each-char setfont kbdf))))

;;;
;;;
;;;
(define (init-screen abdf kbdf)
  (let ((p (open-screen-port)))
    (set! *screen-port* p)
    (move-to p 0 2)
    (format p "あぶらかだぶら ABDC あXい")
    (move-to p 70 3)
    (format p "あぶらかだぶら ABDC あXい")
    (move-to p 71 4)
    (format p "あぶらかだぶら ABDC あXい")
    (move-to p 0 0))

  (make-raster-font abdf kbdf)
  )

;;;
;;; SCREEN-OUTPUT-PORT
;;;
(define-class <screen-output-port> (<virtual-output-port>)
  ((x :init-value 0)
   (y :init-value 0)))

(define (open-screen-port)
  (let ((port (make <screen-output-port>)))

    (define (wrap-point k)
      (if (>= (slot-ref port 'x) (- *screen-width* k))
          (begin
            (slot-set! port 'y (+ (slot-ref port 'y) 1))
            (slot-set! port 'x 0)))
      (if (>= (slot-ref port 'y) *screen-height*)
          (slot-set! port 'y 0)))
    
    (define (putc ch)
      (putb (char->integer ch)))

    (define (putb c)
      (if (> c 255)
          (begin
            (wrap-point 1)
            (let ((x (slot-ref port 'x))
                  (y (slot-ref port 'y)))
              (u16vector-set! (vector-ref *screen* y) x 
                              (logand c #x7f7f))  ; EUC to font's encoding
              (u16vector-set! (vector-ref *screen* y) (+ x 1)
                              #x20)               ; ASCII space
              (slot-set! port 'x (+ x 2))))
          (begin
            (wrap-point 0)
            (let ((x (slot-ref port 'x))
                  (y (slot-ref port 'y)))
              (u16vector-set! (vector-ref *screen* y)  x c)
              (slot-set! port 'x (+ x 1))))))

    (slot-set! port 'putc putc)
    (slot-set! port 'putb putb)
    port))

(define-method move-to ((port <screen-output-port>) x y)
  (slot-set! port 'x x)
  (slot-set! port 'y y))

(define-method clear ((port <screen-output-port>))
  (do ((y 0 (+ y 1)))
      ((= y *screen-height*) #t)
    (u16vector-copy! (vector-ref *screen* y) 
                     0
                     *empty-line*))
  (move-to port 0 0))

;;;
;;; CALL BACK
;;;
(define (display-screen)

  (define (raster-move-to x y)
    (gl-raster-pos (+ *ssv* (* *char-width* x))
                   (- *ystart* (* y *char-height*))))

  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-color '#f32(1.0 1.0 1.0))
  (gl-list-base *font-offset*)
  (gl-push-attrib GL_LIST_BIT)
  (do ((y 0 (+ y 1)))
      ((= y *screen-height*) #t)
    (raster-move-to 0 y)
    (gl-call-lists (vector-ref *screen* y))
    )
  (gl-pop-attrib)
  (gl-flush))

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-ortho 0.0 w 0.0 h -1.0 1.0)
  (gl-matrix-mode GL_MODELVIEW)
  )

(define (keyboard key x y)
  (cond ((= key 27) (exit 0))    ; ESC
        ((= key 12)              ; ^L
         (clear *screen-port*)
         (glut-post-redisplay))
        (else
         (display (integer->char  key) *screen-port*)
         (glut-post-redisplay))))

;;;
;;;
;;;
(define (main args)
  (receive (abdf kbdf) (init-fonts)
    (glut-init args)
    (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
    (glut-init-window-size *screen-width-pixel* *screen-height-pixel*)
    (glut-init-window-position 100 100)
    (glut-create-window (car args))
    (init-screen abdf kbdf)
    (glut-reshape-func reshape)
    (glut-keyboard-func keyboard)
    (glut-display-func display-screen)
    (glut-main-loop)
    0))

;;; EOF
