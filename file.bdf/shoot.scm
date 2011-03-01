;;;
;;; SHOOT!
;;;
(use gl)
(use gl.glut)
(use gauche.time)
(use gauche.uvector)
(use gauche.vport)
(use ggc.file.bdf)

;;;
;;;
;;;
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
(define *screen-port* #f)

(define *ascii-bdf-file* "bdf/a14.bdf")
(define *euc-bdf-file*   "bdf/k14.bdf")
;;(define *ascii-bdf-file* "bdf/12x24.bdf")
;;(define *euc-bdf-file* "bdf/jiskan24.bdf")

(define (init-fonts)
  (let* ((abdf (with-input-from-file *ascii-bdf-file* read-bdf))
         ;;(kbdf (with-input-from-file *euc-bdf-file* read-bdf))
         (kbdf #f)
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

    (set! *ystart* (- *screen-height-pixel* *char-height*))

    (set! *screen-port* (open-screen-port))

    (values abdf kbdf)))

;;;
;;; MAKE-RASTER-FONT
;;;
(define (bdf-bitmap->gl-bitmap w bml)
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
    (u16vector-fill! (vector-ref *screen* y) 
                     (char->integer #\space)))
  (move-to port 0 0))

;;;
;;; CALLBACKS
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
  (case key
    ((27)       (exit 0))      ; ESC
    ((83 115)   (game-start))  ; S s
    ))

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
    (make-raster-font abdf kbdf)

    (print-title)

    (glut-idle-func    #f)
    (glut-reshape-func reshape)
    (glut-keyboard-func keyboard)
    (glut-display-func display-screen)
    (glut-main-loop)
    0))

;;;
;;;  SHOOTING GAME
;;;
(define *tick* 0)
(define *ship* #f)
(define *bullets* #f)
(define *enemies* #f)
(define *battle-field*  #f)
(define *battle-field-x* 50)
(define *battle-field-y* 25)

(define *score* 0)
(define *high-score* 1500)

;;;
;;; BATTLE FIELD ID's
;;;
;;;                   5432109876543210
(define SHIP-MASK   #b0000000000000001)
(define BULLET-MASK #b0000000000000110)
(define ENEMY-MASK  #b0000000011111000)
(define BOMB-MASK   #b0111111100000000)
(define UFO-MASK    #b1000000000000000)
(define TRANSPARENT-MASK 0)
(define BULLET-SHIFT 1)
(define ENEMY-SHIFT  3)
(define BOMB-SHIFT   8)

;;;
;;;
;;;
(define (init-game)
  (set! *tick* 0)
  (set! *battle-field* (make-vector *battle-field-y*))
  (do ((y 0 (+ y 1)))
      ((= y *battle-field-y*) #t)
    (vector-set! *battle-field* y
                 (make-u16vector *battle-field-x* 0)))
  (set! *ship* (make-ship))
  (set! *bullets* (map make-bullet '(1 2 3 4)))
  (set! *enemies* (map make-enemy  '(6 5 4 3 2 1)))
  (set! *score* 0)
  )

(define (print-title)
  (move-to *screen-port* 8 6)
  (display "       S  H  O  O  T  !     " *screen-port*)
  (move-to *screen-port* 8 10)
  (display "    [4] <== pAq ==> [6]     " *screen-port*)
  (move-to *screen-port* 8 14)
  (display "      [SPACE]  : FIRE!      " *screen-port*)
  (move-to *screen-port* 8 18)
  (display "   PRESS [S] KEY TO START   " *screen-port*)
)

(define (print-score)
  (move-to *screen-port* 0 0)
  (format *screen-port* "SCORE:~10,'0d  HI-SCORE:~10,'0d   pAq:~d"
          *score* *high-score* (*ship* 'nships)))

(define (game-start)
  (init-game)
  (glut-idle-func    update-screen)
  (glut-keyboard-func game-keyboard))

(define (game-over)
  (print-score)
  (move-to *screen-port* 8 8)
  (display "                                " *screen-port*)
  (move-to *screen-port* 8 9)
  (display "  G  A  M   E      O  V  E  R!! " *screen-port*)
  (move-to *screen-port* 8 10)
  (display "                                " *screen-port*)
  (move-to *screen-port* 8 11)
  (display "                                " *screen-port*)
  (move-to *screen-port* 8 12)
  (display "   PRESS [S] KEY TO RESTART     " *screen-port*)
  (move-to *screen-port* 8 13)
  (display "                                " *screen-port*)
  (glut-idle-func    #f)
  (glut-keyboard-func keyboard))

(define (update-screen)
  (let ((tc (make <real-time-counter>)))
    (with-time-counter tc
       (clear *screen-port*)
       (clear-battle-field)
       (inc! *tick*)

       (print-score)
       (*ship* 'disp)
       (for-each (lambda (bullet)
                   (bullet 'disp))
                 *bullets*)
       (for-each (lambda (enemy)
                   (enemy 'disp))
                 *enemies*)

       (*ship* 'act)
       (for-each (lambda (bullet)
                   (bullet 'act))
                 *bullets*)
       (for-each (lambda (enemy)
                   (enemy 'act))
                 *enemies*))

    (sys-nanosleep (- 7e7 (* 1e9 (time-counter-value tc))))
    (glut-post-redisplay)
    ))

(define (game-keyboard key x y)
  (cond ((= key 27)                    ; ESC 
         (exit 0)) 
        ((= key 52) (ship-move-left))  ; 4
        ;;((= key 53) ((car *enemies*) 'die))
        ((= key 54) (ship-move-right)) ; 6
        ((= key 32) (ship-fire))       ; SPC
        ))
;;;
;;;
;;;
(define (clear-battle-field)
  (do ((y 0 (+ y 1)))
      ((= y *battle-field-y*) #t)
    (u16vector-fill! (vector-ref *battle-field* y) 0)))

(define (battle-field-ref x y)
  (u16vector-ref  (vector-ref *battle-field* y) x))

(define (battle-field-set! x y v)
  (u16vector-set! (vector-ref *battle-field* y) x v))

(define (battle-field-mask! x y mask)
  (battle-field-set! x y (logior mask (battle-field-ref x y))))

(define (place1 x y w mask str)
  (move-to *screen-port* x y)
  (display str *screen-port*)
  (do ((i 0 (+ i 1))
       (x x (+ x 1)))
      ((>= i w) #t)
    (if (not (eq? (string-ref str i) #\space))
        (battle-field-mask! x y mask))))

(define (place x y w h mask vec)
  (do ((i 0 (+ i 1))
       (y y (+ y 1)))
      ((= i h) #t)
    (place1 x y w mask (vector-ref vec i))))

;;;
;;;  SHIP
;;;
(define (make-ship)
  (let ((state 'alive)
        (stick  *tick*)
        (pos-x (- (quotient *battle-field-x* 2) 1))
        (pos-y (- *battle-field-y* 2))
        (max-x (- *battle-field-x* 5))
        (min-x 2)
        (nships 3)
        (ship-alive  "pAq")
        (ship-burn (vector "XXX" "   " "   " "XXX" "\\\\\\"
                           "---" "///" "|||" "\\\\\\" "---")))
    (define (disp)
      (let ((phy1 (ash (- *tick* stick) -1)))
        (case state 
          ((alive)
           (place1 pos-x pos-y 3 SHIP-MASK ship-alive))
          ((burning)
           (place1 pos-x pos-y 3 TRANSPARENT-MASK
                   (vector-ref ship-burn
                               (modulo phy1 10))))
          ((resting)
           (place1 pos-x pos-y 3 TRANSPARENT-MASK "   ")))))
      
    (define (fire)
      (if (eq? state 'alive)
          (let ((b (get-bullet)))
            (if b ((b 'fire) (+ pos-x 1) (- pos-y 1))))
          #f))

    (define (hit?)
      (define (check x y)
        (let ((o (battle-field-ref x y)))
          (if (= (logand o (logior ENEMY-MASK BOMB-MASK)) 0)
              #f
              #t)))
      (or (check pos-x pos-y)
          (check (+ pos-x 1) pos-y)
          (check (+ pos-x 2) pos-y)))

    (define (act)
      (let ((phy0 (- *tick* stick)))
        (case state 
          ((alive) 
           (if (hit?) (die)))
          ((burning) (if (>= phy0 27) (rest)))
          ((resting) (if (>= phy0 20) (revive))))))

    (define (move-right)
      (if (eq? state 'alive)
          (begin
            (inc! pos-x)
            (if (>= pos-x max-x)
                (set! pos-x max-x)))))

    (define (move-left)
      (if (eq? state 'alive)
          (begin
            (dec! pos-x)
            (if (<= pos-x min-x)
                (set! pos-x min-x)))))

    (define (die)
      (set! state 'burning)
      (set! stick *tick*))
    
    (define (rest)
      (set! state 'resting)
      (set! stick *tick*))

    (define (revive)
      (dec! nships)
      (if (> nships 0)
          (begin
            (set! state 'alive)
            (set! stick *tick*))
          (game-over)))
    
    (lambda (m)
      (case m
        ((disp)       (disp))
        ((act)        (act))
        ((move-left)  (move-left))
        ((move-right) (move-right))
        ((fire)       (fire))
        ((revive)     (revive))
        ((die)        (die))
        ((pos-x)      pos-x)
        ((nships)     nships)
        ((state)      state)))))

(define (ship-move-left)   (*ship* 'move-left))
(define (ship-move-right)  (*ship* 'move-right))
(define (ship-fire)        (*ship* 'fire))

;;;
;;; BULLET
;;;
(define (make-bullet n)
  (let ((state 'nop)
        (mask (ash n BULLET-SHIFT))
        (stick 0)
        (pos-x 0)
        (pos-y 0)
        (min-y 1))

    (define (disp)
      (case state
        ((nop) #t)
        ((exploding) (place1 pos-x pos-y 1 mask "*"))
        ((flying)    (place1 pos-x pos-y 1 mask "!"))))

    (define (hit?)
      (define (check x y)
        (let ((o (battle-field-ref x y)))
          (if (= (logand o (logior ENEMY-MASK BOMB-MASK UFO-MASK)) 0)
              #f
              #t)))
      (check pos-x pos-y))

    (define (act)
      (case state
        ((nop) #t)
        ((exploding)
         (if (>= (- *tick* stick) 1) (nop)))
        ((flying)
         (if (hit?) 
             (explode)
             (dec! pos-y))
         (if (<= pos-y min-y)
             (explode)))))

    (define (fire x y)
      (set! stick *tick*)
      (set! pos-x x)
      (set! pos-y y)
      (set! state 'flying))

    (define (explode)
      (set! stick *tick*)
      (set! state 'exploding))

    (define (nop)
      (set! stick *tick*)
      (set! state 'nop))
    
    (lambda (m)
      (case m
        ((disp)  (disp))
        ((act)   (act))
        ((fire)  fire)
        ((explode) (explode))
        ((pos-x) pos-x)
        ((pos-y) pos-y)
        ((state) state)))))

(define (get-bullet)
  (let lp ((b *bullets*))
    (if (null? b) 
        #f
        (if (eq? ((car b) 'state) 
                 'nop)
            (car b)
            (lp (cdr b))))))

;;;
;;;   ENEMY
;;;
;;;         (die)       (fall)          (rest)          
;;;   alive ----> dying ------> falling ------> resting --+
;;;     ^-------------------------------------------------+
;;;                     (revive)

(define *enemy-alive*   (vector ">O<" "XoX" "<O>" "XoX"))
(define *enemy-dying*   (vector "=O=" "=o=" "=+="))
(define *enemy-falling* (vector "/\\/" "\\/\\"))

(define (make-enemy n)
  (let ((state 'alive)
        (mask (ash n ENEMY-SHIFT))
        (stick 0)
        (speed 6)
        (nrevive 0)
        (dir   'left)
        (pos-x (- *battle-field-x* 4))
        (pos-y (+ 3 (* n 2)))
        (min-x 1)
        (max-x  (- *battle-field-x* 4))
        (max-y  (- *battle-field-y* 1)))
    
    (define (die)
      (case state
        ((alive)
         (inc! *score* (- 33 pos-y speed))
         (if (> *score* *high-score*)
             (set! *high-score* *score*))
         (set! stick *tick*)
         (set! state 'dying))))

    (define (revive)
      (case state
        ((resting)
         (inc! nrevive)
         (if (> speed 1)
             (dec! speed))
         (set! stick *tick*) 
         (set! dir 'left)
         (set! pos-x (- *battle-field-x* 4))
         (set! pos-y (+ 5 (* n 2)))
         (set! state 'alive))))

    (define (fall)
      (set! stick *tick*)
      (set! state 'falling))

    (define (rest)
      (set! stick *tick*)
      (set! state 'resting))

    (define (move-down)
      (inc! pos-y)
      (if (>= pos-y max-y)
          (rest)))

    (define (move-left)
      (dec! pos-x)
      (if (<= pos-x min-x)
          (begin 
            (inc! pos-x)
            (move-down)
            (set! dir 'right))))

    (define (move-right)
      (inc! pos-x)
      (if (>= pos-x max-x)
          (begin
            (dec! pos-x)
            (move-down)
            (set! dir 'left))))

    (define (move-alive)
      (case dir
        ((left)  (move-left))
        ((right) (move-right))))

    (define (disp)
      (let ((phy0 (- *tick* stick))
            (phy1 (ash (- *tick* stick) -1))
            (phy2 (ash (- *tick* stick) -2)))

        (case state
          ((alive)
           (place1 pos-x pos-y 3 mask 
                   (vector-ref *enemy-alive*
                               (modulo phy2 4))))
          ((falling)
           (place1 pos-x pos-y 3 mask 
                   (vector-ref *enemy-falling*
                               (modulo phy0 2))))
          ((dying)
           (place1 pos-x pos-y 3 mask 
                   (vector-ref *enemy-dying*
                               (modulo phy1 3))))
          ((resting) #t))))

    (define (hit?)
      (define (check x y)
        (let ((o (battle-field-ref x y)))
          (if (= (logand o (logior SHIP-MASK BULLET-MASK)) 0)
              #f
              #t)))
      (or (check pos-x pos-y)
          (check (+ pos-x 1) pos-y)
          (check (+ pos-x 2) pos-y)))

    (define (hit?)
      (define (check x y)
        (let ((o (battle-field-ref x y)))
          (if (= (logand o (logior SHIP-MASK BULLET-MASK)) 0)
              #f
              #t)))
      (or (check pos-x pos-y)
          (check (+ pos-x 1) pos-y)
          (check (+ pos-x 2) pos-y)))

    (define (moge)
      (define (check x y)
        (let ((o (battle-field-ref x y)))
          (if (= (logand o ENEMY-MASK) mask)
              #f
              #t)))
      (case dir
        ((left)
         (if (check pos-x pos-y)
             (set! dir 'right)))
        ((right)
         (if (check (+ pos-x 2) pos-y)
             (set! dir 'left)))))

    (define (act)
      (let ((phy0 (- *tick* stick)))
        (case state
          ((alive)
           (if (hit?) (die))
           (moge)
           (if (= (modulo phy0 speed) 0)
               (move-alive)))
          ((falling)  (move-down))
          ((dying)    (if (>= phy0 5)  (fall)))
          ((resting)  (if (>= phy0 50) (revive))))))
    
    (lambda (m)
      (case m
        ((disp) (disp))
        ((act)  (act))
        ((die)  (die))
        ((state) state)))))

;;; EOF
