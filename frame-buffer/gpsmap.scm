#!/usr/local/bin/gosh
;;; $Id: gpsmap.scm,v 1.9 2002/08/29 02:02:59 skimu Exp $
;;;
;;;  8/28/2002 - I noiced that  mapblast no longer offer gif 
;;;              only accsess. 
;;;              So this program does not work anymore.
;;;
;;;
;;; Get map from www.mapblast.com and draw trace and
;;; save as PNG file.
;;;
;;; Usage: gpsmap.scm tracedmap.png < trail-ridge-road.dat
;;;
(use srfi-1)      ; list library
(use srfi-13)     ; string library
(use ggc.skimu.frame-buffer) ; drawing

(define pi (acos -1))

;;; GPS track data structure
(define track-data ;; sample data structure
  '(
;;   lat           lon
   ( 32.936797 . -96.806245 )
   ( 32.936797 . -96.806245 )
   ( 32.936797 . -96.806245 )
   ))

(define (trk-get-la trk)  (car (car trk)))
(define (trk-get-lo trk)  (cdr (car trk)))
(define (trk-next trk)    (cdr trk))
(define (trk-more? trk)   (pair? trk))
(define (trk-end? trk)    (null? trk))
;;;;

(define *v* #f)

(define (msg  . args)
  (if *v*
      (begin
	(apply format (cons (current-error-port)  args))
	(flush (current-error-port)))))

(define (get-corner-from-track trk)
  (let lp ((trk    trk)
	   (max-la -90)
	   (max-lo -180)
	   (min-la  90)
           (min-lo  180))
    (if (trk-end? trk)
	(values max-la max-lo min-la min-lo)
	(let ((la (trk-get-la trk))
	      (lo (trk-get-lo trk)))
	  (if (> la max-la) (set! max-la la))
	  (if (< la min-la) (set! min-la la))
	  (if (> lo max-lo) (set! max-lo lo))
	  (if (< lo min-lo) (set! min-lo lo))
	  (lp (trk-next trk) max-la max-lo min-la min-lo)))))

(define (center-of-two-points x1 y1 x2 y2)
  (values (/ (+ x1 x2) 2)
	  (/ (+ y1 y2) 2)))

(define (metric-distance la1 lo1 la2 lo2)
;; in arbitrary unit of length
  (receive (la0 lo0) (center-of-two-points la1 lo1 la2 lo2)
     (values 
      (* (abs (- lo1 lo2)) (cos (* pi (/ la0 180))))
      (abs (- la1 la2)))))

(define (gen-lalo-to-xy la0 lo0 pixel/la width height)
  (lambda (la lo)
    (values (inexact->exact
	     (+ (* (- lo lo0) pixel/la (cos (* pi (/ la 180))))
		(/ width 2)))
	    (inexact->exact 
	     (+ (* (- la0 la) pixel/la) 
		(/ height 2))))))

(define default-pixel 512)
(define (image-size max-la max-lo min-la min-lo)
    (let ((pixel/la 0)
	  (height   default-pixel)
	  (width    default-pixel))
      (receive (x y) (metric-distance max-la max-lo min-la min-lo)
         (set! x (* x 1.2))
	 (set! y (* y 1.2))
	 (if (> x y)
	     (begin 
	       (set! pixel/la (/ width x))
	       (set! height   (inexact->exact (* y pixel/la))))
	     (begin
	       (set! pixel/la (/ height y))
	       (set! width    (inexact->exact (* x pixel/la))))))
      (values width height pixel/la)))

(define (draw-trace! mapimg trk trans)
  (let lp ((trk trk))
    (if (trk-more? trk)
	(receive (x y) (trans (trk-get-la trk) (trk-get-lo trk))
          (fb-draw-box! mapimg (- x 2) (- y 2) (+ x 2) (+ y 2) 0 0 255)
	  (fb-draw-box! mapimg (- x 3) (- y 3) (+ x 3) (+ y 3) 0 0 255)
	  (lp (trk-next trk))))))

(define tmp-file "/tmp/map.gif")
(define (get-map-from-mapblast la lo pixel/la width height)
  (let ((url
	 (format #f 
  "http://www.mapblast.com/gif?&CT=~d:~d:~d&IC=0:0:0&W=~d&H=~d&FAM=myblast&LB="
		 la lo (/ 320000000  pixel/la)  width height)
	 ))
    (dynamic-wind
	(lambda () #f)
	(lambda () 
	  (msg "getting map...")
	  (if (= 0 (sys-system (format #f 
  "ftp -V -o ~a '~a' 2> /dev/null"     tmp-file url)))
	      (begin
		(msg "done ~%")
		(msg "reading mapfile...")
		(load-frame-buffer-from-gif-file tmp-file))
	      (error "ftp failed")))
	(lambda () 
	  (and  (file-exists? tmp-file)
		(sys-remove tmp-file)
		(msg "done~%"))))))

(define (get-map-of-trace trk)
  (receive (max-la max-lo min-la min-lo) (get-corner-from-track trk)
     (receive (la0 lo0) (center-of-two-points max-la max-lo min-la min-lo)
        (receive (width height pixel/la) (image-size max-la max-lo min-la min-lo)
	   (let ((m          (get-map-from-mapblast la0 lo0 pixel/la width height))
		 (lalo-to-xy (gen-lalo-to-xy        la0 lo0 pixel/la width height)))
	     (if m (draw-trace! m trk lalo-to-xy))
	     m)))))

(define (read-data)
  (let ((l '()))
    (do ((s (read-line) (read-line))
	 (trk '()))
	((not (string? s)) trk)
      (set! l (remove! string-null? (string-split s #\space)))
      (if (>= (length l) 2)
	  (set! trk (cons (cons (string->number (list-ref l 0))
				(string->number (list-ref l 1)))
			  trk))))))

(define (main args)
  (with-error-handler 
   (lambda (e)
     (format (current-error-port) "Error: ~a~%" 
	     (slot-ref e 'message))
     (exit 2))
   (lambda ()
     (if (null? (cdr args))
	 (errorf "To few argument.~%Usage: gpsmap foo.png < data.dat~\n")
	 (let ((mapfile (cadr args)))
	   (let ((trk (read-data)))
	     (let ((traced-map  (get-map-of-trace trk)))
	       (if (not traced-map) (error "Could not get map."))
	       (msg "writing map into ~a..." mapfile)
	       (save-frame-buffer-as-png-file traced-map mapfile)
	       (msg "done~%")))))
     0)))

;;; EOF