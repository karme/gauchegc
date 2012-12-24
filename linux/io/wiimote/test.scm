#!/usr/bin/gosh -I.
(use wiimote)
(use util.list)
(use gauche.collection)
(use gauche.sequence)
(use gauche.interactive)

(define (main args)
  (let1 b (wiimote-open)
	(unwind-protect
	 (begin
	   (dotimes (i 2000)
		    (sys-nanosleep 0.05e9)
		    (let1 s (wiimote-get-state b)
                      ;;#?=s
                      ;; works
                      #?=(map x->number (ref s 'acc))
                      (let1 ir (filter boolean
                                       (map (lambda(src)
                                              (if (zero? (ref src 'valid))
                                                #f
                                                (map x->number
                                                     (append (coerce-to <list> (ref src 'pos))
                                                             (list (ref src 'size))))))
                                            (ref s 'ir_src)))
                        (when (not (null? ir))
                          #?=ir))
                        
                      ;; (when (= 4 (x->number #?=(ref s 'ext_type)))
                      ;;   #?=(coerce-to <list>
                      ;;                 (ref* s 'ext 'motionplus 'angle_rate)))
                      (sys-nanosleep 0.5e9)
                      ))
	   0)
	 (wiimote-close b))))
