(define-module wiimote
  (use c-wrapper)
  (use gauche.collection)
  (use gauche.process)
  (use util.list)
  (export wiimote-open
	  wiimote-get-state
	  wiimote-close))

(select-module wiimote)

(define (package-version p)
  (process-output->string `(pkg-config --modversion ,p)))

(c-load (string-append (string-join (list "cwiid"
                                          (package-version "cwiid")
                                          "bluez"
                                          (package-version "bluez")
                                          "subset")
                                    "_")
                       ".h")
        :cflags "-I."
        :libs-cmd "sh -c 'echo -L/usr/lib/$(dpkg-architecture -qDEB_HOST_MULTIARCH) $(pkg-config --libs cwiid)'")

(define BDADDR_ANY (strtoba "00:00:00:00:00:00"))

(define (wiimote-open)
  (let1 r `((wii   . ,(cwiid_open BDADDR_ANY 0))
	    (state . ,(make <c-struct:cwiid_state>))
	    ;;(cal   . ,(make <c-struct:balance_cal>))
            )
	(when (null-ptr? (assoc-ref r 'wii))
	      (error "open failed")) ;; todo: improve error handling
	#?=(cwiid_set_rpt_mode (assoc-ref r 'wii)
                               (logior CWIID_RPT_STATUS
                                       CWIID_RPT_BTN
                                       CWIID_RPT_ACC))
        ;; todo
        (while (zero? (ref (wiimote-get-state r) 'ext_type))
          #?=(cwiid_set_rpt_mode (assoc-ref r 'wii)
                                 (logior CWIID_RPT_STATUS
                                         CWIID_RPT_BTN
                                         CWIID_RPT_ACC
                                         CWIID_RPT_IR
                                         CWIID_RPT_MOTIONPLUS))
          (cwiid_enable (assoc-ref r 'wii) CWIID_FLAG_MOTIONPLUS)
          (sys-nanosleep 0.5e9))
        r))

(define (wiimote-get-state handle)
  (when (not (zero? (cwiid_get_state (assoc-ref handle 'wii)
				     (ptr (assoc-ref handle 'state)))))
    (error "failed to get state"))
  (assoc-ref handle 'state))

(define (wiimote-close handle)
  (cwiid_close (assoc-ref handle 'wii)))
