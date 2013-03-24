;;;
;;; gps module
;;; current implementation connects to a gpsd (version >=3)
;;; see also:
;;; http://www.catb.org/gpsd/
;;; especially:
;;; http://www.catb.org/gpsd/gpsd_json.html
;;;
;;;   Copyright (c) 2013 Jens Thiele <karme@karme.de>
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
(define-module gps
  (use rfc.json)
  (use gauche.net)
  (use file.filter)
  (use util.list)
  (export gps-open
          gps-data-handler
          gps-tpv-handler
          gps-position-handler))

(select-module gps)

(define (gps-open . args)
  (let-optionals* args ((socket-args '(inet "localhost" "gpsd")))
    (let1 s (apply make-client-socket socket-args)
      (with-output-to-port (socket-output-port s)
        (lambda()
          (print #`"?WATCH=,(construct-json-string '((\"json\" . true)));")))
      (socket-input-port s))))

(define (gps-data-handler handler)
  (lambda(port-or-fd flag)
    (handler (with-input-from-port port-or-fd
               (lambda()
                 (parse-json-string (read-line)))))))
  
;; tpv = time position velocity
(define (gps-tpv-handler update-tpv)
  (gps-data-handler
   (lambda(o)
     (case (string->symbol (assoc-ref o "class"))
       [(TPV)
        (update-tpv o)]))))

(define (gps-position-handler update-position)
  (gps-tpv-handler (lambda(o)
                     (when (and (number? (assoc-ref o "mode"))
                                (> (assoc-ref o "mode") 2))
                       (apply update-position
                              (map
                               (cut assoc-ref o <>)
                               '("device" "lon" "lat" "alt")))))))
