#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"

;; dns proxy hack to test some issues with getaddrinfo
;; notes:
;; - only for testing!

(use gauche.net)
(use gauche.selector)
(use gauche.uvector)
(use gauche.sequence)
(use gauche.process)
(use sxml.ssax)
(use sxml.sxpath)
(use sxml.tools)
(use util.list)

(define *delayed-packet* #f)

(define (sockaddr->string addr)
  (string-append
   (inet-address->string (sockaddr-addr addr)
                         (if (equal? (sockaddr-family addr) 'inet6)
                           PF_INET6 
                           PF_INET))
   ":"
   (x->string (sockaddr-port addr))))

(define (udp-server addr port message-handler)
  (let ((selector (make <selector>))
	(saddrs (make-sockaddrs addr port 'udp))
        (buf (make-u8vector (ash 1 16))))
    (for-each (lambda(saddr)
		(sockaddr-family saddr)
		(let1 socket (make-socket (if (equal? (sockaddr-family saddr) 'inet6)
                                            PF_INET6
                                            PF_INET)
                                          SOCK_DGRAM)
                  (print "udp server started on " (sockaddr->string saddr))
                  (socket-setsockopt socket SOL_SOCKET SO_REUSEADDR 1)
                  (selector-add! selector
                                 (socket-fd (socket-bind socket saddr))
                                 (lambda (input flag)
                                   (receive (got sender) (socket-recvfrom! socket buf #t)
                                     (message-handler selector socket sender (u8vector-copy buf 0 got))))
                                 '(r))))
	      saddrs)
    (do () (#f) (selector-select selector))))

(define (echo socket sender message)
  (print "got " (size-of message) " byte(s) from " (sockaddr->string sender))
  (socket-sendto socket message sender))

(define (hexdump v)
  (for-each (cut format #t "~2,'0x " <>) v)
  (print))

;; todo: crap (unfortunately text2pcap buffers input)
(define (fake-pcap x)
  (call-with-process-io '(text2pcap -q -u "53,53" - -)
                        (lambda (in out)
                          (with-output-to-port out
                            (lambda()
                              (display "0000 ")
                              (hexdump x)
                              (print)))
                          (close-output-port out)
                          (let* ((buf (make-u8vector (ash 1 16)))
                                 (got (read-block! buf in)))
                            (u8vector-copy buf 0 got)))))

(define (pcap->pdml x)
  (call-with-process-io '(tshark -T pdml "-i" -)
                        (lambda(in out)
                          (with-output-to-port out
                            (lambda()
                              (write-block x)
                              (flush)))
                          (close-output-port out)
                          (ssax:xml->sxml in '()))
                        :error "/dev/null"
                        ))

(define (pcap->psml x)
  (call-with-process-io '(tshark -T psml "-i" -)
                        (lambda(in out)
                          (with-output-to-port out
                            (lambda()
                              (write-block x)
                              (flush)))
                          (close-output-port out)
                          (ssax:xml->sxml in '()))
                        :error "/dev/null"
                        ))

(define (pcap->text x)
  (call-with-process-io '(tshark -T text "-i" -)
                        (lambda(in out)
                          (with-output-to-port out
                            (lambda()
                              (write-block x)
                              (flush)))
                          (close-output-port out)
                          (port->string in))
                        :error "/dev/null"
                        ))

(define (dissect x)
  (map
   (lambda(x)
     (list (sxml:attr x 'name) ": "
           (string-join (map (lambda(l)
                               (string-append (x->string (car l))
                                              "='"
                                              (cadr l)
                                              "'"))
                             (filter (lambda(x) (not (equal? (car x) 'name)))
                                     (sxml:attr-list x)))
                        " ")))
   (filter (lambda(x)
             (#/^dns/ (x->string (sxml:attr x 'name))))
           ((sxpath '(// field)) (pcap->pdml (fake-pcap x))))))

(define (dissect-2 x)
  (filter (lambda(x)
            ;; todo: there is no field for ipv6 addr response :(
            (member (car x) '("dns.id" "dns.qry.name" "dns.qry.type" "dns.count.answers" "dns.resp.addr")))
          (map
           (lambda(x)
             (cons (sxml:attr x 'name) (sxml:attr x 'show)))
           (filter (lambda(x)
                     (#/^dns/ (x->string (sxml:attr x 'name))))
                   ((sxpath '(// field)) (pcap->pdml (fake-pcap x)))))))

(define (short-text-summary x)
  (display (subseq (pcap->text (fake-pcap x)) 40)))

;; todo: simpler implementation!
(define (dns-a-class-query? x)
  (equal? (assoc-ref (dissect-2 x) "dns.qry.type" #f)
          "0x0001"))

;; drop every second A class query request
(define drop-request?
  (let1 i 0
    (lambda(x)
      (cond [(dns-a-class-query? x)
             (set! i (modulo (+ i 1) 2))
             (not (zero? i))]
            [else
             #f]))))

(define (verbose-socket-sendto socket msg to-address)
  (socket-sendto socket msg to-address)
  (print "forwarded " (size-of msg) " byte(s) to " (sockaddr->string to-address)))

(define (proxy selector socket sender message dest)
  (print)
  (print "got " (size-of message) " byte(s) from " (sockaddr->string sender))
  ;; (short-text-summary message)
  ;; (print (dissect-2 message))
  ;; todo: support ipv6
  (if (and #t ;; note: bug needs drop!
           (drop-request? message))
    (print "dropped request")
    (let ((tmpsock (make-socket PF_INET SOCK_DGRAM)))
      (verbose-socket-sendto tmpsock message dest)
      (selector-add! selector
                     (socket-fd tmpsock)
                     (lambda(input flag)
                       (let1 buf (make-u8vector (ash 1 16))
                         (receive (got from)
                             (socket-recvfrom! tmpsock buf (list dest))
                           (print "received " got " byte(s) from " (sockaddr->string from))
                           (let1 message (u8vector-copy buf 0 got)
                             ;; (short-text-summary message)
                             ;; (print (dissect message))
                             (cond [(and (not *delayed-packet*)
                                         (dns-a-class-query? message))
                                    (set! *delayed-packet* (cute verbose-socket-sendto socket message sender))
                                    ]
                                   [*delayed-packet*
                                    (print "send re-ordered packets")
                                    (verbose-socket-sendto socket message sender)
                                    ;; todo: don't do a blocking wait / schedule it for later
                                    (sys-nanosleep 2e8)
                                    (*delayed-packet*)
                                    (set! *delayed-packet* #f)]
                                   [else
                                    (verbose-socket-sendto socket message sender)])
                             (selector-delete! selector (socket-fd tmpsock) #f #f)
                             (socket-close tmpsock)))))
                     '(r)))))

;;(define *dissector* (run-process '(sh -c "mergecap -w - -|tshark -l -i -") :input :pipe))
;;(define *dissector* (run-process '(sh -c "./text2pcap -u 53,53 - -|hd") :input :pipe))
;;(define *dissector* (run-process '(sh -c "cat") :input :pipe))

(define (main args)
  (let-optionals* args ((prg "server")
                        (addr #f)
                        (port 53)
                        (dest "192.168.178.1")
                        (dest-port "domain"))
    (udp-server addr (x->number port)
                (cute proxy <> <> <> <>
                      (ref (car (sys-getaddrinfo dest dest-port
                                                 (make <sys-addrinfo> :sock-type SOCK_DGRAM)))
                           'addr))))
  0)
