#!/bin/sh
#| -*- mode: scheme; coding: utf-8; -*- |#
:; exec gosh -I. -- $0 "$@"

;; dns proxy hack to test some issues with slow dns requests
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

(define (verbose-socket-sendto socket msg to-address)
  (socket-sendto socket msg to-address)
  (print "forwarded " (size-of msg) " byte(s) to " (sockaddr->string to-address)))

(define (proxy selector socket sender message dest delay)
  (print)
  (print "got " (size-of message) " byte(s) from " (sockaddr->string sender))
  (let ((tmpsock (make-socket PF_INET SOCK_DGRAM)))
    (selector-add! selector
                   (socket-fd tmpsock)
                   (lambda(input flag)
                     (let1 buf (make-u8vector (ash 1 16))
                       (receive (got from)
                           (socket-recvfrom! tmpsock buf (list dest))
                         (print "received " got " byte(s) from " (sockaddr->string from))
                         (let1 message (u8vector-copy buf 0 got)
			   ;; todo: use something non-blocking here and schedule packet for
			   ;; delayed delivery
			   (sys-sleep delay)
                           (verbose-socket-sendto socket message sender)
                           (selector-delete! selector (socket-fd tmpsock) #f #f)
                           (socket-close tmpsock)))))
                   '(r))
    (verbose-socket-sendto tmpsock message dest)))

(define (main args)
  (let-optionals* args ((prg "server")
                        (addr #f)
                        (port 53)
                        (dest "192.168.178.1")
                        (dest-port "domain")
			(delay "3"))
    (udp-server addr (x->number port)
                (cute proxy <> <> <> <>
                      (ref (car (sys-getaddrinfo dest dest-port
                                                 (make <sys-addrinfo> :sock-type SOCK_DGRAM)))
                           'addr)
		      (x->number delay))))
  0)
