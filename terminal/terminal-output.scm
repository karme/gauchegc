#| -*- mode: scheme; coding: utf-8; -*- |#

;; terminal output "emulator"
;;
;; todos:
;;
;; - cleanup
;;
;; - lots of stuff missing

;; based on emacs term.el:
;;
;;; term.el --- general command interpreter in a window stuff

;; Copyright (C) 1988, 1990, 1992, 1994, 1995, 2001, 2002, 2003,
;;   2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Per Bothner <per@bothner.com>
;; Maintainer: Dan Nicolaescu <dann@ics.uci.edu>, Per Bothner <per@bothner.com>
;; Based on comint mode written by: Olin Shivers <shivers@cs.cmu.edu>
;; Keywords: processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Marck 13 2001
;;; Fixes for CJK support by Yong Lu <lyongu@yahoo.com>.

;;; Dir/Hostname tracking and ANSI colorization by
;;; Marco Melgazzi <marco@techie.com>.

(define-module terminal-output
  (use gauche.vport)
  (use gauche.collection)
  (use srfi-14)
  (use srfi-1)
  (use buffer)
  (export
   open-output-terminal))

(select-module terminal-output)

;; http://osdir.com/ml/lisp.scheme.gauche/2007-03/msg00006.html
(define-macro (debug-assert e)
  `(if (not ,e)
     (error "Assertion failed: " ,(x->string e))))

(define (clip v minv maxv)
  (debug-assert (not (< maxv minv)))
  (min (max minv v) maxv))

(define (1- x)
  (- x 1))

(define-class <terminal-output-port> (<virtual-output-port>)
  (buffer         ; text buffer
   ;; procedure handling next char represented as integer
   put-integer-current
   ;; terminal registers
   params
   ;; saved cursor position
   saved-cursor
   (insert-mode :init-value #f)))

(define (open-output-terminal buffer)
  (define term-scroll-start 0)
  
  (define (current-row)
    (ref buffer 'y))
  
  (define (current-col)
    (ref buffer 'x))
  
  (define (current-width)
    (ref buffer 'width))
  
  (define (current-height)
    (ref buffer 'height))
  
  (define (reset)
    (buffer-clear buffer))
  
  (define (vertical-move down . check-for-scroll)
    (buffer-move buffer 0 down))

  ;; notes:
  ;; - top left in buffer is 0 0 (not 1 1)!
  ;; - and terminal row position is always relative to the end of the buffer!
  (define (goto row col)
    (let ((row (+ (max (- (size-of (ref buffer 'text)) (current-height)) 0)
                  row)))
      (buffer-move buffer (- col (current-col)) (- row (current-row)))
      (debug-assert (and (= row (current-row)) (= col (current-col))))))
    
  (define (clip-x x) (clip x 0 (1- (current-width))))

  (define (clip-y y) (clip y 0 (1- (current-height))))

  (define (goto-clip row col)
    (goto (clip-y row) (clip-x col)))

  (define (erase-next-rows n)
    (when (> n 0)
      (vertical-move 1)
      (buffer-clear-row buffer)
      (erase-next-rows (- n 1))))

  (define (erase-rows-below)
    (let ((opos (list (current-col) (current-row))))
      (term-move-to-column 0)
      (erase-next-rows (- (size-of (ref buffer 'text)) (+ (current-row) 1)))
      (apply buffer-move (cons buffer
                               (map - opos (list (current-col) (current-row)))))))
  
  (define (erase-in-line kind)
    ;; 8.3.41
    (case kind
      [(0) (buffer-clear-row buffer (current-col) (current-width))]
      [(1) (buffer-clear-row buffer 0 (current-col))]
      [(2) (buffer-clear-row buffer)]
      [else (error #`"invalid kind ,|kind|")]))

  (define (term-erase-in-display kind)
    (cond
     [(= kind 0)
      (buffer-clear-row buffer (current-col) (current-width))
      (erase-rows-below)]
     [else
      ;; i need that one but unfortunately it is not trvial to port it from term.el
      #?=`("todo: not yet implemented" ,kind)]))

  (define (term-move-to-column col)
    (buffer-move-x buffer (- (clip-x col) (current-col))))

  ;; Move DELTA column right (or left if delta < 0 limiting at column 0).
  (define (term-move-columns delta)
    (term-move-to-column (+ (current-col) delta)))

  (define (term-delete-chars count)
    #?="term-delete-chars: todo")

  ;; todo: see also Source: ECMA-48 5th Ed. 8.3.118
  (define (term-handle-colors-array p)
    )

  (define (term-set-scroll-region top bottom)
    ;;   "Set scrolling region.
    ;; TOP is the top-most line (inclusive) of the new scrolling region,
    ;; while BOTTOM is the line following the new scrolling region (e.g. exclusive).
    ;; The top-most line is line 0."
    ;; todo:
    #|
    (setq term-scroll-start
    (if (or (< top 0) (>= top term-height))
    0
    top))
    (setq term-scroll-end
    (if (or (<= bottom term-scroll-start) (> bottom term-height))
    term-height
    bottom))
    (setq term-scroll-with-delete
    (or (term-using-alternate-sub-buffer)
    (not (and (= term-scroll-start 0)
    (= term-scroll-end term-height)))))
    (term-move-columns (- (current-col)))
    |#
    #?="term-set-scroll-region: todo"
    (goto 0 0))

  ;; Insert COUNT spaces after point, but do not change any of
  ;; following screen lines.  Hence we may have to delete characters
  ;; at the end of this screen line to make room.
  (define (term-insert-spaces count)
    #?="term-insert-spaces: todo")

  (let ((port (make <terminal-output-port>)))
    (define (term-handle-ansi-escape char)
      (cond
       [(or (= char (char->integer #\H))  ;; cursor motion (terminfo: cup,home)
            ;; (eq char ?f) ;; xterm seems to handle this sequence too, not
            ;; needed for now
            )
        (goto-clip (1- (param 1))
                   (1- (param 0)))]
       ;; \E[A - cursor up (terminfo: cuu, cuu1)
       ((= char (char->integer #\A))
        (let ((tcr (current-row)))
          (vertical-move
           (if (< (- tcr (param)) term-scroll-start)
             ;; If the amount to move is before scroll start, move
             ;; to scroll start.
             (- term-scroll-start tcr)
             (if (>= (param) tcr)
               (- tcr)
               (- (max 1 (param))))) #t)))
       ;; \E[B - cursor down (terminfo: cud)
       ((= char (char->integer #\B))
        (let ((tcr (current-row)))
          (unless (= tcr (1- term-scroll-end))
            (vertical-move
             (if (> (+ tcr (param)) term-scroll-end)
               (- term-scroll-end 1 tcr)
               (max 1 (param))) #t))))
       ;; \E[C - cursor right (terminfo: cuf, cuf1)
       ((= char (char->integer #\C))
        (term-move-columns
         (max 1
              (if (>= (+ (param) (current-col)) (current-width))
                (- (current-width) (current-col)  1)
                (param)))))
       ;; \E[D - cursor left (terminfo: cub)
       ((= char (char->integer #\D))
        (term-move-columns (- (max 1 (param)))))
       ;; \E[J - clear to end of screen (terminfo: ed, clear)
       ((= char (char->integer #\J))
        (term-erase-in-display (param)))
       ;; \E[K - clear to end of line (terminfo: el, el1)
       ((= char (char->integer #\K))
        (erase-in-line (param)))
       ;; \E[L - insert lines (terminfo: il, il1)
       ((= char (char->integer #\L))
        (term-insert-lines (max 1 (param))))
       ;; \E[M - delete lines (terminfo: dl, dl1)
       ((= char (char->integer #\M))
        (term-delete-lines (max 1 (param))))
       ;; \E[P - delete chars (terminfo: dch, dch1) (8.3.26)
       ((= char (char->integer #\P))
        (term-delete-chars (max 1 (param))))
       ;; \E[@ - insert spaces (terminfo: ich)
       ((= char (char->integer #\@))
        (term-insert-spaces (max 1 (param))))
       ;; \E[?h - DEC Private Mode Set
       ((= char (char->integer #\h))
        (cond ((= (param) 4)  ;; (terminfo: smir)
               (slot-set! port 'insert-mode #t))
              ;; ((eq (param) 47) ;; (terminfo: smcup)
              ;; (term-switch-to-alternate-sub-buffer #t))
              ))
       ;; \E[?l - DEC Private Mode Reset
       ((= char (char->integer #\l))
        (cond ((= (param) 4)  ;; (terminfo: rmir)
               (slot-set! port 'insert-mode #f))
              ;; ((eq (param) 47) ;; (terminfo: rmcup)
              ;; (term-switch-to-alternate-sub-buffer nil))
              ))

       ;; \E[m - Set/reset modes, set bg/fg
       ;;(terminfo: smso,rmso,smul,rmul,rev,bold,sgr0,invis,op,setab,setaf)
       ((= char (char->integer #\m))
        (for-each term-handle-colors-array (reverse (ref port 'params))))

       ;; \E[6n - Report cursor position
       ((= char (char->integer #\n))
        (term-handle-deferred-scroll)
        (process-send-string proc
                             (format "\e[%s;%sR"
                                     (1+ (current-row))
                                     (1+ (current-col)))))
       ;; \E[r - Set scrolling region (terminfo: csr)
       ((= char (char->integer #\r))
        (term-set-scroll-region
         (1- (param 1))
         (1- (param))))))

    ;; todo
    (define (beep))
    (define (form-feed)
      ;; Sequence: FF (^L)
      ;; Mnemonic: FF
      ;; Description: Form feed
      ;; FF causes the active presentation position to be moved to the
      ;; corresponding character position of the line at the page home position
      ;; of the next form or page in the presentation component. The page home
      ;; position is established by the parameter value of SET PAGE HOME (SPH).
      ;; Source: ECMA-48 5th Ed. 8.3.51
      ;; Status: standard
      (goto 0 0)
      )

    (define (save-cursor)
      (slot-set! port 'saved-cursor (list (current-row)
                                          (current-col)
                                          term-ansi-current-bg-color
                                          term-ansi-current-bold
                                          term-ansi-current-color
                                          term-ansi-current-invisible
                                          term-ansi-current-reverse
                                          term-ansi-current-underline
                                          term-current-face)))

    (define (restore-cursor)
      (error "restore-cursor")
      (when (ref 'buffer 'saved-cursor)
        (goto (nth 0 term-saved-cursor)
                   (nth 1 term-saved-cursor))
        (setq term-ansi-current-bg-color
              (nth 2 term-saved-cursor)
              term-ansi-current-bold
              (nth 3 term-saved-cursor)
              term-ansi-current-color
              (nth 4 term-saved-cursor)
              term-ansi-current-invisible
              (nth 5 term-saved-cursor)
              term-ansi-current-reverse
              (nth 6 term-saved-cursor)
              term-ansi-current-underline
              (nth 7 term-saved-cursor)
              term-current-face
              (nth 8 term-saved-cursor))))

    (define (param-set! x . args)
      (let-optionals* args ((pos 0))
        ;; #?=#`"Ps,|pos|=,x"
        (set! (ref (ref port 'params) pos) x)))

    (define (param . args)
      (let-optionals* args ((pos 0)
                            (default -1))
        (if (< pos (size-of (ref port 'params)))
          (ref (ref port 'params) pos)
          default)))

    (define (param-insert x)
      (let1 np (cons x (ref port 'params))
        (slot-set! port 'params (take np (min (size-of np) 4)))
        (debug-assert (list? (ref port 'params)))))

    (define (params-init)
      (slot-set! port 'params (list 0)))

    (define (put-integer-text c)
      (if (char-set-contains? char-set:iso-control (integer->char c))
        (if (= c (char->integer #\escape))
          put-integer-esc
          (begin
            (cond
             ((= c (char->integer #\newline)) (buffer-move buffer 0 1))
             ((= c (char->integer #\return)) (term-move-to-column 0))
             ((= c (char->integer #\delete)) (buffer-delete buffer))
             ((= c (char->integer #\tab))
              (let1 tab-width 8
                (buffer-move-x buffer
                               (- (min (- (current-width) 1)
                                       (+ (current-col)
                                          tab-width
                                          (- (modulo (current-col) tab-width))))
                                  (current-col)))))
             ((= c (char->integer #\x08))
              (when (> (current-col) 0)
                (buffer-move-x buffer -1)))
             ((= c (char->integer #\x07)) (beep))
             ((= c (char->integer #\null))) ; Do nothing
             ((= c (char->integer #\x0c)) (form-feed))
             ((= c 15) #?='("todo: SI ignored"))
             (else (error #`"unknown iso control character: ,|c|")))
            put-integer-text))
        (begin (buffer-set-char! buffer c)
               ;; todo: add hack for last column
               ;; ("emulate xterm/vt100-style line-wrapping")
               (buffer-move buffer 1 0)
               put-integer-text)))

    (define (put-integer-esc c)
      (cond ((= c 91)	  ;; ?\133 = ?[
             (params-init)
             put-integer-ctrl)
	    ((= c (char->integer #\D)) ;; scroll forward
	     (vertical-move 1 #t)
	     put-integer-text)
	    ((= c (char->integer #\M)) ;; scroll reversed (terminfo: ri)
	     (if (or (< (current-row) term-scroll-start)
		     (>= (1- (current-row))
			 term-scroll-start))
               ;; Scrolling up will not move outside
               ;; the scroll region.
               (vertical-move -1)
               ;; Scrolling the scroll region is needed.
               (vertical-move -1 #t))
	     put-integer-text)
	    ((= c 7) ;; Save cursor (terminfo: sc)
	     (term-handle-deferred-scroll)
             (save-cursor)
	     put-integer-text)
	    ((= c 8) ;; Restore cursor (terminfo: rc)
             (restore-cursor)
	     put-integer-text)
	    ((= c (char->integer #\c)) ;; \Ec - Reset (terminfo: rs1)
	     ;; This is used by the "clear" program.
	     (reset)
             put-integer-text
             )
	    ;; The \E#8 reset sequence for xterm. We
	    ;; probably don't need to handle it, but this
	    ;; is the code to parse it.
	    ;; ((eq char ?#)
	    ;;  (when (eq (aref str (1+ i)) ?8)
	    ;;    (setq i (1+ i))
	    ;;    (setq term-scroll-start 0)
	    ;;    (setq term-scroll-end (current-height))
	    ;;    (slot-set! port 'state 'text)))
	    (else put-integer-text)))

    (define (put-integer-ctrl c)
      ;; see 5.4.2
      (cond ((and (>= c (char->integer #\0)) (<= c (char->integer #\9)))
             (debug-assert (number? (param)))
             (param-set! (+ (* 10 (param)) (- c (char->integer #\0))))
             put-integer-ctrl)
	    ((= c (char->integer #\;))
             (param-insert 0)
             put-integer-ctrl)
	    ((= c (char->integer #\?))
             put-integer-ctrl) ; Ignore ?
	    (else
             ;;#?="\npre term-handle-ansi-escape"
             (term-handle-ansi-escape c)
             ;;#?="\npost term-handle-ansi-escape"
             (params-init)
             put-integer-text)))

    ;; see also (gauche-refe.info)Virtual ports
    ;; todo:
    ;; implement putb?
    (define (putc c)
      (slot-set! port
                 'put-integer-current
                 ((slot-ref port 'put-integer-current) (char->integer c))))
    
    (slot-set! port 'put-integer-current put-integer-text)
    (slot-set! port 'buffer buffer)
    (slot-set! port 'putc putc)
    (slot-set! port 'params #f)
    (slot-set! port 'saved-cursor #f)
    port))
