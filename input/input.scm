;;;
;;; simple input abstraction layer
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

;; at the moment there is only a SDL 1.2 implementation

(define-module input
  (use sdl)
  (use c-wrapper)
  (use gauche.collection)
  (use gauche.sequence)
  (use util.list)
  (export input-open
          input-close
          input-poll-event
          input-show-cursor))

(select-module input)

(define-macro (debug-assert e)
  `(if (not ,e)
     (error "Debug assertion failed: " ,(x->string e))))

(define (c-struct->alist c-struct)
  (let ((slots (map car (ref (class-of c-struct) 'decl-alist))))
    (map (lambda(slot) (cons slot (ref c-struct slot))) slots)))

(define (generic-event->alist c-struct . args)
  (let-optionals* args ((skip '(type)))
    (filter (lambda(m) (not (any (cut eq? (car m) <>) skip)))
            (c-struct->alist c-struct))))

(define keysym-value->symbol (make-hash-table))
(define mod-value->symbol (make-hash-table))

;; uhh :-(
(define-macro (register-keysyms . l)
  (cons 'begin 
        (map (lambda(sym)
               (list 'hash-table-put! 
                     'keysym-value->symbol 
                     sym `(quote ,(string->symbol
                                   (subseq (symbol->string sym) 3))))) l)))

(define-macro (register-mod . l)
  (cons 'begin 
        (map (lambda(sym)
               (list 'hash-table-put! 
                     'mod-value->symbol 
                     sym `(quote ,(string->symbol
                                   (subseq (symbol->string sym) 5)))))
             l)))

(register-keysyms SDLK_FIRST SDLK_BACKSPACE SDLK_TAB SDLK_CLEAR
                  SDLK_RETURN SDLK_PAUSE SDLK_ESCAPE SDLK_SPACE SDLK_EXCLAIM
                  SDLK_QUOTEDBL SDLK_HASH SDLK_DOLLAR SDLK_AMPERSAND SDLK_QUOTE
                  SDLK_LEFTPAREN SDLK_RIGHTPAREN SDLK_ASTERISK SDLK_PLUS
                  SDLK_COMMA SDLK_MINUS SDLK_PERIOD SDLK_SLASH SDLK_0
                  SDLK_1 SDLK_2 SDLK_3 SDLK_4 SDLK_5 SDLK_6 SDLK_7
                  SDLK_8 SDLK_9 SDLK_COLON SDLK_SEMICOLON SDLK_LESS SDLK_EQUALS
                  SDLK_GREATER SDLK_QUESTION SDLK_AT SDLK_LEFTBRACKET
                  SDLK_BACKSLASH SDLK_RIGHTBRACKET SDLK_CARET SDLK_UNDERSCORE
                  SDLK_BACKQUOTE SDLK_a SDLK_b SDLK_c SDLK_d SDLK_e SDLK_f
                  SDLK_g SDLK_h SDLK_i SDLK_j SDLK_k SDLK_l SDLK_m SDLK_n
                  SDLK_o SDLK_p SDLK_q SDLK_r SDLK_s SDLK_t SDLK_u SDLK_v SDLK_w
                  SDLK_x SDLK_y SDLK_z SDLK_DELETE SDLK_WORLD_0 SDLK_WORLD_1
                  SDLK_WORLD_2 SDLK_WORLD_3 SDLK_WORLD_4 SDLK_WORLD_5
                  SDLK_WORLD_6 SDLK_WORLD_7 SDLK_WORLD_8 SDLK_WORLD_9
                  SDLK_WORLD_10 SDLK_WORLD_11 SDLK_WORLD_12 SDLK_WORLD_13
                  SDLK_WORLD_14 SDLK_WORLD_15 SDLK_WORLD_16 SDLK_WORLD_17
                  SDLK_WORLD_18 SDLK_WORLD_19 SDLK_WORLD_20 SDLK_WORLD_21
                  SDLK_WORLD_22 SDLK_WORLD_23 SDLK_WORLD_24 SDLK_WORLD_25
                  SDLK_WORLD_26 SDLK_WORLD_27 SDLK_WORLD_28 SDLK_WORLD_29
                  SDLK_WORLD_30 SDLK_WORLD_31 SDLK_WORLD_32 SDLK_WORLD_33
                  SDLK_WORLD_34 SDLK_WORLD_35 SDLK_WORLD_36 SDLK_WORLD_37
                  SDLK_WORLD_38 SDLK_WORLD_39 SDLK_WORLD_40 SDLK_WORLD_41
                  SDLK_WORLD_42 SDLK_WORLD_43 SDLK_WORLD_44 SDLK_WORLD_45
                  SDLK_WORLD_46 SDLK_WORLD_47 SDLK_WORLD_48 SDLK_WORLD_49
                  SDLK_WORLD_50 SDLK_WORLD_51 SDLK_WORLD_52 SDLK_WORLD_53
                  SDLK_WORLD_54 SDLK_WORLD_55 SDLK_WORLD_56 SDLK_WORLD_57
                  SDLK_WORLD_58 SDLK_WORLD_59 SDLK_WORLD_60 SDLK_WORLD_61
                  SDLK_WORLD_62 SDLK_WORLD_63 SDLK_WORLD_64 SDLK_WORLD_65
                  SDLK_WORLD_66 SDLK_WORLD_67 SDLK_WORLD_68 SDLK_WORLD_69
                  SDLK_WORLD_70 SDLK_WORLD_71 SDLK_WORLD_72 SDLK_WORLD_73
                  SDLK_WORLD_74 SDLK_WORLD_75 SDLK_WORLD_76 SDLK_WORLD_77
                  SDLK_WORLD_78 SDLK_WORLD_79 SDLK_WORLD_80 SDLK_WORLD_81
                  SDLK_WORLD_82 SDLK_WORLD_83 SDLK_WORLD_84 SDLK_WORLD_85
                  SDLK_WORLD_86 SDLK_WORLD_87 SDLK_WORLD_88 SDLK_WORLD_89
                  SDLK_WORLD_90 SDLK_WORLD_91 SDLK_WORLD_92 SDLK_WORLD_93
                  SDLK_WORLD_94 SDLK_WORLD_95 SDLK_KP0 SDLK_KP1 SDLK_KP2
                  SDLK_KP3 SDLK_KP4 SDLK_KP5 SDLK_KP6 SDLK_KP7 SDLK_KP8
                  SDLK_KP9 SDLK_KP_PERIOD SDLK_KP_DIVIDE SDLK_KP_MULTIPLY
                  SDLK_KP_MINUS SDLK_KP_PLUS SDLK_KP_ENTER SDLK_KP_EQUALS
                  SDLK_UP SDLK_DOWN SDLK_RIGHT SDLK_LEFT SDLK_INSERT SDLK_HOME
                  SDLK_END SDLK_PAGEUP SDLK_PAGEDOWN SDLK_F1 SDLK_F2 SDLK_F3
                  SDLK_F4 SDLK_F5 SDLK_F6 SDLK_F7 SDLK_F8 SDLK_F9 SDLK_F10
                  SDLK_F11 SDLK_F12 SDLK_F13 SDLK_F14 SDLK_F15 SDLK_NUMLOCK
                  SDLK_CAPSLOCK SDLK_SCROLLOCK SDLK_RSHIFT SDLK_LSHIFT
                  SDLK_RCTRL SDLK_LCTRL SDLK_RALT SDLK_LALT SDLK_RMETA
                  SDLK_LMETA SDLK_LSUPER SDLK_RSUPER SDLK_MODE SDLK_COMPOSE
                  SDLK_HELP SDLK_PRINT SDLK_SYSREQ SDLK_BREAK SDLK_MENU
                  SDLK_POWER SDLK_EURO SDLK_UNDO SDLK_LAST)

;; #?=(hash-table->alist value->symbol)

(register-mod KMOD_NONE KMOD_LSHIFT KMOD_RSHIFT KMOD_LCTRL KMOD_RCTRL
              KMOD_LALT KMOD_RALT KMOD_LMETA KMOD_RMETA KMOD_NUM KMOD_CAPS
              KMOD_MODE)

(define (sdl-keysym-value->name keysym)
  (hash-table-get keysym-value->symbol keysym))

(define (sdl-mod-value->name mod)
  (hash-table-get mod-value->symbol mod))

(define (sdl-modifiers->input-modifiers mod)
  (filter identity
          (map (lambda(x)
                 (if (> (logand mod x) 0)
                   (sdl-mod-value->name x)
                   #f))
               (list KMOD_NONE KMOD_LSHIFT KMOD_RSHIFT KMOD_LCTRL
                     KMOD_RCTRL KMOD_LALT KMOD_RALT KMOD_LMETA KMOD_RMETA
                     KMOD_NUM KMOD_CAPS KMOD_MODE))))

(define (sdl-key-event->input-event event)
  (debug-assert (or (eq? (ref event 'type) SDL_KEYDOWN)
                    (eq? (ref event 'type) SDL_KEYUP)))
  (list
   (if (eq? (ref event 'type) SDL_KEYDOWN) 'key-down 'key-up)
   (append `((symname . ,(sdl-keysym-value->name (ref* event 'key 'keysym 'sym)))
             (modifiers . ,(sdl-modifiers->input-modifiers
                            (ref* event 'key 'keysym 'mod))))
           (generic-event->alist (ref* event 'key) '(type keysym))
           (generic-event->alist (ref* event 'key 'keysym)))))

(define (sdl-event->input-event event)
  (cond [(eq? (ref event 'type) SDL_VIDEORESIZE)
         (let ((w (ref* event 'resize 'w))
               (h (ref* event 'resize 'h)))
           (debug-print (format "got resize event ~s ~s" w h))
           `(resized ,w ,h))]
        [(or (eq? (ref event 'type) SDL_QUIT)
             ;; todo: maybe remove
             (and (eq? (ref event 'type) SDL_KEYDOWN)
                  (eq? (ref* event 'key 'keysym 'sym) SDLK_ESCAPE)
                  (not (zero? (ref* event 'key 'keysym 'mod)))))
         '(quit)]
        ;; todo: maybe remove
        [(and (eq? (ref event 'type) SDL_KEYDOWN)
              (eq? (ref* event 'key 'keysym 'sym) SDLK_RETURN)
              (not (zero? (ref* event 'key 'keysym 'mod))))
         (SDL_WM_ToggleFullScreen (SDL_GetVideoSurface))
         `(fullscreen-toggle ,(not (= 0 (logand SDL_FULLSCREEN (ref (SDL_GetVideoSurface) 'flags)))))]
        [(eq? (ref event 'type) SDL_MOUSEMOTION)
         `(mouse-motion ,(generic-event->alist (ref* event 'motion)))]
        [(eq? (ref event 'type) SDL_MOUSEBUTTONDOWN)
         `(mouse-button-down ,(generic-event->alist (ref* event 'button)))]
        [(eq? (ref event 'type) SDL_MOUSEBUTTONUP)
         `(mouse-button-up ,(generic-event->alist (ref* event 'button)))]
        [(any (cut equal? (ref event 'type) <>) (list SDL_KEYDOWN SDL_KEYUP))
         (sdl-key-event->input-event event)]
        [(eq? (ref event 'type) SDL_VIDEOEXPOSE)
         '(video-expose ())]
        ;; todo: sdl reports multiple changes as one event
        [(and (eq? (ref event 'type) SDL_ACTIVEEVENT)
              (> (logand (ref* event 'active 'state) SDL_APPACTIVE) 0))
         `(visibility-change ,(= (ref* event 'active 'gain) 1))]
        ;; todo: for now ignore SDL_ACTIVEEVENTs not handled above
        [(eq? (ref event 'type) SDL_ACTIVEEVENT)
         #t]
        [else
         #?=(format "unknown event of type ~s: ~s, video size: ~s"
                    (ref event 'type) event
                    (cons (ref (SDL_GetVideoSurface) 'w)
                          (ref (SDL_GetVideoSurface) 'h)))
         `(unsupported-event ,event)]))

(define (input-poll-event)
  (let* ((event (make <SDL_Event>))
         (ret (SDL_PollEvent (ptr event))))
    (if (not (= ret 1))
      #f
      (sdl-event->input-event event))))

(define (input-open)
  (SDL_EnableUNICODE 1)
  (SDL_EnableKeyRepeat SDL_DEFAULT_REPEAT_DELAY SDL_DEFAULT_REPEAT_INTERVAL)
  ;; todo: hack
  ;; sdl api doesn't really support select based input handling
  (list 3))

(define (input-close)
  )

(define (input-show-cursor show?)
  (cond [show?
         (SDL_ShowCursor SDL_ENABLE)
         (SDL_WM_GrabInput SDL_GRAB_OFF)]
        [else
         (SDL_ShowCursor SDL_DISABLE)
         (SDL_WM_GrabInput SDL_GRAB_ON)]))
