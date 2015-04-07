;;;
;;; setup opengl context using SDL 1.2
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

(define-module gl-setup-sdl
  (use sdl)
  (use c-wrapper)
  (use gl)
  (use gauche.sequence)
  (export gl-open
          gl-close
          gl-get-size
          gl-set-size!
          gl-toggle-fullscreen
          gl-swap-buffers))

(select-module gl-setup-sdl)

(define (sdl-error)
  (error (cast <string> (SDL_GetError))))

(define *fullscreen* #t)

(define (gl-toggle-fullscreen . args)
  (let-optionals* args ((f (not *fullscreen*)))
    (apply gl-set-size! (append (gl-get-size)
                                (list :fullscreen f)))
    f))

(define (gl-set-size! w h :key
                      (fullscreen *fullscreen*))
  ;; note: width and height 0 means current screen resolution
  ;; (assuming SDL version > 1.2.?)
  (set! *fullscreen* fullscreen)
  (let1 video (SDL_SetVideoMode w h 0
                                (logior
                                 SDL_OPENGL
                                 SDL_ANYFORMAT
                                 SDL_RESIZABLE
                                 (if fullscreen SDL_FULLSCREEN 0)))
    (when (null-ptr? video)
      (sdl-error))
    (let ((w (ref video 'w))
          (h (ref video 'h)))
      (list w h))))

;; todo: stupid - speed it up
;; (problem was that somehow the gl-setup got loaded twice)
(define (gl-get-size)
  (list (ref (SDL_GetVideoSurface) 'w)
        (ref (SDL_GetVideoSurface) 'h)))

(define (warning s)
  (with-output-to-port (current-error-port) (cute print <>)))
    
(define (gl-open :key
                 (fullscreen #t)
                 (width 0)
                 (height 0)
                 (title "gl-setup"))
  (when (not (zero? (SDL_Init (logior SDL_INIT_VIDEO SDL_INIT_NOPARACHUTE))))
    (sdl-error))
  (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
  (SDL_GL_SetAttribute SDL_GL_SWAP_CONTROL 1)
  (SDL_WM_SetCaption title "")
  ;; sdl disables dpms
  ;; (run-process "xset" "+dpms")
  (let1 ret (gl-set-size! 0 0 :fullscreen fullscreen)
    (let ((val (make <c-int>)))
      (SDL_GL_GetAttribute SDL_GL_DOUBLEBUFFER (ptr val))
      (when (not (= 1 (cast <number> val)))
        (warning "no double buffer"))
      (SDL_GL_GetAttribute SDL_GL_SWAP_CONTROL (ptr val))
      (when (not (= 1 (cast <number> val)))
        (warning "no swap control")))
    ret))

(define (gl-close)
  (SDL_Quit)
  #t)

(define gl-swap-buffers SDL_GL_SwapBuffers)
