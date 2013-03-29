;;;
;;; setup opengl context using osmesa
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

;; gl-setup implementation using OSMesa

(define-module gl-setup-osmesa
  (use c-wrapper)
  (use gauche.uvector)
  (export gl-open
          gl-close
          gl-get-size
          gl-set-size!
          gl-swap-buffers
          gl-get-frame))

(select-module gl-setup-osmesa)
#?="gl-setup-osmesa init"
(c-load '("GL/osmesa.h") :libs-cmd "echo -L/usr/lib/$(dpkg-architecture -qDEB_HOST_MULTIARCH) -lOSMesa")

(define *width* 640)
(define *height* 480)
(define *ctx* #f)
(define *buffers* #f)
(define *current-buffer* 0)

(define (gl-get-integer pname)
  (let ((i (make <GLint>)))
    (glGetIntegerv pname (ptr i))
    (cast <number> i)))

(define (gl-get-string pname)
  (cast <string> (glGetString pname)))

(define (debug x)
  (display (format "~s\n" x) (current-error-port)))

(define (gl-get-size)
  (list *width* *height*))

(define (gl-get-frame)
  (ref *buffers* (modulo (- *current-buffer* 1) (size-of *buffers*))))

(define (gl-open)
  (set! *ctx* #?=(OSMesaCreateContext GL_RGBA 0))
  (set! *buffers* (map (lambda _ (make-u32vector (* *width* *height*)))
                       '(0 1 2 3)))
  #?=(OSMesaMakeCurrent *ctx* (ref *buffers* *current-buffer*) GL_UNSIGNED_BYTE *width* *height*)
  #?=(OSMesaPixelStore OSMESA_Y_UP 0)
  (glViewport 0 0 *width* *height*)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0 (- *width* 1)
           0 (- *height* 1)
           0 100)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glDisable GL_DEPTH_TEST)
  (glEnable GL_LINE_SMOOTH)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (let ((glerror (glGetError)))
    (when (not (= glerror GL_NO_ERROR))
      (error "GL ERROR:" glerror)))
  (undefined))

(define (gl-close)
  (debug "osmesa.gl-close")
  (glFlush)
  (glFinish)
  (OSMesaDestroyContext *ctx*)
  (set! *ctx* #f)
  (set! *buffers* #f)
  #t)

(define (check-error)
  (let1 e (glGetError)
    (when (not (= 0 e))
      (error "gl error!" e))))

(define (gl-swap-buffers)
  (glFlush)
  (glFinish)
  (set! *current-buffer* (modulo (+ *current-buffer* 1) (size-of *buffers*)))
  (when (not (= GL_TRUE (OSMesaMakeCurrent *ctx* (ref *buffers* *current-buffer*) GL_UNSIGNED_BYTE *width* *height*)))
    (error "failed"))
  (check-error))

(define (gl-set-size! w h)
  (error "todo"))
