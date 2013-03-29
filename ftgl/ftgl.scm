;;;
;;; ftgl bindings
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

(define-module ftgl
  (use c-wrapper)
  (use gl)
  (export ftgl-get-font-line-height
          ftgl-get-font-advance
          ftgl-load-font
          ftgl-render-font))

(select-module ftgl)
(c-load '("FTGL/ftgl.h")
        :cppflags-cmd "pkg-config --cflags ftgl"
        :libs-cmd "pkg-config --libs ftgl"
        :module #f)

(define (ftgl-check-error font)
  (let ((e (ftglGetFontError font)))
    (when (not (zero? e))
      (error font (ftglGetFontError font)))))

(define (ftgl-test font-name)
  (let ((font (ftglCreateTextureFont font-name))
        (bbox (make-f32vector 6)))
    (ftgl-check-error font)
    (ftglSetFontFaceSize font 72 72)
    (ftgl-check-error font)
    (ftglGetFontBBox font "X" -1 bbox)
    (ftgl-check-error font)
    #?=bbox
    #?=(ftglGetFontLineHeight font)
    (ftgl-check-error font)
    #?=(ftglGetFontAdvance font "X")
    (ftgl-check-error font)
    (ftglDestroyFont font)))

(define (ftgl-render-font font s)
  (ftglRenderFont font s FTGL_RENDER_ALL)
  (ftgl-check-error font))

(define (ftgl-load-font font-name font-size)
  (let1 font
      ;;(ftglCreateTextureFont font-name)
      ;;(ftglCreatePolygonFont font-name)
      ;;(ftglCreateOutlineFont font-name)
      (ftglCreateBufferFont font-name)
    (when (null-ptr? font)
      (error "failed to load font" font-name font-size))
    (ftgl-check-error font)
    (ftglSetFontFaceSize font font-size font-size)
    (ftgl-check-error font)
    font))

(define ftgl-get-font-line-height ftglGetFontLineHeight)
(define ftgl-get-font-advance ftglGetFontAdvance)
