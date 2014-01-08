#| -*- mode: scheme; coding: utf-8; -*- |#

;; create opengl textures from image files
;; this implementation uses SDL and SDL_image
;; todo:
;; - use rectangular texture extension if available

(define-module gl-texture
  (use sdl) ;; todo: get rid of it in the osmesa case?
  (use gl)
  (use c-wrapper)
  (use gauche.uvector)
  (use gauche.sequence)
  (use util.list)
  (export gl-texture-load
          ))

(select-module gl-texture)

(define-module c-hack)
(c-load '("unistd.h") :import '(write) :module (find-module 'c-hack))
(define c-write (with-module c-hack write))

;; todo: depends on byte order?!
(define (gl-fmt fmt)
  (values (case fmt
            ((rgb565) GL_RGB)
            ((bgr565) GL_RGB)            
            ((rgb32)  GL_BGRA)
            ((bgr32)  GL_RGBA)
            ((rgb24)  GL_RGB)
            ((bgr24)  GL_BGR)
            (else (error "unsupported format" format)))
          (case fmt
            ((rgb565) GL_UNSIGNED_SHORT_5_6_5_REV)
            ((bgr565) GL_UNSIGNED_SHORT_5_6_5)
            ((rgb32 bgr32 rgb24 bgr24) GL_UNSIGNED_BYTE)
            (else (error "unsupported format" format)))
          (case fmt
            ((rgb565 bgr565) 2)
            ((rgb24 bgr24) 3)
            ((rgb32 bgr32) 4)
            (else (error "unsupported format" format)))))

(define (next-power-of-two i)
  ((rec (pot i c)
        (if (< c i)
            (pot i (* c 2))
            c)) i 1))

(define (fit-image-to-power-of-two image)
  (let* ((fited-image (SDL_CreateRGBSurface SDL_SWSURFACE
                                            (next-power-of-two (ref image 'w))
                                            ;; todo: it seems some buggy drivers even require
                                            ;; equal width and height ?
                                            (next-power-of-two (ref image 'h))
                                            (ref* image 'format 'BitsPerPixel)
                                            (ref* image 'format 'Rmask)
                                            (ref* image 'format 'Gmask)
                                            (ref* image 'format 'Bmask)
                                            (ref* image 'format 'Amask)))
         (saved-flags (logand (ref image 'flags)
                              (logior SDL_SRCALPHA SDL_RLEACCELOK)))
         (saved-alpha (ref* image 'format 'alpha)))
    (when (eq? (logand saved-flags SDL_SRCALPHA) SDL_SRCALPHA)
      (SDL_SetAlpha image 0 0))
    ;; copy smaller input image into bigger one
    (let1 area (make <SDL_Rect>)
      (set! (ref area 'x) 0)
      (set! (ref area 'y) 0)
      (set! (ref area 'w) (ref image 'w))
      (set! (ref area 'h) (ref image 'h))
      (SDL_BlitSurface image (ptr area) fited-image (ptr area))
      ;; restore the alpha blending attributes
      (when (eq? (logand saved-flags SDL_SRCALPHA) SDL_SRCALPHA)
        (SDL_SetAlpha image saved-flags saved-alpha))
      fited-image)))

(define (RGB_PixelFormat)
  (let1 ret (make <SDL_PixelFormat>)
    (set! (ref ret 'BitsPerPixel) (* 3 8))
    (set! (ref ret 'BytesPerPixel) 3)
    (if (eq? SDL_BYTEORDER SDL_LIL_ENDIAN)
        (begin
          (set! (ref ret 'Rmask) #x000000ff)
          (set! (ref ret 'Gmask) #x0000ff00)
          (set! (ref ret 'Bmask) #x00ff0000)
          (set! (ref ret 'Amask) 0))
        (begin
          (set! (ref ret 'Rmask) #x00ff0000)
          (set! (ref ret 'Gmask) #x0000ff00)
          (set! (ref ret 'Bmask) #x000000ff)
          (set! (ref ret 'Amask) #x00000000)))
    (set! (ref ret 'palette) (make-null-ptr))
    (set! (ref ret 'alpha) #xff)
    ret))

(define (image-has-alpha? image)
  (> (ref* image 'format 'Amask) 0))

(define (RGBA_PixelFormat)
  (let1 ret (make <SDL_PixelFormat>)
    (set! (ref ret 'BitsPerPixel) (* 4 8))
    (set! (ref ret 'BytesPerPixel) 4)
    (if (eq? SDL_BYTEORDER SDL_LIL_ENDIAN)
        (begin
          (set! (ref ret 'Rmask) #x000000ff)
          (set! (ref ret 'Gmask) #x0000ff00)
          (set! (ref ret 'Bmask) #x00ff0000)
          (set! (ref ret 'Amask) #xff000000))
        (begin
          (set! (ref ret 'Rmask) #xff000000)
          (set! (ref ret 'Gmask) #x00ff0000)
          (set! (ref ret 'Bmask) #x0000ff00)
          (set! (ref ret 'Amask) #x000000ff)))
    (set! (ref ret 'palette) (make-null-ptr))
    (set! (ref ret 'alpha) #xff)
    ret))

(define (load-image name)
  (let1 ret (IMG_Load name)
    (if (null-ptr? ret)
        (error "Failed to load" name)
        ret)))

(define (delete-texture-proc texname)
  (let ((t (coerce-to <u32vector> (list texname))))
    (lambda()
      (gl-delete-textures t))))

(define (texture-resident? texname)
  (let ((names (coerce-to <u32vector> (list texname)))
        (residences (gl-boolean-vector #t)))
    (gl-are-textures-resident! names residences)))

;; create texture
(define (gl-texture width height pixfmt update-proc)
  (receive (glf glt psize) (gl-fmt pixfmt)
    (let ((glfi (case psize
                  ((2) GL_RGB4)
                  ((3) GL_RGB)
                  ((4) GL_RGBA)))) ;; todo
      (let* ((texnames (gl-gen-textures 1))
             (texname (ref texnames 0))
             (data (case pixfmt
                     ((rgb565 bgr565) (make-u16vector (* width height)))
                     ((rgb32 bgr32 rgb24 bgr24) (make-u8vector (* width height psize)))
                     (else (error "fmt not supported" pixfmt))))
             (update-texture (lambda()
                               (gl-tex-image-2d GL_TEXTURE_2D 0
                                                glfi
                                                width height 0
                                                glf
                                                glt
                                                data)
                               (check-error))))
        (when (or (> width (gl-get-integer GL_MAX_TEXTURE_SIZE))
                  (> height (gl-get-integer GL_MAX_TEXTURE_SIZE)))
          ;; todo: downscale or use multiple textures
          (error (format "texture too big: (~s,~s) > (~s,~s)"
                         width height (gl-get-integer GL_MAX_TEXTURE_SIZE) (gl-get-integer GL_MAX_TEXTURE_SIZE))))
        (gl-bind-texture GL_TEXTURE_2D texname)
        (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
        (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
        (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
        (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
        (glTexImage2D GL_PROXY_TEXTURE_2D 0 #?=glfi width height 0 glf glt (make-null-ptr))
        (check-error)
        (let ((glfi-real (make <GLuint>)))
          (glGetTexLevelParameteriv GL_PROXY_TEXTURE_2D 0 GL_TEXTURE_INTERNAL_FORMAT (ptr glfi-real))
          (when (zero? #?=(cast <number> glfi-real))
            (with-output-to-port (current-error-port) (cut print "WARNING: texture loading probably will fail"))))
        (update-texture)
        (check-error)
        (let ((i (make <GLuint>)))
          (glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_INTERNAL_FORMAT (ptr i))
          #?=i
          (glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_RED_SIZE (ptr i))
          #?=i
          (glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_GREEN_SIZE (ptr i))
          #?=i
          (glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_BLUE_SIZE (ptr i))
          #?=i
          (glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_ALPHA_SIZE (ptr i))
          #?=i)
                                    
        (let1 ret `((tw . 1)
                    (th . 1)
                    (w . ,width)
                    (h . ,height)
                    (name . ,texname)
                    (update-proc . ,(lambda l (when (apply update-proc (cons data l))
                                                (update-texture))))
                    (read-back . ,(lambda()
                                    (gl-bind-texture GL_TEXTURE_2D texname)
                                    (let ((ret (make-u16vector (* width height))))
                                      (glGetTexImage GL_TEXTURE_2D 0 glf glt ret)
                                      ret)))
                    (read-back! . ,(lambda()
                                     (gl-bind-texture GL_TEXTURE_2D texname)
                                     (glGetTexImage GL_TEXTURE_2D 0 glf glt data)
                                     (check-error)
                                     data))
                    (free . ,(cut delete-texture-proc texname))))))))


;; load image to texture using SDL_image
(define (gl-texture-load name)
  (let* (;; load image
         (image-in (load-image name))
         ;; convert to RGB or RGBA
         ;; todo: do this only if needed!
         (image-converted
          (if (image-has-alpha? image-in)
              (SDL_ConvertSurface image-in
                                  (ptr (RGBA_PixelFormat)) SDL_SWSURFACE)
              (SDL_ConvertSurface image-in
                                  (ptr (RGB_PixelFormat)) SDL_SWSURFACE)))
         ;; fit into power of two sized image
         ;; todo: do this only if needed!
         (fited-image (if #t
                        (fit-image-to-power-of-two image-converted)
                        image-converted))
         (texnames (gl-gen-textures 1))
         (o-width (ref image-in 'w))
         (o-height (ref image-in 'h))
         (n-width (ref fited-image 'w))
         (n-height (ref fited-image 'h))
         (texname (ref texnames 0)))
    (when (or (> n-width (gl-get-integer GL_MAX_TEXTURE_SIZE))
              (> n-height (gl-get-integer GL_MAX_TEXTURE_SIZE)))
      ;; todo: downscale or use multiple textures
      (error (format "texture too big: (~s,~s) > (~s,~s)" n-width n-height (gl-get-integer GL_MAX_TEXTURE_SIZE) (gl-get-integer GL_MAX_TEXTURE_SIZE))))
    (gl-bind-texture GL_TEXTURE_2D texname)
    (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
    (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
    (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)

    (glTexImage2D GL_PROXY_TEXTURE_2D
                  0
                  #?=(if (image-has-alpha? image-in) GL_RGBA8 GL_RGB8)
                  n-width
                  n-height
                  0
                  (if (image-has-alpha? image-in) GL_RGBA GL_RGB)
                  GL_UNSIGNED_BYTE
                  (make-null-ptr))
    (let ((glfi-real (make <GLuint>)))
      (glGetTexLevelParameteriv GL_PROXY_TEXTURE_2D 0 GL_TEXTURE_INTERNAL_FORMAT (ptr glfi-real))
      (when (zero? #?=(cast <number> glfi-real))
        (with-output-to-port (current-error-port) (cut print "WARNING: texture loading probably will fail"))))

    (SDL_LockSurface fited-image)
    ;; we don't use gl-tex-image-2d for performance reasons
    (glTexImage2D GL_TEXTURE_2D
                  0
                  (if (image-has-alpha? image-in) GL_RGBA8 GL_RGB8)
                  n-width
                  n-height
                  0
                  (if (image-has-alpha? image-in) GL_RGBA GL_RGB)
                  GL_UNSIGNED_BYTE
                  (ref fited-image 'pixels))
    (SDL_UnlockSurface fited-image)
    (SDL_FreeSurface image-in)
    (when (not (eq? image-converted fited-image))
      (SDL_FreeSurface image-converted))
    (SDL_FreeSurface fited-image)
    ;; #?=(texture-resident? texname)
    (let1 ret `((tw . ,(/ o-width n-width))
                (th . ,(/ o-height n-height))
                (w . ,o-width)
                (h . ,o-height)
                (name . ,texname)
                (free . ,(cut delete-texture-proc texname))))))

(define *num-pbo-buffers* 10)
(define *pbo-buffers* #f)
(define *current-pbo-buffer* 0)

(define gl-read-pixels! glReadPixels)

(define (check-error)
  (let1 e (gl-get-error)
    (when (not (= 0 e))
      (error e))))

;; two step read pixel using pbo
(define (gl-read-pixels2! x y w h f t psize proc)
  (when (not *pbo-buffers*)
    (set! *pbo-buffers* (make (c-array <GLuint> *num-pbo-buffers*)))
    #?=(glGenBuffers *num-pbo-buffers* *pbo-buffers*)
    (dotimes (i *num-pbo-buffers*)
      #?=(glBindBuffer GL_PIXEL_PACK_BUFFER (ref *pbo-buffers* i))
      (check-error)
      #?=w
      #?=h
      #?=(glBufferData GL_PIXEL_PACK_BUFFER #?=(* w h psize) 0 GL_DYNAMIC_READ)
      (check-error)))

  (glBindBuffer GL_PIXEL_PACK_BUFFER (ref *pbo-buffers* *current-pbo-buffer*))
  ;;(check-error)
  ;; idea is that this only starts a dma transfer, on my hardware it doesn't
  (glReadPixels x y w h f t 0) ;; 0 = offset
  ;;(check-error)
  (glBindBuffer GL_PIXEL_PACK_BUFFER 0)
  ;;(check-error)
  (let* ((buf (ref *pbo-buffers* *current-pbo-buffer*))
         (ret (lambda()
                ;;                 #?=*pbo-buffers*
                ;;                 #?=buf
                (glBindBuffer GL_PIXEL_PACK_BUFFER buf)
                ;;(check-error)
                (proc (glMapBuffer GL_PIXEL_PACK_BUFFER GL_READ_ONLY))
                ;; todo: check for != GL_TRUE
                (glUnmapBuffer GL_PIXEL_PACK_BUFFER)
                (glBindBuffer GL_PIXEL_PACK_BUFFER 0))))
    (set! *current-pbo-buffer* (modulo (+ *current-pbo-buffer* 1) *num-pbo-buffers*))
    ret))

;; thanks to Rob 'phantom' Jones
;; http://www.gamedev.net/reference/articles/article2331.asp
;; todo: cleanup
(define (gl-create-fbo width height)
  (let ((fbo (make <GLuint>))
        (depthbuffer (make <GLuint>))
        (img (ref (gl-gen-textures 1) 0))) ;;make <GLuint>)))
    (glGenFramebuffersEXT 1 (ptr fbo))
    (glBindFramebufferEXT GL_FRAMEBUFFER_EXT fbo)
    #?=fbo
    (glGenRenderbuffersEXT 1 (ptr depthbuffer))
    #?=depthbuffer
    (check-error)
    (glBindRenderbufferEXT GL_RENDERBUFFER_EXT depthbuffer)
    (glRenderbufferStorageEXT GL_RENDERBUFFER_EXT  GL_DEPTH_COMPONENT width height)
    (glFramebufferRenderbufferEXT GL_FRAMEBUFFER_EXT GL_DEPTH_ATTACHMENT_EXT GL_RENDERBUFFER_EXT depthbuffer)
    (check-error)
    (glBindTexture GL_TEXTURE_2D #?=img)
    (check-error)
    (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 width height 0 GL_RGBA GL_UNSIGNED_BYTE (make-null-ptr))
    (check-error)
    (glFramebufferTexture2DEXT GL_FRAMEBUFFER_EXT GL_COLOR_ATTACHMENT0_EXT GL_TEXTURE_2D img 0)
    (check-error)
    (when (not (= #?=GL_FRAMEBUFFER_COMPLETE_EXT #?=(glCheckFramebufferStatusEXT GL_FRAMEBUFFER_EXT)))
      (error "failed"))
    (glBindFramebufferEXT GL_FRAMEBUFFER_EXT 0)
    (lambda(proc)
      (glBindFramebufferEXT GL_FRAMEBUFFER_EXT fbo)
      (glPushAttrib GL_VIEWPORT_BIT)
      (glViewport 0 0 width height)
      (proc)
      (glPopAttrib)
      (glBindFramebufferEXT GL_FRAMEBUFFER_EXT 0))))

(define (gl-fb->texture texture)
  (gl-bind-texture GL_TEXTURE_2D (assoc-ref texture 'name))
  (glCopyTexImage2D GL_TEXTURE_2D 0 GL_RGB4 0 0 (assoc-ref texture 'w) (assoc-ref texture 'h) 0)
;;   (let ((i (make <GLuint>)))
;;     (glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_INTERNAL_FORMAT (ptr i))
;;     #?=i
;;     (glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_RED_SIZE (ptr i))
;;     #?=i
;;     (glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_GREEN_SIZE (ptr i))
;;     #?=i
;;     (glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_BLUE_SIZE (ptr i))
;;     #?=i
;;     (glGetTexLevelParameteriv GL_TEXTURE_2D 0 GL_TEXTURE_ALPHA_SIZE (ptr i))
;;     #?=i)
  (check-error))

(define (write-rows-flipped w h buf)
  (when (not (= (* w h) (size-of buf)))
    (error "should not happen!" (* w h) (class-of buf) (size-of buf)))
  (dotimes (y h)
    (let ((offset (* (- h 1 y) w)))
      (write-block buf (current-output-port) offset (+ offset w))))
  buf)

(define (write-rows-flipped-2 w h psize buf)
  (when (not (= (* w h psize) (size-of buf)))
    (error "should not happen!"))
  (dotimes (y h)
    (let ((offset (* (- h 1 y) w psize)))
      (write-block buf (current-output-port) offset (+ offset (* w psize)))))
  buf)

(define (uvec-writer make-uvec glf glt)
  (lambda(wh buf)
    (receive (w h)
        (apply values wh)
      (let ((buf (if (not (= (size-of buf) (* w h)))
                   (make-uvec (* w h))
                   buf)))
        (gl-read-pixels! 0 0 w h glf glt buf)
        (write-rows-flipped w h buf)))))

(define (rgb24-writer wh buf glf glt)
  (receive (w h)
      (apply values wh)
    (let ((buf (if (not (= (size-of buf) (* w h 3)))
                 (make-u8vector (* w h 3))
                 buf)))
      (gl-read-pixels! 0 0 w h glf glt buf)
      (write-rows-flipped-2 w h 3 buf))))

(define (pixfmt->write-frame pixfmt)
  (receive (glf glt psize) (gl-fmt pixfmt)
    (case pixfmt
      ((rgb565 bgr565) write-rows-flipped)
      ((rgb32 bgr32 rgb24 bgr24) (cut write-rows-flipped-2 <> <> psize <>))
      (else (error "unsupported format" format)))))

(define (gl-frame-writer pixfmt)
  (receive (glf glt psize) (gl-fmt pixfmt)
    (let ((buf '())
          (writer (case pixfmt
                    ((rgb565)      (uvec-writer make-u16vector glf glt))
                    ((rgb32 bgr32) (uvec-writer make-u32vector glf glt))
                    ((rgb24 bgr24) (cut rgb24-writer <> glf glt))
                    (else (error "unsupported format" format)))))
      (lambda(wh)
        (set! buf (writer wh buf))))))

;; unfortunately not faster :(
(define (gl-frame-writer2 pixfmt)
  (receive (glf glt psize) (gl-fmt pixfmt)
    (let ((frames (list)))
      (lambda(wh)
        (when (>= (size-of frames) *num-pbo-buffers*)
          ((car frames))
          (set! frames (cdr frames)))
        (receive (w h) (apply values wh)
          (set! frames
                (reverse
                 (cons
                  (gl-read-pixels2! 0 0 w h glf glt psize
                                    (lambda(data)
                                      (dotimes (y h)
                                        ;; todo: buffered write could improve performance!
                                        ;; on the other hand i don't want to copy byte for byte in scheme ;-)
                                        (c-write (port-file-number (current-output-port))
                                                 (c-ptr+ data (* (- h 1 y) (* w psize)))
                                                 (* w psize))
                                        )
                                      ;;(flush)
                                      ))
                  (reverse frames)))))))))

(define *num-tex-buffers* 2)
(define *tex-buffers* #f)
(define *current-tex-buffer* 0)

;; two step read pixel using texture
(define (gl-read-pixels3! x y w h fmt proc)
  (when (not *tex-buffers*)
    (let ((w 512)
          (h 512))
      (set! *tex-buffers* (list
                           (gl-texture w h fmt (lambda l #f))
                           (gl-texture w h fmt (lambda l #f))))
      (check-error)))
  (gl-fb->texture (ref *tex-buffers* *current-tex-buffer*))
  (let* ((tex (ref *tex-buffers* *current-tex-buffer*))
         (ret (lambda()
                (proc ((assoc-ref tex 'read-back!))))))
    (set! *current-tex-buffer* (modulo (+ *current-tex-buffer* 1) *num-tex-buffers*))
    ret))

(define (gl-frame-writer3 pixfmt)
  (let ((frames (list))
        (write-frame (pixfmt->write-frame pixfmt)))
    (lambda(wh)
      (when (>= (size-of frames) *num-tex-buffers*)
        ((car frames))
        (set! frames (cdr frames)))
      (receive (w h) (apply values wh)
        (set! frames
              (reverse
               (cons
                (gl-read-pixels3! 0 0 w h pixfmt (cut write-frame w h <>))
                (reverse frames))))))))

(define (gl-frame-writer4 pixfmt)
  (when (not (eq? pixfmt 'bgr32))
    (error "unsupported format" pixfmt))
  (lambda(wh)
    (receive (w h)
        (apply values wh)
      (let ((block (gl-get-frame)))
        (when (not (= (size-of block) (* w h)))
          (error "todo"))
        (write-block block)))))

