(define-module pipe
  (use c-wrapper)
  (use gauche.uvector)
  (export set-pipe-buffer-size!
          get-pipe-buffer-size
          vmsplice-block
          c-vmsplice))

(select-module pipe)
(c-load "fcntl.h" :cppflags "-D_GNU_SOURCE")
(c-load '("stdint.h" "errno.h"))

(define (get-errno)
  ;; todo: i am not sure how errno is handled by gauche/c-wrapper in different versions
  (cast <number> (deref (__errno_location))))

(define (my-fcntl port . l)
  (apply fcntl (cons (port-file-number port) l)))

;; todo: F_GETPIPE_SZ (void; since Linux 2.6.35)
(define (get-pipe-buffer-size p)
  (when (global-variable-bound? (current-module) 'F_GETPIPE_SZ)
    (my-fcntl p F_GETPIPE_SZ)))

;; todo: F_SETPIPE_SZ (long; since Linux 2.6.35)
(define (set-pipe-buffer-size! p rns)
  (when (global-variable-bound? (current-module) 'F_SETPIPE_SZ)
    (my-fcntl p F_SETPIPE_SZ rns)
    (let1 ns (my-fcntl p F_GETPIPE_SZ)
      (when (not (<= rns ns))
        (error #`"could not increase buffer size to ,|rns|! please adjust kernel setting fs.pipe-max-size"))
      ns)))

(define vmsplice-block
  (let1 iov (make <c-struct:iovec>)
    (lambda(b)
      (let ((s (uvector-size b))
            (offset 0))
        (while (< offset s)
          (set! (ref iov 'iov_base)
                (c-ptr+ (cast (ptr <c-void>) b) offset))
          ;; todo
          (when (not (zero? (modulo (cast <number>
                                          (cast <uintptr_t>
                                                (ref iov 'iov_base)))
                                    4096)))
            #?="maybe not page alligned?")
          (set! (ref iov 'iov_len) (- s offset))
          (let1 r (vmsplice (port-file-number (current-output-port)) (ptr iov) 1 0)
            (when (<= r 0)
              (error "splice failed"))
            (set! offset (+ offset r))))))))

(define c-vmsplice
  (let1 iov (make <c-struct:iovec>)
    (lambda(p s)
      (let ((offset 0))
        (while (< offset s)
          (set! (ref iov 'iov_base)
                (c-ptr+ p offset))
          ;; todo
          (when (not (zero? (modulo (cast <number>
                                          (cast <uintptr_t>
                                                (ref iov 'iov_base)))
                                    4096)))
            #?="maybe not page alligned?")
          (set! (ref iov 'iov_len) (- s offset))
          (let1 r (vmsplice (port-file-number (current-output-port)) (ptr iov) 1 0)
            (when (<= r 0)
              (error "splice failed: " (get-errno)))
            (set! offset (+ offset r))))))))
