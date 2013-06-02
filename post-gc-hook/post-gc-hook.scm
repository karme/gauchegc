(define-module post-gc-hook
  (cond-expand
   (post-gc-hook-via-vport
    (extend post-gc-hook-via-vport))
   (post-gc-hook-via-runtime-compile
    (extend post-gc-hook-via-runtime-compile))
   (post-gc-hook-via-c-wrapper
    (extend post-gc-hook-via-c-wrapper))
   (else
    (extend post-gc-hook-via-weak-vector))))
