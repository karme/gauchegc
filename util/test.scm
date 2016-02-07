(use gauche.test)
(test-start "ggc.util")
(use ggc.util)
(test-module 'ggc.util)

(test-section "pie and %f")

(test* "pie" "3.14\n"      (with-output-to-string (lambda () (pie 3.141592 2))))
(test* "pie" "-3.14\n"     (with-output-to-string (lambda () (pie -3.141592 2))))
(test* "pie" "3.00\n"      (with-output-to-string (lambda () (pie 3.004 2))))
(test* "pie" "3.01\n"      (with-output-to-string (lambda () (pie 3.005000000000001 2))))
(test* "pie" "3.99\n"      (with-output-to-string (lambda () (pie 3.994999999999999 2))))
(test* "pie" "4.00\n"      (with-output-to-string (lambda () (pie 3.995 2))))
(test* "pie 3.14" "3.14\n" (with-output-to-string (lambda () (pie 3.141592 2))))

(test* "pie" "-3.14\n"     (with-output-to-string (lambda () (pie -3.141592 2 #\space))))
(test* "pie" " 3.14\n"     (with-output-to-string (lambda () (pie  3.141592 2 #\space))))
(test* "pie" "-3.14\n"     (with-output-to-string (lambda () (pie -3.141592 2 " "))))
(test* "pie" " 3.14\n"     (with-output-to-string (lambda () (pie  3.141592 2 " "))))


(test* "%f" "-3.14"     (%f -3.141592 2))
(test* "%f"  "3.00"     (%f 3.004 2))
(test* "%f"  "3.01"     (%f 3.005000000000001 2))
(test* "%f"  "3.99"     (%f 3.994999999999999 2))
(test* "%f"  "4.00"     (%f 3.995 2))
(test* "%f" "-3.14"     (%f -3.141592 2 #\space))
(test* "%f" " 3.14"     (%f  3.141592 2 #\space))
(test* "%f" "-3.14"     (%f -3.141592 2 " "))
(test* "%f" " 3.14"     (%f  3.141592 2 " "))

(test-section "direct product")

(test* "direct-product" '(())
       (direct-product (lambda x x)))

(test* "direct-product" '((a) (b) (c))
       (direct-product (lambda x x) '(a b c)))

(test* "direct-product" '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2) )
       (direct-product (lambda x x) '(a b c) '(1 2)))

(test* "direct-product" '((a 1 X) (a 2 X) (b 1 X)
                          (b 2 X) (c 1 X) (c 2 X))
       (direct-product (lambda x x) '(a b c) '(1 2) '(X)))

(test* "direct-product" '((a 1 X) (a 1 Y) (a 2 X) (a 2 Y)
                          (b 1 X) (b 1 Y) (b 2 X) (b 2 Y)
                          (c 1 X) (c 1 Y) (c 2 X) (c 2 Y)
                          )
       (direct-product (lambda x x) '(a b c) '(1 2) '(X Y)))

(define (test-dp-for-each sets)
  (let ((lis '()))
    (apply direct-product-for-each
           (lambda x (set! lis (cons x lis)))
           sets)
    (reverse lis)))

(test* "direct-product-for-each" '()
       (test-dp-for-each  '(())))

(test* "direct-product-for-each" '((a) (b) (c))
       (test-dp-for-each '((a b c))))

(test* "direct-product-for-each" '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2) )
       (test-dp-for-each '((a b c) (1 2))))

(test* "direct-product-for-each" '((a 1 X) (a 2 X) (b 1 X)
                                   (b 2 X) (c 1 X) (c 2 X))
       (test-dp-for-each '((a b c) (1 2) (X))))

(test* "direct-product-for-each" '((a 1 X) (a 1 Y) (a 2 X) (a 2 Y)
                                   (b 1 X) (b 1 Y) (b 2 X) (b 2 Y)
                                   (c 1 X) (c 1 Y) (c 2 X) (c 2 Y)
                                   )
       (test-dp-for-each '((a b c) (1 2) (X Y))))

(test-end :exit-on-failure #t)

