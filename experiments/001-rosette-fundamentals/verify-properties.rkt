#lang rosette

;; Verification examples

(define-symbolic x y z integer?)

;; 1. Verify commutativity of addition
(displayln "Verifying commutativity of addition:")
(define result1
  (verify (assert (= (+ x y) (+ y x)))))
(displayln result1)

;; 2. Verify associativity of addition
(displayln "\nVerifying associativity of addition:")
(define result2
  (verify (assert (= (+ (+ x y) z) (+ x (+ y z))))))
(displayln result2)

;; 3. Verify a false property (will find counterexample)
(displayln "\nTrying to verify subtraction is commutative (should fail):")
(define result3
  (verify (assert (= (- x y) (- y x)))))
(displayln result3)

;; 4. Verify simple function correctness
(define (abs-val x)
  (if (< x 0) (- x) x))

(displayln "\nVerifying absolute value is non-negative:")
(define result4
  (verify (assert (>= (abs-val x) 0))))
(displayln result4)

;; 5. Verify max function
(define (max2 a b)
  (if (> a b) a b))

(displayln "\nVerifying max returns one of its arguments:")
(define result5
  (verify
    (assert
      (or (= (max2 x y) x)
          (= (max2 x y) y)))))
(displayln result5)
