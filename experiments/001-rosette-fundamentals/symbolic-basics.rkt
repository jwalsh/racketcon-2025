#lang rosette

;; Basic symbolic values
(define-symbolic x y z integer?)
(define-symbolic a b boolean?)

;; Display symbolic values
(displayln "Symbolic integers:")
(displayln x)
(displayln y)

;; Assertions
(displayln "\nAssertion example:")
(assert (> x 0))
(displayln "Asserted x > 0")

;; Solving constraints
(displayln "\nSolving for x + y = 10:")
(define sol (solve (assert (= (+ x y) 10))))
(displayln sol)

;; Multiple constraints
(displayln "\nSolving multiple constraints:")
(define sol2
  (solve
    (begin
      (assert (> x 0))
      (assert (< x 10))
      (assert (= (* x 2) y)))))
(displayln sol2)
