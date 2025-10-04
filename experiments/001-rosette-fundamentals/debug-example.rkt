#lang rosette

;; Debugging with Rosette

;; Buggy function: intended to return the larger of two numbers
(define (buggy-max a b)
  (if (< a b) a b))  ;; Bug: should return b when a < b

;; Test with symbolic values
(define-symbolic x y integer?)

(displayln "Finding bug in max function:")

;; Try to verify the function works correctly
(define bug-check
  (verify
    (assert
      (and (>= (buggy-max x y) x)
           (>= (buggy-max x y) y)))))

;; This will produce a counterexample showing the bug
(displayln bug-check)

;; Extract counterexample
(displayln "\nCounterexample values:")
(when (sat? bug-check)
  (define model (evaluate x bug-check))
  (displayln (format "x = ~a" model))
  (define model2 (evaluate y bug-check))
  (displayln (format "y = ~a" model2)))

;; Correct version
(define (correct-max a b)
  (if (< a b) b a))

(displayln "\nVerifying corrected max function:")
(define correct-check
  (verify
    (assert
      (and (>= (correct-max x y) x)
           (>= (correct-max x y) y)))))

(displayln correct-check)
