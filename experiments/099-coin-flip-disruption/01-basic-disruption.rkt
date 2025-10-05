#lang roulette

;; Basic Coin Flip Disruption
;; Demonstrates how observe! changes probability distributions

(require racket/format)
(provide (all-defined-out))

;; ============================================================================
;; Basic Example: Two Coins
;; ============================================================================

(module+ main
  (displayln "=== Coin Flip Disruption ===\n")

  ;; Two independent fair coins
  (displayln "Initial Setup:")
  (displayln "  Two fair coins (50% heads each)")
  (define first-coin (flip 0.5))
  (define second-coin (flip 0.5))

  (displayln "\nBefore Observation:")
  (displayln (~a "  P(first-coin = #t) = " (probability first-coin)))
  (displayln (~a "  P(second-coin = #t) = " (probability second-coin)))

  ;; Both heads
  (define both-heads (and first-coin second-coin))
  (displayln (~a "  P(both heads) = " (probability both-heads)))

  ;; Show all outcomes
  (displayln "\nPossible outcomes:")
  (displayln "  (H, H): P = 0.25 - both heads = true")
  (displayln "  (H, T): P = 0.25 - both heads = false")
  (displayln "  (T, H): P = 0.25 - both heads = false")
  (displayln "  (T, T): P = 0.25 - both heads = false")

  ;; Observe: NOT both heads
  (displayln "\n=== Making Observation ===")
  (displayln "Observe: NOT both heads (at least one tails)\n")

  (observe! (not both-heads))

  ;; After observation
  (displayln "After Observation:")
  (displayln (~a "  P(first-coin = #t) = " (probability first-coin)))
  (displayln (~a "  P(second-coin = #t) = " (probability second-coin)))
  (displayln (~a "  P(both heads) = " (probability both-heads)))

  (displayln "\nRemaining outcomes (renormalized):")
  (displayln "  (H, T): P = 1/3 ≈ 0.333")
  (displayln "  (T, H): P = 1/3 ≈ 0.333")
  (displayln "  (T, T): P = 1/3 ≈ 0.333")

  (displayln "\nKey Insight:")
  (displayln "  Probability dropped from 0.5 to 1/3!")
  (displayln "  Observing a condition eliminates impossible outcomes")
  (displayln "  Remaining outcomes are renormalized")

  (newline))

;; ============================================================================
;; Verify with Explicit Enumeration
;; ============================================================================

(module+ verification
  (displayln "\n=== Verification via Enumeration ===\n")

  (displayln "All possible outcomes:")
  (define outcomes
    '((#t #t)   ; H H
      (#t #f)   ; H T
      (#f #t)   ; T H
      (#f #f))) ; T T

  (displayln "\nBefore observation (4 outcomes):")
  (for ([outcome outcomes])
    (define first (car outcome))
    (define second (cadr outcome))
    (define both (and first second))
    (displayln (~a "  " outcome ": P = 0.25, both-heads = " both)))

  (displayln "\nAfter observing (not both-heads) - 3 outcomes remain:")
  (define valid-outcomes
    (filter (lambda (outcome)
              (not (and (car outcome) (cadr outcome))))
            outcomes))

  (displayln (~a "  Valid outcomes: " (length valid-outcomes)))
  (for ([outcome valid-outcomes])
    (displayln (~a "  " outcome ": P = " (exact->inexact (/ 1 3)))))

  (displayln "\nP(first = #t) in valid outcomes:")
  (define first-true-count
    (count (lambda (outcome) (car outcome)) valid-outcomes))
  (displayln (~a "  " first-true-count " out of " (length valid-outcomes)
                " = " (exact->inexact (/ first-true-count (length valid-outcomes)))))

  (newline))

;; ============================================================================
;; Bayesian Calculation
;; ============================================================================

(module+ bayesian
  (displayln "\n=== Bayesian Calculation ===\n")

  (displayln "Using Bayes' Rule:")
  (displayln "P(first=H | not both=H) = P(not both=H | first=H) × P(first=H) / P(not both=H)")

  (define p-first-h 0.5)
  (define p-not-both-given-first-h 0.5)  ; If first=H, second can be T (50%)
  (define p-not-both 0.75)                ; 3 out of 4 outcomes

  (define p-first-given-not-both
    (/ (* p-not-both-given-first-h p-first-h)
       p-not-both))

  (displayln (~a "\nP(first=H) = " p-first-h))
  (displayln (~a "P(not both=H | first=H) = " p-not-both-given-first-h))
  (displayln (~a "P(not both=H) = " p-not-both))
  (displayln (~a "\nP(first=H | not both=H) = "
                (exact->inexact p-first-given-not-both)))

  (displayln "\nThis matches Roulette's inference!")
  (newline))

;; ============================================================================
;; Independence Lost
;; ============================================================================

(module+ independence
  (displayln "\n=== Independence Analysis ===\n")

  ;; Before observation
  (define first-coin (flip 0.5))
  (define second-coin (flip 0.5))

  (displayln "Before observation:")
  (displayln "  first-coin and second-coin are INDEPENDENT")
  (displayln "  P(first=H) = 0.5")
  (displayln "  P(second=H) = 0.5")
  (displayln "  P(first=H, second=H) = 0.5 × 0.5 = 0.25")
  (displayln "  (multiplication works because independent)")

  ;; After observation
  (define both-heads (and first-coin second-coin))
  (observe! (not both-heads))

  (displayln "\nAfter observation:")
  (displayln "  first-coin and second-coin are DEPENDENT!")
  (displayln (~a "  P(first=H) = " (probability first-coin)))
  (displayln (~a "  P(second=H) = " (probability second-coin)))
  (displayln (~a "  P(both=H) = " (probability both-heads)))
  (displayln "  0.333 × 0.333 ≠ 0 (not independent anymore)")

  (displayln "\nKey Insight:")
  (displayln "  Conditioning on joint events creates dependencies!")
  (displayln "  Knowing first-coin tells you about second-coin")
  (displayln "  If first=H, then second MUST be T (given observation)")

  (newline))

;; ============================================================================
;; Run All Modules
;; ============================================================================

(module+ all
  (require (submod ".." main))
  (require (submod ".." verification))
  (require (submod ".." bayesian))
  (require (submod ".." independence))
  (displayln "=== Coin Flip Disruption Complete ==="))
