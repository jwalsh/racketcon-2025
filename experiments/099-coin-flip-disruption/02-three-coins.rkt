#lang roulette

;; Three Coins Disruption
;; Extended example with three independent coins

(require racket/format)
(provide (all-defined-out))

(module+ main
  (displayln "=== Three Coins Disruption ===\n")

  ;; Three independent fair coins
  (displayln "Initial Setup:")
  (displayln "  Three fair coins (50% heads each)")

  (define coin-1 (flip 0.5))
  (define coin-2 (flip 0.5))
  (define coin-3 (flip 0.5))

  (displayln "\nBefore Observation:")
  (displayln (~a "  P(coin-1 = #t) = " (probability coin-1)))
  (displayln (~a "  P(coin-2 = #t) = " (probability coin-2)))
  (displayln (~a "  P(coin-3 = #t) = " (probability coin-3)))

  ;; All heads
  (define all-heads (and coin-1 coin-2 coin-3))
  (displayln (~a "  P(all heads) = " (probability all-heads)))

  ;; Possible outcomes
  (displayln "\nPossible outcomes: 2³ = 8")
  (displayln "  (H,H,H): P = 1/8 = 0.125 - all heads")
  (displayln "  (H,H,T): P = 1/8 = 0.125")
  (displayln "  (H,T,H): P = 1/8 = 0.125")
  (displayln "  (H,T,T): P = 1/8 = 0.125")
  (displayln "  (T,H,H): P = 1/8 = 0.125")
  (displayln "  (T,H,T): P = 1/8 = 0.125")
  (displayln "  (T,T,H): P = 1/8 = 0.125")
  (displayln "  (T,T,T): P = 1/8 = 0.125")

  ;; Observe: NOT all heads
  (displayln "\n=== Making Observation ===")
  (displayln "Observe: NOT all heads (at least one tails)\n")

  (observe! (not all-heads))

  ;; After observation
  (displayln "After Observation:")
  (displayln (~a "  P(coin-1 = #t) = " (probability coin-1)))
  (displayln (~a "  P(coin-2 = #t) = " (probability coin-2)))
  (displayln (~a "  P(coin-3 = #t) = " (probability coin-3)))
  (displayln (~a "  P(all heads) = " (probability all-heads)))

  (displayln "\nRemaining outcomes: 7 (after eliminating HHH)")
  (displayln "  Each outcome: P = 1/7 ≈ 0.143")

  (displayln "\nP(coin-1 = H) after observation:")
  (displayln "  Count outcomes with coin-1 = H: (H,H,T), (H,T,H), (H,T,T)")
  (displayln "  That's 3 out of 7 outcomes")
  (displayln (~a "  3/7 ≈ " (exact->inexact (/ 3 7))))

  (displayln "\nPattern:")
  (displayln "  Before: P(coin = H) = 1/2 = 0.5")
  (displayln "  After: P(coin = H) = 3/7 ≈ 0.429")
  (displayln "  Observation decreased probability!")

  (newline))

;; ============================================================================
;; At Least One Heads Observation
;; ============================================================================

(module+ at-least-one
  (displayln "\n=== Different Observation: At Least One Heads ===\n")

  (define coin-1 (flip 0.5))
  (define coin-2 (flip 0.5))
  (define coin-3 (flip 0.5))

  (displayln "Observe: At least one heads (not all tails)")

  (define any-heads (or coin-1 coin-2 coin-3))
  (observe! any-heads)

  (displayln "\nAfter Observation:")
  (displayln (~a "  P(coin-1 = #t) = " (probability coin-1)))
  (displayln (~a "  P(coin-2 = #t) = " (probability coin-2)))
  (displayln (~a "  P(coin-3 = #t) = " (probability coin-3)))

  (displayln "\nRemaining outcomes: 7 (after eliminating TTT)")
  (displayln "  Outcomes with coin-1 = H: (H,H,H), (H,H,T), (H,T,H), (H,T,T)")
  (displayln "  That's 4 out of 7 outcomes")
  (displayln (~a "  4/7 ≈ " (exact->inexact (/ 4 7))))

  (displayln "\nPattern:")
  (displayln "  Before: P(coin = H) = 1/2 = 0.5")
  (displayln "  After: P(coin = H) = 4/7 ≈ 0.571")
  (displayln "  Observation INCREASED probability!")

  (newline))

;; ============================================================================
;; Exactly Two Heads
;; ============================================================================

(module+ exactly-two
  (displayln "\n=== Observation: Exactly Two Heads ===\n")

  (define coin-1 (flip 0.5))
  (define coin-2 (flip 0.5))
  (define coin-3 (flip 0.5))

  (define heads-count
    (+ (if coin-1 1 0)
       (if coin-2 1 0)
       (if coin-3 1 0)))

  (displayln "Observe: Exactly two heads")
  (observe! (= heads-count 2))

  (displayln "\nAfter Observation:")
  (displayln (~a "  P(coin-1 = #t) = " (probability coin-1)))
  (displayln (~a "  P(coin-2 = #t) = " (probability coin-2)))
  (displayln (~a "  P(coin-3 = #t) = " (probability coin-3)))

  (displayln "\nRemaining outcomes: 3")
  (displayln "  (H,H,T): coin-1=T count = 2/3")
  (displayln "  (H,T,H): coin-1=H count = 2/3")
  (displayln "  (T,H,H): coin-1=H count = 2/3")

  (displayln (~a "\nP(coin-1 = H) = 2/3 ≈ " (exact->inexact (/ 2 3))))

  (displayln "\nSymmetry:")
  (displayln "  All three coins have same probability!")
  (displayln "  By symmetry, each coin is heads in 2 out of 3 outcomes")

  (newline))

;; ============================================================================
;; Run All
;; ============================================================================

(module+ all
  (require (submod ".." main))
  (require (submod ".." at-least-one))
  (require (submod ".." exactly-two))
  (displayln "=== Three Coins Complete ==="))
