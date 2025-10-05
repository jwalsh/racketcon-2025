#lang roulette/example/disrupt

;; Basic Coin Flip Disruption
;; Demonstrates how observe! changes probability distributions
;; Based on: https://docs.racket-lang.org/roulette/Examples.html

;; ============================================================================
;; Basic Example: Two Coins
;; ============================================================================

;; Two independent fair coins
(define first-coin (flip 0.5))
(define second-coin (flip 0.5))

;; Conjunction: both heads
(define both-heads (and first-coin second-coin))

;; Observe: NOT both heads (at least one tails)
(observe! (not both-heads))

;; Query the updated probabilities
;; In roulette/example/disrupt, just evaluating returns the PMF

(displayln "=== Coin Flip Disruption ===\n")
(displayln "After observing (not both-heads):\n")

(displayln "first-coin:")
(displayln first-coin)
(displayln "")

(displayln "second-coin:")
(displayln second-coin)
(displayln "")

(displayln "both-heads:")
(displayln both-heads)
(displayln "")

(displayln "Explanation:")
(displayln "  Before observation: P(coin = #t) = 0.5")
(displayln "  After observation: P(coin = #t) = 0.333...")
(displayln "  The (H,H) outcome was eliminated, leaving 3 equally likely outcomes:")
(displayln "    (H,T), (T,H), (T,T)")
(displayln "  First coin is #t in only 1 of these 3 cases: (H,T)")
(displayln "  Therefore P(first = #t) = 1/3 ≈ 0.333")
