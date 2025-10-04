#lang racket

;; Comparison: Probabilistic microKanren vs Roulette
;; Side-by-side implementations

(require racklog)
(provide (all-defined-out))

;; ============================================================================
;; Example 1: Coin Flip
;; ============================================================================

(module+ coin
  (displayln "=== Coin Flip Comparison ===\n")

  ;; In probabilistic microKanren (conceptual):
  (displayln "Probabilistic microKanren:")
  (displayln "(run* (q)")
  (displayln "  (conde")
  (displayln "    [(flip 0.5) (≡ q 'heads)]")
  (displayln "    [(≡ q 'tails)]))")
  (displayln "→ [(heads . 0.5) (tails . 0.5)]\n")

  ;; In Roulette:
  (displayln "Roulette (#lang roulette):")
  (displayln "(if (flip 0.5) 'heads 'tails)")
  (displayln "→ 'heads with P=0.5, 'tails with P=0.5\n")

  ;; In Racklog (non-probabilistic):
  (displayln "Racklog (deterministic logic):")
  (displayln "(%which (q)")
  (displayln "  (%or")
  (displayln "    (%= q 'heads)")
  (displayln "    (%= q 'tails)))")
  (displayln "→ 'heads (first solution only)\n")
  (newline))

;; ============================================================================
;; Example 2: Dice Roll
;; ============================================================================

(module+ dice
  (displayln "=== Dice Roll Comparison ===\n")

  ;; Probabilistic microKanren
  (displayln "Probabilistic microKanren:")
  (displayln "(run* (q)")
  (displayln "  (conde")
  (displayln "    [(≡ q 1)] [(≡ q 2)] [(≡ q 3)]")
  (displayln "    [(≡ q 4)] [(≡ q 5)] [(≡ q 6)]))")
  (displayln "→ Uniform distribution over {1,2,3,4,5,6}\n")

  ;; Roulette
  (displayln "Roulette:")
  (displayln "(discrete-uniform '(1 2 3 4 5 6))")
  (displayln "→ Each outcome: P = 1/6\n")

  ;; Using Racklog
  (displayln "Racklog:")
  (%let (q)
    (displayln (%which (q)
                 (%member q '(1 2 3 4 5 6)))))
  (displayln "→ 1 (first solution)\n")
  (newline))

;; ============================================================================
;; Example 3: Conditional Probability
;; ============================================================================

(module+ conditional
  (displayln "=== Conditional Probability ===\n")

  ;; Probabilistic microKanren
  (displayln "Probabilistic microKanren:")
  (displayln "(run* (temp)")
  (displayln "  (fresh (weather)")
  (displayln "    (conde")
  (displayln "      [(≡ weather 'sunny)  ; 60%")
  (displayln "       (conde")
  (displayln "         [(≡ temp 'warm)]   ; 80% if sunny")
  (displayln "         [(≡ temp 'cool)])] ; 20% if sunny")
  (displayln "      [(≡ weather 'rainy)  ; 40%")
  (displayln "       (conde")
  (displayln "         [(≡ temp 'warm)]   ; 30% if rainy")
  (displayln "         [(≡ temp 'cool)])])))")
  (displayln "\nDistribution:")
  (displayln "  warm: 0.6×0.8 + 0.4×0.3 = 0.60")
  (displayln "  cool: 0.6×0.2 + 0.4×0.7 = 0.40\n")

  ;; Roulette
  (displayln "Roulette:")
  (displayln "(define weather (if (flip 0.6) 'sunny 'rainy))")
  (displayln "(if (eq? weather 'sunny)")
  (displayln "    (if (flip 0.8) 'warm 'cool)")
  (displayln "    (if (flip 0.3) 'warm 'cool))")
  (displayln "\nAutomatically computes exact distribution\n")
  (newline))

;; ============================================================================
;; Example 4: Bayesian Inference
;; ============================================================================

(module+ bayesian
  (displayln "=== Bayesian Inference ===\n")

  ;; Problem: Medical test
  (displayln "Problem: Medical diagnosis")
  (displayln "  P(disease) = 0.01")
  (displayln "  P(positive | disease) = 0.95")
  (displayln "  P(positive | healthy) = 0.05")
  (displayln "  Query: P(disease | positive)?\n")

  ;; Probabilistic microKanren approach
  (displayln "Probabilistic microKanren:")
  (displayln "(run* (disease)")
  (displayln "  (fresh (test-result)")
  (displayln "    (conde")
  (displayln "      [(≡ disease 'yes)  ; Prior: 1%")
  (displayln "       (conde")
  (displayln "         [(≡ test-result 'pos)]  ; 95%")
  (displayln "         [(≡ test-result 'neg)])]")
  (displayln "      [(≡ disease 'no)   ; Prior: 99%")
  (displayln "       (conde")
  (displayln "         [(≡ test-result 'pos)]  ; 5%")
  (displayln "         [(≡ test-result 'neg)])])")
  (displayln "    ;; Condition on positive test")
  (displayln "    (≡ test-result 'pos)))")
  (displayln "\nAfter conditioning:")
  (displayln "  P(disease=yes | pos) ≈ 0.161\n")

  ;; Roulette approach
  (displayln "Roulette:")
  (displayln "(define disease (flip 0.01))")
  (displayln "(define test-positive")
  (displayln "  (if disease")
  (displayln "      (flip 0.95)")
  (displayln "      (flip 0.05)))")
  (displayln "(probability (and disease test-positive))")
  (displayln "→ Exact inference via RSDD\n")
  (newline))

;; ============================================================================
;; Comparison Table
;; ============================================================================

(module+ comparison-table
  (displayln "=== Feature Comparison ===\n")

  (displayln "| Feature              | μKanren+Prob | Roulette        | Racklog       |")
  (displayln "|----------------------|--------------|-----------------|---------------|")
  (displayln "| *Paradigm*           | Logic        | Probabilistic   | Logic         |")
  (displayln "| *Probability*        | Weighted     | Native          | None          |")
  (displayln "| *Inference*          | Search       | RSDD (exact)    | Backtracking  |")
  (displayln "| *Conditioning*       | Manual       | Automatic       | N/A           |")
  (displayln "| *Unification*        | Full         | No              | Full          |")
  (displayln "| *Relations*          | Yes          | Limited         | Yes           |")
  (displayln "| *Size*               | ~100 LOC     | Full #lang      | Library       |")
  (displayln "| *Exact inference*    | No           | Yes (discrete)  | N/A           |")
  (displayln "| *Distribution query* | Manual       | probability     | N/A           |")
  (newline))

;; ============================================================================
;; Strengths and Weaknesses
;; ============================================================================

(module+ strengths
  (displayln "=== Strengths and Use Cases ===\n")

  (displayln "Probabilistic microKanren:")
  (displayln "  ✓ Relational programming with uncertainty")
  (displayln "  ✓ Unification-based constraints")
  (displayln "  ✓ Minimal implementation")
  (displayln "  ✓ Educational value")
  (displayln "  ✗ Manual probability tracking")
  (displayln "  ✗ No automatic inference")
  (displayln "  ✗ Approximate for large spaces\n")

  (displayln "Roulette:")
  (displayln "  ✓ Exact discrete inference")
  (displayln "  ✓ RSDD for efficient computation")
  (displayln "  ✓ Automatic probability calculation")
  (displayln "  ✓ Clean probabilistic primitives")
  (displayln "  ✗ No unification")
  (displayln "  ✗ No relational programming")
  (displayln "  ✗ Discrete only\n")

  (displayln "Racklog:")
  (displayln "  ✓ Full Prolog-style logic programming")
  (displayln "  ✓ Unification and relations")
  (displayln "  ✓ Racket integration")
  (displayln "  ✗ No probabilistic reasoning")
  (displayln "  ✗ Deterministic only\n")

  (displayln "Ideal: Combine strengths!")
  (displayln "  • Unification from microKanren/Racklog")
  (displayln "  • Exact inference from Roulette")
  (displayln "  • Probabilistic relations\n")
  (newline))

;; ============================================================================
;; Hybrid Example
;; ============================================================================

(module+ hybrid
  (displayln "=== Hybrid Approach (Conceptual) ===\n")

  (displayln "Goal: Probabilistic logic programming with exact inference\n")

  (displayln "Example: Probabilistic type checking")
  (displayln "")
  (displayln "(define-prob-relation (typeof expr type)")
  (displayln "  ;; Integer literal: certain")
  (displayln "  [(fresh (n)")
  (displayln "     (≡ expr `(int ,n))")
  (displayln "     (≡ type 'int))")
  (displayln "   1.0]")
  (displayln "")
  (displayln "  ;; Addition: probably int")
  (displayln "  [(fresh (e1 e2)")
  (displayln "     (≡ expr `(+ ,e1 ,e2))")
  (displayln "     (typeof e1 'int)")
  (displayln "     (typeof e2 'int)")
  (displayln "     (≡ type 'int))")
  (displayln "   0.9]")
  (displayln "")
  (displayln "  ;; Polymorphic fallback")
  (displayln "  [(≡ type 'any)")
  (displayln "   0.1])")
  (displayln "")
  (displayln "Query:")
  (displayln "  (prob-query (t) (typeof '(+ (int 5) (int 3)) t))")
  (displayln "")
  (displayln "→ {(int . 0.9), (any . 0.1)}")
  (displayln "")
  (displayln "This combines:")
  (displayln "  • Unification for pattern matching")
  (displayln "  • Relations for recursive rules")
  (displayln "  • Probabilities for uncertainty")
  (displayln "  • Exact inference for queries\n")
  (newline))

;; ============================================================================
;; Run All Comparisons
;; ============================================================================

(module+ all
  (require (submod ".." coin))
  (require (submod ".." dice))
  (require (submod ".." conditional))
  (require (submod ".." bayesian))
  (require (submod ".." comparison-table))
  (require (submod ".." strengths))
  (require (submod ".." hybrid))
  (displayln "=== Comparison Complete ==="))
