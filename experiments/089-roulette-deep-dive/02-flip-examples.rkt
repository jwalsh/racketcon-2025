#lang roulette

;; Flip examples - probabilistic coin flips with Roulette
;; RacketCon 2025 - Experiment 089

(provide (all-defined-out))

;; ============================================================================
;; Basic Flip Operations
;; ============================================================================

(module+ main
  (displayln "=== Roulette Flip Examples ===\n")

  ;; Example 1: Basic flip with probability 0.5
  (displayln "Example 1: Fair coin flip")
  (define x (flip 0.5))
  (displayln (~a "x = " x))
  (displayln (~a "P(x = #t) = " (probability (equal? x #t))))
  (displayln (~a "P(x = #f) = " (probability (equal? x #f))))
  (newline)

  ;; Example 2: Biased flip
  (displayln "Example 2: Biased flip (70% true)")
  (define y (flip 0.7))
  (displayln (~a "P(y = #t) = " (probability (equal? y #t))))
  (displayln (~a "P(y = #f) = " (probability (equal? y #f))))
  (newline)

  ;; Example 3: Multiple flips
  (displayln "Example 3: Two independent flips")
  (define flip1 (flip 0.5))
  (define flip2 (flip 0.5))
  (displayln (~a "P(both true) = "
                (probability (and flip1 flip2))))
  (displayln (~a "P(at least one true) = "
                (probability (or flip1 flip2))))
  (displayln (~a "P(exactly one true) = "
                (probability (and (or flip1 flip2)
                                 (not (and flip1 flip2))))))
  (newline))

;; ============================================================================
;; Flip with Conditioning
;; ============================================================================

(module+ conditioning
  (displayln "=== Flip with Conditioning ===\n")

  ;; Example 4: Conditional probability
  (displayln "Example 4: Two flips, observe at least one heads")
  (define (two-flips-conditioned)
    (define f1 (flip 0.5))
    (define f2 (flip 0.5))

    ;; Observe: at least one is true (heads)
    (observe (or f1 f2))

    ;; Return both
    (list f1 f2))

  (displayln (~a "P(both heads | at least one heads) = "
                (probability (equal? (two-flips-conditioned) '(#t #t)))))
  (displayln (~a "P(first only | at least one heads) = "
                (probability (equal? (two-flips-conditioned) '(#t #f)))))
  (newline)

  ;; Example 5: Three flips with constraint
  (displayln "Example 5: Three flips, exactly two heads")
  (define (three-flips-two-heads)
    (define f1 (flip 0.5))
    (define f2 (flip 0.5))
    (define f3 (flip 0.5))

    ;; Observe: exactly two are true
    (observe (= (+ (if f1 1 0) (if f2 1 0) (if f3 1 0)) 2))

    ;; Return configuration
    (list f1 f2 f3))

  (displayln (~a "P(HHT | exactly 2 heads) = "
                (probability (equal? (three-flips-two-heads) '(#t #t #f)))))
  (displayln (~a "P(HTH | exactly 2 heads) = "
                (probability (equal? (three-flips-two-heads) '(#t #f #t)))))
  (newline))

;; ============================================================================
;; Practical Examples with Flip
;; ============================================================================

(module+ practical
  (displayln "=== Practical Flip Examples ===\n")

  ;; Example 6: Noisy communication channel
  (displayln "Example 6: Noisy channel (10% bit flip rate)")
  (define (noisy-channel bit)
    (define flipped (flip 0.1))  ; 10% chance of error
    (if flipped (not bit) bit))

  (define sent #t)
  (define received (noisy-channel sent))
  (displayln (~a "P(received correctly | sent #t) = "
                (probability (equal? received sent))))
  (newline)

  ;; Example 7: Majority voting
  (displayln "Example 7: Majority voting with noisy voters")
  (define (noisy-voter correct-answer)
    ;; Voter is correct 80% of the time
    (define is-correct (flip 0.8))
    (if is-correct correct-answer (not correct-answer)))

  (define (majority-vote answer)
    (define v1 (noisy-voter answer))
    (define v2 (noisy-voter answer))
    (define v3 (noisy-voter answer))

    ;; Return majority
    (>= (+ (if v1 1 0) (if v2 1 0) (if v3 1 0)) 2))

  (displayln (~a "P(majority correct | answer = #t) = "
                (probability (equal? (majority-vote #t) #t))))
  (newline)

  ;; Example 8: Random walk
  (displayln "Example 8: Random walk (3 steps)")
  (define (random-walk n pos)
    (if (= n 0)
        pos
        (let ([step (if (flip 0.5) 1 -1)])
          (random-walk (- n 1) (+ pos step)))))

  (displayln (~a "P(end at position 1 after 3 steps) = "
                (probability (= (random-walk 3 0) 1))))
  (displayln (~a "P(end at position -3 after 3 steps) = "
                (probability (= (random-walk 3 0) -3))))
  (newline))

;; ============================================================================
;; Flip vs Other Probability Primitives
;; ============================================================================

(module+ comparison
  (displayln "=== Flip vs Bernoulli vs Disrupt ===\n")

  ;; All equivalent for binary outcomes
  (displayln "Example 9: Three ways to express coin flip")

  ;; Method 1: flip
  (define x1 (flip 0.7))
  (displayln (~a "flip: P(#t) = " (probability x1)))

  ;; Method 2: bernoulli
  (define x2 (bernoulli 0.7))
  (displayln (~a "bernoulli: P(#t) = " (probability x2)))

  ;; Method 3: disrupt
  (define x3 (disrupt [(0.7) #t] [(0.3) #f]))
  (displayln (~a "disrupt: P(#t) = " (probability x3)))

  (displayln "All three are equivalent!")
  (newline))

;; ============================================================================
;; Advanced: Flip Sequences
;; ============================================================================

(module+ sequences
  (displayln "=== Flip Sequences ===\n")

  ;; Example 10: Count heads in n flips
  (displayln "Example 10: Expected number of heads in 10 flips")
  (define (count-heads n)
    (if (= n 0)
        0
        (+ (if (flip 0.5) 1 0)
           (count-heads (- n 1)))))

  (define heads (count-heads 10))
  (displayln (~a "P(exactly 5 heads) = "
                (probability (= heads 5))))
  (displayln (~a "P(at least 7 heads) = "
                (probability (>= heads 7))))
  (newline)

  ;; Example 11: Geometric distribution (flips until heads)
  (displayln "Example 11: Flips until first heads")
  (define (flips-until-heads count)
    (define result (flip 0.5))
    (if result
        count
        (flips-until-heads (+ count 1))))

  ;; Note: This might not terminate in symbolic execution!
  ;; Better to bound it:
  (define (bounded-flips-until-heads max-flips count)
    (if (= count max-flips)
        max-flips
        (let ([result (flip 0.5)])
          (if result
              count
              (bounded-flips-until-heads max-flips (+ count 1))))))

  (define flips-needed (bounded-flips-until-heads 5 1))
  (displayln (~a "P(first heads on flip 1) = "
                (probability (= flips-needed 1))))
  (displayln (~a "P(first heads on flip 2) = "
                (probability (= flips-needed 2))))
  (newline))

;; ============================================================================
;; Conditional Flips
;; ============================================================================

(module+ conditional-flips
  (displayln "=== Conditional Flips ===\n")

  ;; Example 12: Flip probability depends on previous outcome
  (displayln "Example 12: Dependent flips")
  (define (dependent-flips)
    (define first (flip 0.5))

    ;; Second flip probability depends on first
    (define second
      (if first
          (flip 0.8)  ; 80% if first was true
          (flip 0.2))) ; 20% if first was false

    (list first second))

  (displayln (~a "P(both true) = "
                (probability (equal? (dependent-flips) '(#t #t)))))
  (displayln (~a "P(first true, second false) = "
                (probability (equal? (dependent-flips) '(#t #f)))))
  (newline)

  (displayln "=== Flip Examples Complete ==="))

;; ============================================================================
;; Tests
;; ============================================================================

(module+ test
  (require rackunit)

  (test-case "Basic flip probabilities"
    ;; Fair coin
    (define fair (flip 0.5))
    (check-equal? (probability fair) 0.5)

    ;; Biased coin
    (define biased (flip 0.7))
    (check-equal? (probability biased) 0.7))

  (test-case "Flip combinations"
    (define f1 (flip 0.5))
    (define f2 (flip 0.5))

    ;; Independent flips
    (check-equal? (probability (and f1 f2)) 0.25)
    (check-equal? (probability (or f1 f2)) 0.75))

  (test-case "Conditional flips"
    (define (conditional)
      (define x (flip 0.5))
      (observe x)
      x)

    ;; Given observation, must be true
    (check-equal? (probability (conditional)) 1.0)))
