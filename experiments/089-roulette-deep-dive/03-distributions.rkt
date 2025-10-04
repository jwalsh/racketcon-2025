#lang roulette

;; Computing and displaying probability distributions with Roulette
;; RacketCon 2025 - Experiment 089

(require racket/format)
(provide (all-defined-out))

;; ============================================================================
;; Distribution Helpers
;; ============================================================================

;; Compute probability distribution over all possible outcomes
(define (distribution-of expr possible-values)
  (for/hash ([val possible-values])
    (values val (probability (equal? expr val)))))

;; Pretty print a distribution
(define (display-distribution dist [label "Distribution"])
  (displayln (~a label ":"))
  (for ([(outcome prob) (in-hash dist)])
    (displayln (~a "  " outcome " → " (~r prob #:precision 4))))
  (newline))

;; ============================================================================
;; Example 1: Die Roll Distribution
;; ============================================================================

(module+ main
  (displayln "=== Probability Distributions in Roulette ===\n")

  (displayln "Example 1: Single die roll distribution")
  (define die (discrete-uniform '(1 2 3 4 5 6)))

  (define die-dist
    (distribution-of die '(1 2 3 4 5 6)))

  (display-distribution die-dist "Single Die")

  ;; Example 2: Sum of two dice
  (displayln "Example 2: Sum of two dice distribution")
  (define (two-dice-sum)
    (define d1 (discrete-uniform '(1 2 3 4 5 6)))
    (define d2 (discrete-uniform '(1 2 3 4 5 6)))
    (+ d1 d2))

  (define sum-dist
    (distribution-of (two-dice-sum) (range 2 13)))

  (display-distribution sum-dist "Two Dice Sum")

  ;; Visualize with histogram
  (displayln "Histogram of two dice sum:")
  (for ([sum (range 2 13)])
    (define prob (hash-ref sum-dist sum))
    (define bar-length (exact-round (* prob 60)))
    (displayln (~a (~a sum #:width 3 #:align 'right) " "
                  (~a (~r prob #:precision 4) #:width 8)
                  " " (make-string bar-length #\█))))
  (newline))

;; ============================================================================
;; Example 3: Coin Flip Distributions
;; ============================================================================

(module+ coins
  (displayln "=== Coin Flip Distributions ===\n")

  ;; Example 3: Count heads in n flips
  (displayln "Example 3: Number of heads in 5 flips")
  (define (count-heads n)
    (if (= n 0)
        0
        (+ (if (flip 0.5) 1 0)
           (count-heads (- n 1)))))

  (define heads-dist
    (distribution-of (count-heads 5) (range 0 6)))

  (display-distribution heads-dist "Heads in 5 Flips")

  ;; Show as binomial distribution
  (displayln "Binomial Distribution (n=5, p=0.5):")
  (for ([k (range 0 6)])
    (define prob (hash-ref heads-dist k))
    (define expected (/ (factorial 5)
                       (* (factorial k)
                          (factorial (- 5 k))))
                     (expt 2 5))
    (displayln (~a "  P(X=" k ") = "
                  (~r prob #:precision 4)
                  " (expected: " (~r expected #:precision 4) ")")))
  (newline))

;; ============================================================================
;; Example 4: Conditional Distributions
;; ============================================================================

(module+ conditional
  (displayln "=== Conditional Distributions ===\n")

  ;; Example 4: Two dice, conditioned on sum
  (displayln "Example 4: Die values given sum = 7")
  (define (dice-given-sum-7)
    (define d1 (discrete-uniform '(1 2 3 4 5 6)))
    (define d2 (discrete-uniform '(1 2 3 4 5 6)))
    (observe (= (+ d1 d2) 7))
    (list d1 d2))

  ;; All possible (d1, d2) pairs that sum to 7
  (define pairs-sum-7 '((1 6) (2 5) (3 4) (4 3) (5 2) (6 1)))

  (define conditional-dist
    (distribution-of (dice-given-sum-7) pairs-sum-7))

  (display-distribution conditional-dist "Dice Values | Sum = 7")

  ;; Example 5: Medical test posterior
  (displayln "Example 5: Disease probability given test result")
  (define (disease-given-test)
    (define has-disease (flip 0.01))  ; 1% prior
    (define test-positive
      (if has-disease
          (flip 0.99)  ; 99% sensitivity
          (flip 0.05))) ; 5% false positive

    (observe test-positive)
    has-disease)

  (define posterior-dist
    (distribution-of (disease-given-test) '(#t #f)))

  (display-distribution posterior-dist "Has Disease | Test Positive")
  (newline))

;; ============================================================================
;; Example 6: Complex Distributions
;; ============================================================================

(module+ complex
  (displayln "=== Complex Distributions ===\n")

  ;; Example 6: Random walk after n steps
  (displayln "Example 6: Position after 5-step random walk")
  (define (random-walk n pos)
    (if (= n 0)
        pos
        (random-walk (- n 1)
                    (+ pos (if (flip 0.5) 1 -1)))))

  (define walk-dist
    (distribution-of (random-walk 5 0) (range -5 6)))

  (display-distribution walk-dist "Position After 5 Steps")

  ;; Visualize
  (displayln "Position histogram:")
  (for ([pos (sort (hash-keys walk-dist) <)])
    (define prob (hash-ref walk-dist pos))
    (when (> prob 0)
      (define bar-length (exact-round (* prob 40)))
      (displayln (~a (~a pos #:width 3 #:align 'right) " "
                    (~a (~r prob #:precision 4) #:width 8)
                    " " (make-string bar-length #\█)))))
  (newline)

  ;; Example 7: Urn problem posterior
  (displayln "Example 7: Urn selection given ball color")
  (define (urn-given-red)
    ;; Pick an urn
    (define urn (discrete-uniform '(A B C)))

    ;; Different color distributions
    (define ball
      (case urn
        [(A) (discrete-uniform '(red red red blue))]
        [(B) (discrete-uniform '(red red blue blue))]
        [(C) (discrete-uniform '(red blue blue blue))]))

    ;; Observe: drew red ball
    (observe (equal? ball 'red))
    urn)

  (define urn-dist
    (distribution-of (urn-given-red) '(A B C)))

  (display-distribution urn-dist "Urn | Red Ball Drawn")
  (newline))

;; ============================================================================
;; Example 8: Joint Distributions
;; ============================================================================

(module+ joint
  (displayln "=== Joint Distributions ===\n")

  ;; Example 8: Joint distribution of two correlated variables
  (displayln "Example 8: Correlated coin flips")
  (define (correlated-flips)
    (define x (flip 0.5))
    (define y (if x (flip 0.8) (flip 0.2)))
    (list x y))

  (define joint-dist
    (distribution-of (correlated-flips)
                    '((#t #t) (#t #f) (#f #t) (#f #f))))

  (display-distribution joint-dist "Joint P(X,Y)")

  ;; Compute marginals
  (displayln "Marginal P(X):")
  (displayln (~a "  P(X=#t) = "
                (~r (+ (hash-ref joint-dist '(#t #t))
                      (hash-ref joint-dist '(#t #f)))
                    #:precision 4)))
  (displayln (~a "  P(X=#f) = "
                (~r (+ (hash-ref joint-dist '(#f #t))
                      (hash-ref joint-dist '(#f #f)))
                    #:precision 4)))
  (newline))

;; ============================================================================
;; Example 9: Distribution Statistics
;; ============================================================================

(module+ statistics
  (displayln "=== Distribution Statistics ===\n")

  ;; Compute expected value
  (define (expected-value dist)
    (for/sum ([(outcome prob) (in-hash dist)])
      (* outcome prob)))

  ;; Compute variance
  (define (variance dist)
    (define mean (expected-value dist))
    (for/sum ([(outcome prob) (in-hash dist)])
      (* prob (expt (- outcome mean) 2))))

  ;; Example 9: Statistics of die roll
  (displayln "Example 9: Die roll statistics")
  (define die (discrete-uniform '(1 2 3 4 5 6)))
  (define die-dist (distribution-of die '(1 2 3 4 5 6)))

  (displayln (~a "Expected value: " (~r (expected-value die-dist) #:precision 4)))
  (displayln (~a "Variance: " (~r (variance die-dist) #:precision 4)))
  (displayln (~a "Std deviation: " (~r (sqrt (variance die-dist)) #:precision 4)))
  (newline)

  ;; Example 10: Sum of two dice statistics
  (displayln "Example 10: Sum of two dice statistics")
  (define (two-dice) (+ (discrete-uniform '(1 2 3 4 5 6))
                        (discrete-uniform '(1 2 3 4 5 6))))
  (define sum-dist (distribution-of (two-dice) (range 2 13)))

  (displayln (~a "Expected value: " (~r (expected-value sum-dist) #:precision 4)))
  (displayln (~a "Variance: " (~r (variance sum-dist) #:precision 4)))
  (displayln (~a "Std deviation: " (~r (sqrt (variance sum-dist)) #:precision 4)))
  (newline))

;; ============================================================================
;; Example 11: Entropy
;; ============================================================================

(module+ entropy
  (displayln "=== Information Theory ===\n")

  ;; Compute Shannon entropy
  (define (entropy dist)
    (- (for/sum ([(outcome prob) (in-hash dist)])
         (if (= prob 0)
             0
             (* prob (log prob 2))))))

  ;; Example 11: Compare entropies
  (displayln "Example 11: Entropy comparison")

  (define fair-coin (flip 0.5))
  (define fair-dist (distribution-of fair-coin '(#t #f)))
  (displayln (~a "Fair coin entropy: "
                (~r (entropy fair-dist) #:precision 4) " bits"))

  (define biased-coin (flip 0.9))
  (define biased-dist (distribution-of biased-coin '(#t #f)))
  (displayln (~a "Biased coin (90%) entropy: "
                (~r (entropy biased-dist) #:precision 4) " bits"))

  (define die (discrete-uniform '(1 2 3 4 5 6)))
  (define die-dist (distribution-of die '(1 2 3 4 5 6)))
  (displayln (~a "Fair die entropy: "
                (~r (entropy die-dist) #:precision 4) " bits"))
  (newline)

  (displayln "=== Distribution Analysis Complete ==="))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; ============================================================================
;; All Modules
;; ============================================================================

(module+ all
  (require (submod ".." main))
  (require (submod ".." coins))
  (require (submod ".." conditional))
  (require (submod ".." complex))
  (require (submod ".." joint))
  (require (submod ".." statistics))
  (require (submod ".." entropy)))
