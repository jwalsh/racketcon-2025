#lang roulette

;; Weightmap for 100d6 Dice Rolls
;; Probability distribution of sum of 100 six-sided dice

(require racket/format)
(provide (all-defined-out))

;; ============================================================================
;; Dice Sum Distribution
;; ============================================================================

;; Sum of n dice rolls
(define (sum-dice n)
  (if (= n 0)
      0
      (+ (discrete-uniform '(1 2 3 4 5 6))
         (sum-dice (- n 1)))))

;; ============================================================================
;; Small Example: 2d6
;; ============================================================================

(module+ small
  (displayln "=== Weightmap: 2d6 ===\n")

  (define sum (sum-dice 2))

  (define dist
    (for/hash ([s (in-range 2 13)])
      (values s (probability (= sum s)))))

  (displayln "Probability of sum for 2d6:")
  (displayln "Sum  P(sum)    Histogram")
  (displayln "---  --------  -----------------------------------------")

  (for ([s (in-range 2 13)])
    (define prob (hash-ref dist s))
    (define bar-length (exact-round (* prob 60)))
    (define bar (make-string bar-length #\█))
    (displayln (~a (~a s #:width 3)
                  " "
                  (~a (~r prob #:precision 6) #:width 9)
                  " "
                  bar)))

  ;; Statistics
  (define mean
    (for/sum ([s (in-range 2 13)])
      (* s (hash-ref dist s))))

  (displayln (~a "\nMean: " (~r mean #:precision 2)))
  (displayln (~a "Expected: " 7.0))
  (newline))

;; ============================================================================
;; Medium Example: 10d6
;; ============================================================================

(module+ medium
  (displayln "=== Weightmap: 10d6 ===\n")

  (define sum (sum-dice 10))

  ;; Range: 10 to 60
  (define dist
    (for/hash ([s (in-range 10 61)])
      (values s (probability (= sum s)))))

  (displayln "Probability of sum for 10d6 (selected values):")
  (displayln "Sum   P(sum)     Histogram")
  (displayln "----  ---------  -----------------------------------------")

  ;; Show every 5th value
  (for ([s (in-range 10 61 5)])
    (define prob (hash-ref dist s))
    (define bar-length (exact-round (* prob 60)))
    (define bar (make-string bar-length #\█))
    (displayln (~a (~a s #:width 4)
                  " "
                  (~a (~r prob #:precision 7) #:width 10)
                  " "
                  bar)))

  ;; Statistics
  (define mean
    (for/sum ([s (in-range 10 61)])
      (* s (hash-ref dist s))))

  (define variance
    (for/sum ([s (in-range 10 61)])
      (* (expt (- s mean) 2) (hash-ref dist s))))

  (displayln (~a "\nMean: " (~r mean #:precision 2)))
  (displayln (~a "Expected: " 35.0))
  (displayln (~a "Variance: " (~r variance #:precision 2)))
  (displayln (~a "Std Dev: " (~r (sqrt variance) #:precision 2)))
  (newline))

;; ============================================================================
;; Large Example: 100d6 (Approximation)
;; ============================================================================

(module+ large
  (displayln "=== Weightmap: 100d6 (Normal Approximation) ===\n")

  ;; For 100d6, use normal approximation (CLT)
  ;; Mean = 100 * 3.5 = 350
  ;; Variance = 100 * (35/12) ≈ 291.67
  ;; Std Dev ≈ 17.08

  (define n 100)
  (define mean 350.0)
  (define variance 291.67)
  (define std-dev 17.08)

  ;; Normal PDF approximation
  (define (normal-pdf x mu sigma)
    (* (/ 1 (* sigma (sqrt (* 2 pi))))
       (exp (- (/ (expt (- x mu) 2)
                  (* 2 (expt sigma 2)))))))

  (displayln "Probability distribution for sum of 100d6:")
  (displayln "Sum   P(sum)     Histogram")
  (displayln "----  ---------  -----------------------------------------")

  ;; Range: 100 to 600, show every 20
  (for ([s (in-range 100 601 20)])
    (define prob (normal-pdf s mean std-dev))
    (define bar-length (exact-round (* prob 50)))
    (define bar (make-string bar-length #\█))
    (displayln (~a (~a s #:width 4)
                  " "
                  (~a (~r prob #:precision 7) #:width 10)
                  " "
                  bar)))

  (displayln (~a "\nTheoretical Statistics:"))
  (displayln (~a "Mean: " mean))
  (displayln (~a "Variance: " variance))
  (displayln (~a "Std Dev: " (~r std-dev #:precision 2)))

  ;; Probability within standard deviations
  (define (normal-cdf-range low high)
    ;; Approximate using numerical integration
    (define steps 1000)
    (define dx (/ (- high low) steps))
    (for/sum ([i steps])
      (define x (+ low (* i dx)))
      (* (normal-pdf x mean std-dev) dx)))

  (displayln (~a "\nProbability Mass Around Mean:"))
  (displayln (~a "P(333 ≤ sum ≤ 367) [μ±1σ] = "
                (~r (normal-cdf-range 333 367) #:precision 4)
                " (~68%)"))
  (displayln (~a "P(316 ≤ sum ≤ 384) [μ±2σ] = "
                (~r (normal-cdf-range 316 384) #:precision 4)
                " (~95%)"))
  (displayln (~a "P(299 ≤ sum ≤ 401) [μ±3σ] = "
                (~r (normal-cdf-range 299 401) #:precision 4)
                " (~99.7%)"))
  (newline))

;; ============================================================================
;; Comparison: Different Numbers of Dice
;; ============================================================================

(module+ comparison
  (displayln "=== Comparison: ndX Distributions ===\n")

  ;; Helper: compute stats for n dice
  (define (dice-stats n)
    (define mean (* n 3.5))
    (define variance (* n (/ 35 12)))
    (define std-dev (sqrt variance))
    (hash 'mean mean 'variance variance 'std-dev std-dev))

  (displayln "Statistics for different numbers of dice:")
  (displayln "n     Mean    Variance  Std Dev  Relative Std%")
  (displayln "----  ------  --------  -------  -------------")

  (for ([n '(1 2 5 10 20 50 100)])
    (define stats (dice-stats n))
    (define mean (hash-ref stats 'mean))
    (define variance (hash-ref stats 'variance))
    (define std-dev (hash-ref stats 'std-dev))
    (define rel-std (* 100 (/ std-dev mean)))

    (displayln (~a (~a n #:width 4)
                  "  "
                  (~a (~r mean #:precision 1) #:width 6)
                  "  "
                  (~a (~r variance #:precision 2) #:width 8)
                  "  "
                  (~a (~r std-dev #:precision 2) #:width 7)
                  "  "
                  (~a (~r rel-std #:precision 2) #:width 6)
                  "%")))

  (displayln "\nNote: Relative std dev decreases with √n (CLT)")
  (newline))

;; ============================================================================
;; Heatmap Visualization
;; ============================================================================

(module+ heatmap
  (displayln "=== 2D Heatmap: 100d6 ===\n")

  (define n 100)
  (define mean 350.0)
  (define std-dev 17.08)

  (define (normal-pdf x mu sigma)
    (* (/ 1 (* sigma (sqrt (* 2 pi))))
       (exp (- (/ (expt (- x mu) 2)
                  (* 2 (expt sigma 2)))))))

  ;; Map probability to character
  (define (prob-to-char prob)
    (cond
      [(< prob 0.002) " "]
      [(< prob 0.005) "░"]
      [(< prob 0.010) "▒"]
      [(< prob 0.015) "▓"]
      [else "█"]))

  (displayln "Heatmap (brightness = probability density):")
  (displayln "")

  ;; Build visualization (100 to 600, every 5)
  (for ([s (in-range 100 601)])
    (define prob (normal-pdf s mean std-dev))
    (define char (prob-to-char prob))

    (when (zero? (modulo (- s 100) 50))
      (display (~a (~a s #:width 3) " ")))

    (when (zero? (modulo (- s 100) 5))
      (display char))

    (when (= (modulo (- s 100 + 5) 50) 0)
      (newline)))

  (displayln "\nLegend:")
  (displayln "  ' ' = P < 0.002")
  (displayln "  '░' = P < 0.005")
  (displayln "  '▒' = P < 0.010")
  (displayln "  '▓' = P < 0.015")
  (displayln "  '█' = P ≥ 0.015")
  (newline))

;; ============================================================================
;; Central Limit Theorem Demonstration
;; ============================================================================

(module+ clt
  (displayln "=== Central Limit Theorem: Sum of Dice ===\n")

  (define (normal-pdf x mu sigma)
    (* (/ 1 (* sigma (sqrt (* 2 pi))))
       (exp (- (/ (expt (- x mu) 2)
                  (* 2 (expt sigma 2)))))))

  (displayln "As n increases, sum becomes more normal:")
  (displayln "")

  (for ([n '(1 5 20 100)])
    (define mean (* n 3.5))
    (define std-dev (sqrt (* n (/ 35 12))))

    (displayln (~a "n=" n " dice (mean=" mean ", σ=" (~r std-dev #:precision 2) ")"))

    ;; Show distribution shape
    (define range-width (* 3 std-dev))
    (define low (- mean range-width))
    (define high (+ mean range-width))

    (for ([x (in-range (exact-round low) (exact-round (+ high 1))
                      (max 1 (exact-round (/ (* 6 std-dev) 40))))])
      (define prob (normal-pdf x mean std-dev))
      (define bar-length (exact-round (* prob (/ 300 (/ 1 std-dev)))))
      (define bar (make-string (min 50 bar-length) #\█))
      (when (> (string-length bar) 0)
        (displayln (~a "  " (~a x #:width 4) " " bar))))

    (newline)))

  (displayln "=== Dice Weightmap Complete ==="))

;; ============================================================================
;; Run All Examples
;; ============================================================================

(module+ all
  (require (submod ".." small))
  (require (submod ".." medium))
  (require (submod ".." large))
  (require (submod ".." comparison))
  (require (submod ".." heatmap))
  (require (submod ".." clt)))
