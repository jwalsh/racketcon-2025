#lang roulette

;; Weightmap for 100 Coin Flips
;; Exact probability distribution using Roulette

(require racket/format)
(provide (all-defined-out))

;; ============================================================================
;; Coin Flip Distribution
;; ============================================================================

;; Count heads in n flips
(define (count-heads n)
  (if (= n 0)
      0
      (+ (if (flip 0.5) 1 0)
         (count-heads (- n 1)))))

;; ============================================================================
;; Small Example: 10 Flips
;; ============================================================================

(module+ small
  (displayln "=== Weightmap: 10 Coin Flips ===\n")

  ;; Compute distribution
  (define heads (count-heads 10))

  ;; Build distribution over all possible outcomes
  (define dist
    (for/hash ([k (in-range 0 11)])
      (values k (probability (= heads k)))))

  ;; Display distribution
  (displayln "Probability of k heads in 10 flips:")
  (displayln "k    P(k)      Histogram")
  (displayln "---  --------  -----------------------------------------")

  (for ([k (in-range 0 11)])
    (define prob (hash-ref dist k))
    (define bar-length (exact-round (* prob 60)))
    (define bar (make-string bar-length #\█))
    (displayln (~a (~a k #:width 3)
                  " "
                  (~a (~r prob #:precision 6) #:width 9)
                  " "
                  bar)))

  ;; Statistics
  (define mean
    (for/sum ([k (in-range 0 11)])
      (* k (hash-ref dist k))))

  (define variance
    (for/sum ([k (in-range 0 11)])
      (* (expt (- k mean) 2) (hash-ref dist k))))

  (displayln (~a "\nMean: " (~r mean #:precision 2)))
  (displayln (~a "Variance: " (~r variance #:precision 2)))
  (displayln (~a "Std Dev: " (~r (sqrt variance) #:precision 2)))
  (newline))

;; ============================================================================
;; Medium Example: 20 Flips
;; ============================================================================

(module+ medium
  (displayln "=== Weightmap: 20 Coin Flips ===\n")

  (define heads (count-heads 20))

  (define dist
    (for/hash ([k (in-range 0 21)])
      (values k (probability (= heads k)))))

  (displayln "Probability of k heads in 20 flips (selected values):")
  (displayln "k    P(k)      Histogram")
  (displayln "---  --------  -----------------------------------------")

  ;; Show every 2nd value to save space
  (for ([k (in-range 0 21 2)])
    (define prob (hash-ref dist k))
    (define bar-length (exact-round (* prob 60)))
    (define bar (make-string bar-length #\█))
    (displayln (~a (~a k #:width 3)
                  " "
                  (~a (~r prob #:precision 6) #:width 9)
                  " "
                  bar)))

  ;; Show peak
  (define peak-k
    (argmax (lambda (k) (hash-ref dist k)) (range 0 21)))
  (displayln (~a "\nPeak at k=" peak-k
                " with P=" (~r (hash-ref dist peak-k) #:precision 6)))
  (newline))

;; ============================================================================
;; Large Example: 100 Flips (Approximate)
;; ============================================================================

(module+ large
  (displayln "=== Weightmap: 100 Coin Flips (Approximation) ===\n")

  ;; For 100 flips, exact computation is expensive
  ;; Use binomial formula approximation
  (define (binomial-pmf n k p)
    (* (binomial n k)
       (expt p k)
       (expt (- 1 p) (- n k))))

  (define (binomial n k)
    (/ (factorial n)
       (* (factorial k) (factorial (- n k)))))

  (define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))

  ;; Compute distribution using formula (faster than Roulette for n=100)
  (define n 100)
  (define p 0.5)

  (displayln "Probability of k heads in 100 flips (every 5th value):")
  (displayln "k     P(k)       Histogram")
  (displayln "----  ---------  -----------------------------------------")

  (for ([k (in-range 0 101 5)])
    (define prob (binomial-pmf n k p))
    (define bar-length (exact-round (* prob 50)))
    (define bar (make-string bar-length #\█))
    (displayln (~a (~a k #:width 4)
                  " "
                  (~a (~r prob #:precision 7) #:width 10)
                  " "
                  bar)))

  ;; Statistics
  (define mean (* n p))
  (define variance (* n p (- 1 p)))
  (define std-dev (sqrt variance))

  (displayln (~a "\nTheoretical Statistics:"))
  (displayln (~a "Mean: " mean))
  (displayln (~a "Variance: " variance))
  (displayln (~a "Std Dev: " (~r std-dev #:precision 2)))

  ;; Concentration around mean
  (displayln (~a "\nProbability Mass Around Mean:"))
  (define (prob-range low high)
    (for/sum ([k (in-range low (+ high 1))])
      (binomial-pmf n k p)))

  (displayln (~a "P(45 ≤ heads ≤ 55) = " (~r (prob-range 45 55) #:precision 4)))
  (displayln (~a "P(40 ≤ heads ≤ 60) = " (~r (prob-range 40 60) #:precision 4)))
  (displayln (~a "P(35 ≤ heads ≤ 65) = " (~r (prob-range 35 65) #:precision 4)))
  (newline))

;; ============================================================================
;; Heatmap: 2D Visualization
;; ============================================================================

(module+ heatmap
  (displayln "=== 2D Heatmap: 100 Coin Flips ===\n")

  (define (binomial-pmf n k p)
    (* (binomial n k)
       (expt p k)
       (expt (- 1 p) (- n k))))

  (define (binomial n k)
    (/ (factorial n)
       (* (factorial k) (factorial (- n k)))))

  (define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))

  (define n 100)
  (define p 0.5)

  ;; Create heatmap rows (every 10 values)
  (displayln "Heatmap (brightness = probability):")
  (displayln "")

  ;; Map probability to character
  (define (prob-to-char prob)
    (cond
      [(< prob 0.001) " "]
      [(< prob 0.005) "░"]
      [(< prob 0.01) "▒"]
      [(< prob 0.02) "▓"]
      [(< prob 0.04) "█"]
      [else "█"]))

  ;; Build visualization
  (for ([k (in-range 0 101)])
    (define prob (binomial-pmf n k p))
    (define char (prob-to-char prob))
    (when (zero? (modulo k 10))
      (display (~a (~a k #:width 3) " ")))
    (when (zero? (modulo k 1))
      (display char))
    (when (= (modulo (+ k 1) 10) 0)
      (newline)))

  (displayln "\nLegend:")
  (displayln "  ' ' = P < 0.001")
  (displayln "  '░' = P < 0.005")
  (displayln "  '▒' = P < 0.01")
  (displayln "  '▓' = P < 0.02")
  (displayln "  '█' = P ≥ 0.04")
  (newline))

;; ============================================================================
;; Cumulative Distribution
;; ============================================================================

(module+ cumulative
  (displayln "=== Cumulative Distribution: 100 Coin Flips ===\n")

  (define (binomial-pmf n k p)
    (* (binomial n k)
       (expt p k)
       (expt (- 1 p) (- n k))))

  (define (binomial n k)
    (/ (factorial n)
       (* (factorial k) (factorial (- n k)))))

  (define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))

  (define n 100)
  (define p 0.5)

  (displayln "Cumulative probability P(heads ≤ k):")
  (displayln "k     P(≤k)     Histogram")
  (displayln "----  --------  -----------------------------------------")

  (define cumulative 0.0)

  (for ([k (in-range 0 101 5)])
    ;; Add probabilities from last checkpoint to current
    (for ([j (in-range (- k 5) k)])
      (when (>= j 0)
        (set! cumulative (+ cumulative (binomial-pmf n j p)))))

    (define bar-length (exact-round (* cumulative 50)))
    (define bar (make-string bar-length #\█))
    (displayln (~a (~a k #:width 4)
                  " "
                  (~a (~r cumulative #:precision 6) #:width 9)
                  " "
                  bar)))

  (displayln "\n=== Coin Flip Weightmap Complete ==="))

;; ============================================================================
;; Run All Examples
;; ============================================================================

(module+ all
  (require (submod ".." small))
  (require (submod ".." medium))
  (require (submod ".." large))
  (require (submod ".." heatmap))
  (require (submod ".." cumulative)))
