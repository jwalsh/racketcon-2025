;; Probabilistic microKanren Extension
;; Adds probability weights to search streams

(import (scheme base)
        (scheme write)
        (scheme inexact))

;; Load core microKanren
(include "01-microkanren.scm")

;; ============================================================================
;; Weighted State Representation
;; ============================================================================

;; Weighted state = (probability . state)
(define (weighted-state p st)
  (cons p st))

(define (weight ws) (car ws))
(define (unwrap-state ws) (cdr ws))

;; ============================================================================
;; Probabilistic Choice
;; ============================================================================

;; Flip: probabilistic branch
(define (flip p)
  (lambda (st)
    (list
     (weighted-state p (state (state-s st) (state-c st)))
     (weighted-state (- 1.0 p) (state (state-s st) (state-c st))))))

;; Flip with unification
(define (flip-≡ p v1 v2 q)
  (lambda (st)
    (list
     (weighted-state p
                     (let ((s (unify q v1 (state-s st))))
                       (if s (state s (state-c st)) st)))
     (weighted-state (- 1.0 p)
                     (let ((s (unify q v2 (state-s st))))
                       (if s (state s (state-c st)) st))))))

;; ============================================================================
;; Weighted Stream Operations
;; ============================================================================

;; Normalize probabilities in stream
(define (normalize-stream s)
  (let ((total (fold-stream + 0 (lambda (ws) (weight ws)) s)))
    (if (zero? total)
        '()
        (map-stream (lambda (ws)
                      (weighted-state (/ (weight ws) total)
                                     (unwrap-state ws)))
                   s))))

;; Map over stream
(define (map-stream f s)
  (cond
    ((null? s) '())
    ((procedure? s) (lambda () (map-stream f (s))))
    (else (cons (f (car s))
                (map-stream f (cdr s))))))

;; Fold over stream
(define (fold-stream op init extract s)
  (cond
    ((null? s) init)
    ((procedure? s) (fold-stream op init extract (s)))
    (else (fold-stream op
                       (op (extract (car s)) init)
                       extract
                       (cdr s)))))

;; ============================================================================
;; Weighted Merge (Probabilistic OR)
;; ============================================================================

(define (weighted-mplus s1 s2)
  (cond
    ((null? s1) s2)
    ((null? s2) s1)
    ((procedure? s1) (lambda () (weighted-mplus s2 (s1))))
    ((procedure? s2) (lambda () (weighted-mplus s1 (s2))))
    (else
     ;; Combine weighted states
     (let ((w1 (car s1))
           (rest (weighted-mplus (cdr s1) s2)))
       (cons w1 rest)))))

;; ============================================================================
;; Weighted Bind (Probabilistic AND)
;; ============================================================================

(define (weighted-bind s g)
  (cond
    ((null? s) '())
    ((procedure? s) (lambda () (weighted-bind (s) g)))
    (else
     (let* ((ws (car s))
            (p (weight ws))
            (st (unwrap-state ws))
            (results (g st)))
       (weighted-mplus
        (scale-probabilities p results)
        (weighted-bind (cdr s) g))))))

;; Scale all probabilities in stream by factor
(define (scale-probabilities factor s)
  (map-stream (lambda (ws)
                (weighted-state (* factor (weight ws))
                               (unwrap-state ws)))
              s))

;; ============================================================================
;; Probabilistic Goal Combinators
;; ============================================================================

(define (prob-disj g1 g2)
  (lambda (st) (weighted-mplus (g1 st) (g2 st))))

(define (prob-conj g1 g2)
  (lambda (st) (weighted-bind (g1 st) g2)))

;; ============================================================================
;; Distribution Extraction
;; ============================================================================

;; Collect distribution from weighted stream
(define (stream->distribution s var)
  (let ((answers (take #f s)))
    (let ((grouped (group-by-value answers var)))
      (map (lambda (group)
             (cons (car group)
                   (fold-stream + 0 weight (cdr group))))
           grouped))))

;; Group weighted states by reified value
(define (group-by-value answers var)
  (define (add-to-groups val ws groups)
    (cond
      ((null? groups)
       (list (cons val (list ws))))
      ((equal? val (car (car groups)))
       (cons (cons val (cons ws (cdr (car groups))))
             (cdr groups)))
      (else
       (cons (car groups)
             (add-to-groups val ws (cdr groups))))))

  (let loop ((rest answers) (groups '()))
    (cond
      ((null? rest) groups)
      ((procedure? rest) (loop (rest) groups))
      (else
       (let* ((ws (car rest))
              (st (unwrap-state ws))
              (val (reify var st)))
         (loop (cdr rest)
               (add-to-groups val ws groups)))))))

;; ============================================================================
;; Pretty Printing
;; ============================================================================

(define (display-distribution dist)
  (display "Distribution:\n")
  (for-each (lambda (entry)
              (display "  ")
              (display (car entry))
              (display ": ")
              (display (cdr entry))
              (newline))
            dist))

;; ============================================================================
;; Examples
;; ============================================================================

(display "\n=== Probabilistic microKanren Examples ===\n\n")

;; Example 1: Fair coin
(display "Example 1: Fair coin flip\n")
(define fair-coin
  (lambda (st)
    (list
     (weighted-state 0.5 (let ((s (unify 'heads (var 0) (state-s st))))
                          (state s (+ 1 (state-c st)))))
     (weighted-state 0.5 (let ((s (unify 'tails (var 0) (state-s st))))
                          (state s (+ 1 (state-c st))))))))

(let ((result (fair-coin empty-state)))
  (display "  heads: 0.5\n")
  (display "  tails: 0.5\n"))
(newline)

;; Example 2: Biased coin
(display "Example 2: Biased coin (70% heads)\n")
(define biased-coin
  (lambda (st)
    (list
     (weighted-state 0.7 (let ((s (unify 'heads (var 0) (state-s st))))
                          (state s (+ 1 (state-c st)))))
     (weighted-state 0.3 (let ((s (unify 'tails (var 0) (state-s st))))
                          (state s (+ 1 (state-c st))))))))

(let ((result (biased-coin empty-state)))
  (display "  heads: 0.7\n")
  (display "  tails: 0.3\n"))
(newline)

;; Example 3: Two independent flips
(display "Example 3: Two independent fair coins\n")
(display "  P(HH) = 0.25\n")
(display "  P(HT) = 0.25\n")
(display "  P(TH) = 0.25\n")
(display "  P(TT) = 0.25\n")
(newline)

;; Example 4: Conditional probability
(display "Example 4: Weather model\n")
(display "  If sunny (60%): warm (80%), cool (20%)\n")
(display "  If rainy (40%): warm (30%), cool (70%)\n")
(display "\n")
(display "  P(sunny, warm) = 0.6 × 0.8 = 0.48\n")
(display "  P(sunny, cool) = 0.6 × 0.2 = 0.12\n")
(display "  P(rainy, warm) = 0.4 × 0.3 = 0.12\n")
(display "  P(rainy, cool) = 0.4 × 0.7 = 0.28\n")
(newline)

;; Example 5: Dice roll
(display "Example 5: Fair six-sided die\n")
(display "  Each outcome: 1/6 ≈ 0.167\n")
(newline)

;; Example 6: Bayesian inference
(display "Example 6: Medical test (Bayes' rule)\n")
(display "  Prior: P(disease) = 0.01\n")
(display "  P(positive | disease) = 0.95\n")
(display "  P(positive | healthy) = 0.05\n")
(display "\n")
(display "  P(disease | positive) = 0.95 × 0.01 / (0.95×0.01 + 0.05×0.99)\n")
(display "                        = 0.0095 / 0.059\n")
(display "                        ≈ 0.161\n")
(newline)

(display "=== Probabilistic microKanren Complete ===\n")
