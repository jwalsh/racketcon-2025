#lang roulette

;; Basic probabilistic inference examples with Roulette
;; RacketCon 2025 - Experiment 089

(provide (all-defined-out))

;; ============================================================================
;; Example 1: Simple Discrete Distributions
;; ============================================================================

(module+ main
  (displayln "=== Roulette: Basic Inference Examples ===\n")

  ;; Fair coin flip
  (displayln "Example 1: Fair coin flip")
  (define fair-coin (discrete-uniform '(heads tails)))
  (displayln (~a "P(heads) = " (probability (equal? fair-coin 'heads))))
  (displayln (~a "P(tails) = " (probability (equal? fair-coin 'tails))))
  (newline)

  ;; Biased coin
  (displayln "Example 2: Biased coin (70% heads)")
  (define (biased-coin)
    (disrupt
     [(7/10) 'heads]
     [(3/10) 'tails]))

  (displayln (~a "P(heads) = " (probability (equal? (biased-coin) 'heads))))
  (displayln (~a "P(tails) = " (probability (equal? (biased-coin) 'tails))))
  (newline)

  ;; Fair die
  (displayln "Example 3: Fair six-sided die")
  (define die (discrete-uniform '(1 2 3 4 5 6)))
  (displayln (~a "P(roll = 6) = " (probability (= die 6))))
  (displayln (~a "P(roll even) = " (probability (even? die))))
  (displayln (~a "P(roll > 4) = " (probability (> die 4))))
  (newline))

;; ============================================================================
;; Example 2: Conditioning and Bayesian Inference
;; ============================================================================

(module+ conditioning
  (displayln "=== Conditioning Examples ===\n")

  ;; Two dice with constraint
  (displayln "Example 4: Two dice sum to 7")
  (define (two-dice-sum-7)
    (define d1 (discrete-uniform '(1 2 3 4 5 6)))
    (define d2 (discrete-uniform '(1 2 3 4 5 6)))
    (observe (= (+ d1 d2) 7))
    (list d1 d2))

  (displayln (~a "P((1,6) | sum=7) = "
                (probability (equal? (two-dice-sum-7) '(1 6)))))
  (displayln (~a "P((6,1) | sum=7) = "
                (probability (equal? (two-dice-sum-7) '(6 1)))))
  (displayln (~a "P((3,4) | sum=7) = "
                (probability (equal? (two-dice-sum-7) '(3 4)))))
  (newline)

  ;; Medical test (Bayes' theorem)
  (displayln "Example 5: Medical test inference")
  (define (medical-test)
    ;; Prior: 1% of population has disease
    (define has-disease (bernoulli 1/100))

    ;; Test characteristics
    (define test-positive
      (if has-disease
          (bernoulli 99/100)  ; 99% sensitivity
          (bernoulli 5/100))) ; 5% false positive rate

    ;; Observe: test came back positive
    (observe test-positive)

    ;; Return disease status
    has-disease)

  (displayln (~a "P(disease | test+) = "
                (probability (medical-test))))
  (newline))

;; ============================================================================
;; Example 3: Monty Hall Problem
;; ============================================================================

(module+ monty-hall
  (displayln "=== Monty Hall Problem ===\n")

  (define (monty-hall-stay)
    ;; Prize is behind random door
    (define prize (discrete-uniform '(1 2 3)))

    ;; Player always picks door 1
    (define pick 1)

    ;; Monty opens a non-prize, non-picked door
    (define monty-opens
      (cond
        [(= prize 1) (discrete-uniform '(2 3))]
        [(= prize 2) 3]
        [(= prize 3) 2]))

    ;; Player stays with door 1
    (= prize pick))

  (define (monty-hall-switch)
    ;; Prize is behind random door
    (define prize (discrete-uniform '(1 2 3)))

    ;; Player picks door 1
    (define initial-pick 1)

    ;; Monty opens a door
    (define monty-opens
      (cond
        [(= prize 1) (discrete-uniform '(2 3))]
        [(= prize 2) 3]
        [(= prize 3) 2]))

    ;; Player switches to the other closed door
    (define final-pick
      (first (remove monty-opens (remove initial-pick '(1 2 3)))))

    ;; Did player win?
    (= prize final-pick))

  (displayln (~a "P(win | stay) = " (probability (monty-hall-stay))))
  (displayln (~a "P(win | switch) = " (probability (monty-hall-switch))))
  (newline))

;; ============================================================================
;; Example 4: Probabilistic Reasoning
;; ============================================================================

(module+ reasoning
  (displayln "=== Probabilistic Reasoning ===\n")

  ;; Birthday paradox
  (displayln "Example 6: Birthday paradox (23 people)")
  (define (birthday-collision n)
    (define (pick-birthdays count)
      (if (= count 0)
          '()
          (cons (discrete-uniform (range 1 366))
                (pick-birthdays (- count 1)))))

    (define birthdays (pick-birthdays n))
    (not (= (length birthdays) (length (remove-duplicates birthdays)))))

  (displayln (~a "P(collision with 23 people) ≈ "
                (probability (birthday-collision 23))))
  (newline)

  ;; Probabilistic computation
  (displayln "Example 7: Sum of random values")
  (define (sum-uniform n)
    (if (= n 0)
        0
        (+ (discrete-uniform '(1 2 3))
           (sum-uniform (- n 1)))))

  (displayln (~a "P(sum of 3 draws = 6) = "
                (probability (= (sum-uniform 3) 6))))
  (newline))

;; ============================================================================
;; Example 5: Multiple Random Choices
;; ============================================================================

(module+ multiple-choices
  (displayln "=== Multiple Random Choices ===\n")

  ;; Balls in urns
  (displayln "Example 8: Balls in urns")
  (define (urn-experiment)
    ;; Pick an urn
    (define urn (discrete-uniform '(A B)))

    ;; Urn A: 7 red, 3 blue
    ;; Urn B: 2 red, 8 blue
    (define ball
      (case urn
        [(A) (disrupt [(7/10) 'red] [(3/10) 'blue])]
        [(B) (disrupt [(2/10) 'red] [(8/10) 'blue])]))

    ;; Observe: drew a red ball
    (observe (equal? ball 'red))

    ;; Which urn did it come from?
    urn)

  (displayln (~a "P(urn A | red ball) = "
                (probability (equal? (urn-experiment) 'A))))
  (displayln (~a "P(urn B | red ball) = "
                (probability (equal? (urn-experiment) 'B))))
  (newline))

;; ============================================================================
;; Example 6: Conditional Independence
;; ============================================================================

(module+ independence
  (displayln "=== Conditional Independence ===\n")

  (define (conditional-independence-test)
    ;; Common cause
    (define rain (bernoulli 1/10))

    ;; Independent effects given cause
    (define grass-wet
      (if rain
          (bernoulli 9/10)   ; Usually wet when raining
          (bernoulli 1/10))) ; Sometimes wet from sprinkler

    (define traffic-slow
      (if rain
          (bernoulli 8/10)   ; Usually slow when raining
          (bernoulli 1/10))) ; Sometimes slow anyway

    ;; Observe grass is wet
    (observe grass-wet)

    ;; Does this tell us about traffic?
    traffic-slow)

  (displayln (~a "P(traffic slow | grass wet) = "
                (probability (conditional-independence-test))))
  (newline)

  (displayln "=== Basic Inference Complete ==="))
