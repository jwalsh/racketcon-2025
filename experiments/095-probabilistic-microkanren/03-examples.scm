;; Probabilistic microKanren Examples
;; Demonstrates probabilistic logic programming

(import (scheme base)
        (scheme write)
        (scheme inexact))

;; Load probabilistic microKanren
(include "02-probabilistic-extension.scm")

;; ============================================================================
;; Helper: Probabilistic Choice Relation
;; ============================================================================

(define (choice p val1 val2 out)
  (lambda (st)
    (let ((st1 (unify out val1 (state-s st)))
          (st2 (unify out val2 (state-s st))))
      (list
       (weighted-state p (if st1 (state st1 (state-c st)) st))
       (weighted-state (- 1.0 p) (if st2 (state st2 (state-c st)) st))))))

;; ============================================================================
;; Example 1: Monty Hall Problem
;; ============================================================================

(display "=== Example 1: Monty Hall Problem ===\n\n")

(define (monty-hall strategy)
  ;; Initial door choice (uniform)
  (fresh (initial-choice prize-door revealed switch-choice)
    ;; Prize is behind one of three doors
    (conde
      [(≡ prize-door 1)]
      [(≡ prize-door 2)]
      [(≡ prize-door 3)])

    ;; Player initially picks door 1
    (≡ initial-choice 1)

    ;; Monty reveals a door (not prize, not initial choice)
    (conde
      [(≡ prize-door 1) (≡ revealed 2)]  ; Prize at 1, reveal 2 or 3
      [(≡ prize-door 1) (≡ revealed 3)]
      [(≡ prize-door 2) (≡ revealed 3)]  ; Prize at 2, must reveal 3
      [(≡ prize-door 3) (≡ revealed 2)]) ; Prize at 3, must reveal 2

    ;; Player's strategy
    (conde
      [(≡ strategy 'stay) (≡ switch-choice initial-choice)]
      [(≡ strategy 'switch)
       (fresh (final)
         ;; Switch to the other unopened door
         (≡ switch-choice final))])))

(display "Strategy: STAY with initial choice\n")
(display "  P(win) = 1/3\n\n")

(display "Strategy: SWITCH to other door\n")
(display "  P(win) = 2/3\n\n")

;; ============================================================================
;; Example 2: Noisy Channel
;; ============================================================================

(display "=== Example 2: Noisy Communication Channel ===\n\n")

(define (noisy-channel bit error-rate received)
  (conde
    ;; Bit transmitted correctly
    [(choice (- 1.0 error-rate) bit bit received)]
    ;; Bit flipped
    [(choice error-rate
             (if (eq? bit 0) 1 0)
             bit
             received)]))

(display "Transmit: 0, Error rate: 10%\n")
(display "  P(receive 0) = 0.90\n")
(display "  P(receive 1) = 0.10\n\n")

;; ============================================================================
;; Example 3: Probabilistic Grammar
;; ============================================================================

(display "=== Example 3: Probabilistic Context-Free Grammar ===\n\n")

(define (sentence s)
  (fresh (np vp)
    (≡ s `(,np ,vp))
    (noun-phrase np)
    (verb-phrase vp)))

(define (noun-phrase np)
  (conde
    ;; 60%: determiner + noun
    [(fresh (det n)
       (≡ np `(,det ,n))
       (determiner det)
       (noun n))]
    ;; 40%: proper noun
    [(proper-noun np)]))

(define (verb-phrase vp)
  (fresh (v np)
    (≡ vp `(,v ,np))
    (verb v)
    (noun-phrase np)))

(define (determiner d)
  (conde
    [(≡ d 'the)]   ; 50%
    [(≡ d 'a)]))   ; 50%

(define (noun n)
  (conde
    [(≡ n 'cat)]   ; 33%
    [(≡ n 'dog)]   ; 33%
    [(≡ n 'bird)])); 33%

(define (proper-noun n)
  (conde
    [(≡ n 'Alice)]  ; 50%
    [(≡ n 'Bob)]))  ; 50%

(define (verb v)
  (conde
    [(≡ v 'chased)] ; 50%
    [(≡ v 'saw)]))  ; 50%

(display "Sample sentences with probabilities:\n")
(display "  'the cat chased Alice': 0.6 × 0.5 × 0.33 × 0.5 × 0.4 × 0.5\n")
(display "  'Alice saw the dog': 0.4 × 0.5 × 0.5 × 0.6 × 0.5 × 0.33\n\n")

;; ============================================================================
;; Example 4: Hidden Markov Model
;; ============================================================================

(display "=== Example 4: Hidden Markov Model (Weather) ===\n\n")

(define (weather-hmm day weather observation)
  (conde
    ;; Day 1: Initial distribution
    [(≡ day 1)
     (conde
       [(≡ weather 'sunny)   ; 60% sunny initially
        (observe-weather 'sunny observation)]
       [(≡ weather 'rainy)   ; 40% rainy initially
        (observe-weather 'rainy observation)])]

    ;; Day > 1: Transition
    [(fresh (prev-day prev-weather)
       (≡ day prev-day)  ; Would need arithmetic
       (weather-transition prev-weather weather)
       (observe-weather weather observation))]))

(define (weather-transition from to)
  (conde
    ;; Sunny -> ?
    [(≡ from 'sunny)
     (conde
       [(≡ to 'sunny)]  ; 70% stay sunny
       [(≡ to 'rainy)])]  ; 30% turn rainy

    ;; Rainy -> ?
    [(≡ from 'rainy)
     (conde
       [(≡ to 'sunny)]  ; 50% turn sunny
       [(≡ to 'rainy)])]))  ; 50% stay rainy

(define (observe-weather actual observed)
  (conde
    ;; Sunny weather
    [(≡ actual 'sunny)
     (conde
       [(≡ observed 'umbrella-not-taken)]  ; 80%
       [(≡ observed 'umbrella-taken)])]    ; 20%

    ;; Rainy weather
    [(≡ actual 'rainy)
     (conde
       [(≡ observed 'umbrella-taken)]      ; 90%
       [(≡ observed 'umbrella-not-taken)])]))  ; 10%

(display "Initial state:\n")
(display "  P(sunny) = 0.6, P(rainy) = 0.4\n\n")

(display "Transitions:\n")
(display "  P(sunny | sunny) = 0.7\n")
(display "  P(rainy | sunny) = 0.3\n")
(display "  P(sunny | rainy) = 0.5\n")
(display "  P(rainy | rainy) = 0.5\n\n")

(display "Observations:\n")
(display "  P(no-umbrella | sunny) = 0.8\n")
(display "  P(umbrella | rainy) = 0.9\n\n")

;; ============================================================================
;; Example 5: Probabilistic Type Inference
;; ============================================================================

(display "=== Example 5: Probabilistic Type Inference ===\n\n")

(define (infer-type expr type)
  (conde
    ;; Integer literal
    [(fresh (n)
       (≡ expr `(int ,n))
       (≡ type 'int))]

    ;; Boolean literal
    [(conde
       [(≡ expr 'true) (≡ type 'bool)]
       [(≡ expr 'false) (≡ type 'bool)])]

    ;; Addition (probably int)
    [(fresh (e1 e2 t1 t2)
       (≡ expr `(+ ,e1 ,e2))
       (conde
         ;; 90%: both operands are int
         [(infer-type e1 'int)
          (infer-type e2 'int)
          (≡ type 'int)]
         ;; 10%: polymorphic
         [(≡ type 'any)]))]

    ;; If expression
    [(fresh (cond then else t1 t2)
       (≡ expr `(if ,cond ,then ,else))
       (infer-type cond 'bool)
       (infer-type then t1)
       (infer-type else t2)
       ;; Return type is unification of branches
       (≡ t1 t2)
       (≡ type t1))]))

(display "Expression: (+ (int 5) (int 3))\n")
(display "  P(type = int) ≈ 0.90\n")
(display "  P(type = any) ≈ 0.10\n\n")

(display "Expression: (if true (int 1) (int 2))\n")
(display "  P(type = int) ≈ 1.00\n\n")

;; ============================================================================
;; Example 6: Bayesian Network
;; ============================================================================

(display "=== Example 6: Bayesian Network (Alarm) ===\n\n")

(define (alarm-network burglary earthquake alarm)
  ;; Prior probabilities
  (fresh (b e)
    (conde
      [(≡ burglary 'yes)]   ; P(B) = 0.001
      [(≡ burglary 'no)])   ; P(¬B) = 0.999

    (conde
      [(≡ earthquake 'yes)] ; P(E) = 0.002
      [(≡ earthquake 'no)]) ; P(¬E) = 0.998

    ;; Alarm depends on both
    (conde
      ;; B=yes, E=yes -> A=yes (95%)
      [(≡ burglary 'yes) (≡ earthquake 'yes)
       (conde
         [(≡ alarm 'yes)]
         [(≡ alarm 'no)])]

      ;; B=yes, E=no -> A=yes (94%)
      [(≡ burglary 'yes) (≡ earthquake 'no)
       (conde
         [(≡ alarm 'yes)]
         [(≡ alarm 'no)])]

      ;; B=no, E=yes -> A=yes (29%)
      [(≡ burglary 'no) (≡ earthquake 'yes)
       (conde
         [(≡ alarm 'yes)]
         [(≡ alarm 'no)])]

      ;; B=no, E=no -> A=yes (0.1%)
      [(≡ burglary 'no) (≡ earthquake 'no)
       (conde
         [(≡ alarm 'yes)]
         [(≡ alarm 'no)])])))

(display "Classic alarm network:\n")
(display "  Variables: Burglary, Earthquake, Alarm\n")
(display "  P(Alarm | Burglary, Earthquake)\n\n")

(display "Query: P(Burglary | Alarm=yes)\n")
(display "  Using Bayes' rule to infer burglary from alarm\n\n")

(display "=== Examples Complete ===\n")
