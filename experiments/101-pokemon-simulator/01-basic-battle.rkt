#lang roulette/example/disrupt

;; Pokemon Battle Simulator using Roulette
;; Demonstrates probabilistic game mechanics with observe!

(require racket/format)

;; ============================================================================
;; Pokemon Stats
;; ============================================================================

(struct pokemon (name hp attack defense speed) #:transparent)

(define pikachu (pokemon "Pikachu" 100 55 40 90))
(define charmander (pokemon "Charmander" 100 52 43 65))
(define squirtle (pokemon "Squirtle" 100 48 65 43))

;; ============================================================================
;; Battle Mechanics
;; ============================================================================

;; Critical hit: 1/16 chance (~6.25%)
(define (critical-hit?)
  (flip (/ 1 16)))

;; Move accuracy (most moves are 100%, some 90%, 85%)
(define (move-hits? accuracy)
  (flip accuracy))

;; Status effects
(define (paralysis-check)
  (flip 0.25)) ; 25% chance to be fully paralyzed

(define (burn-damage)
  (flip 0.5)) ; Burn does damage

;; ============================================================================
;; Damage Calculation
;; ============================================================================

(define (calculate-damage attacker defender move-power)
  (define base-damage
    (/ (* (* 2 50) ; Level 50
          move-power
          (/ (pokemon-attack attacker)
             (pokemon-defense defender)))
       50))

  (define critical (if (critical-hit?) 2 1))
  (define random-factor (if (flip 0.5) 1.0 0.85)) ; Simplified random 85-100%

  (* base-damage critical random-factor))

;; ============================================================================
;; Simple Battle Scenario
;; ============================================================================

(module+ main
  (displayln "=== Pokemon Battle Simulator ===\n")

  ;; Battle: Pikachu vs Charmander
  (displayln "Pikachu vs Charmander\n")

  ;; Move: Thunderbolt (power 90, 100% accuracy)
  (define thunderbolt-power 90)
  (define thunderbolt-accuracy 1.0)

  (displayln "Pikachu uses Thunderbolt!")

  ;; Does it hit?
  (define hits (move-hits? thunderbolt-accuracy))
  (displayln (~a "Move hits: " hits))
  (displayln (~a "  P(hit) = " thunderbolt-accuracy "\n"))

  ;; Calculate damage if it hits
  (when hits
    (define is-crit (critical-hit?))
    (displayln (~a "Critical hit: " is-crit))
    (displayln (~a "  P(crit) = " (/ 1 16) " ≈ 0.0625\n"))

    (define damage (calculate-damage pikachu charmander thunderbolt-power))
    (displayln (~a "Damage dealt: " damage))
    (displayln (~a "  Base: 90 power"))
    (displayln (~a "  Crit multiplier: " (if is-crit "2x" "1x")))
    (displayln (~a "  Random factor: 85-100%")))

  (newline))

;; ============================================================================
;; Conditional Battle Analysis
;; ============================================================================

(module+ conditional
  (displayln "=== Conditional Analysis ===\n")

  ;; What if we KNOW Pikachu won?
  (displayln "Scenario: Pikachu defeats Charmander\n")

  (define thunderbolt (move-hits? 1.0))
  (define crit (critical-hit?))

  (displayln "Before observation:")
  (displayln (~a "  P(critical hit) = " crit))
  (newline)

  ;; Observe: The attack was strong enough to KO
  ;; (simplified: assume KO requires critical hit)
  (observe! crit)

  (displayln "After observing KO:")
  (displayln (~a "  P(critical hit) = " crit))
  (displayln "  Knowing Pikachu won, critical hit probability increases!")

  (newline))

;; ============================================================================
;; Speed Tie
;; ============================================================================

(module+ speed-tie
  (displayln "=== Speed Tie Analysis ===\n")

  ;; When speed is tied, 50/50 who goes first
  (define (speed-priority p1 p2)
    (cond
      [(> (pokemon-speed p1) (pokemon-speed p2)) p1]
      [(< (pokemon-speed p1) (pokemon-speed p2)) p2]
      [else (if (flip 0.5) p1 p2)])) ; Speed tie!

  ;; Create two Pokemon with same speed
  (define pika-1 (pokemon "Pikachu-1" 100 55 40 90))
  (define pika-2 (pokemon "Pikachu-2" 100 55 40 90))

  (define first-mover (speed-priority pika-1 pika-2))

  (displayln "Two Pikachu with same speed (90)")
  (displayln (~a "First to move: " first-mover))
  (displayln "  Speed tie = 50/50 chance\n"))

;; ============================================================================
;; Type Effectiveness
;; ============================================================================

(module+ type-effectiveness
  (displayln "=== Type Effectiveness ===\n")

  ;; Type chart (simplified)
  (define (type-multiplier attacker-move defender-type)
    (match (list attacker-move defender-type)
      [(list "electric" "water") 2.0]   ; Super effective
      [(list "electric" "ground") 0.0]  ; No effect
      [(list "fire" "grass") 2.0]       ; Super effective
      [(list "water" "fire") 2.0]       ; Super effective
      [_ 1.0]))                         ; Neutral

  (displayln "Pikachu (Electric) vs Squirtle (Water)")
  (define electric-vs-water (type-multiplier "electric" "water"))
  (displayln (~a "  Thunderbolt effectiveness: " electric-vs-water "x"))
  (displayln "  Super effective!\n")

  (displayln "Pikachu (Electric) vs Sandshrew (Ground)")
  (define electric-vs-ground (type-multiplier "electric" "ground"))
  (displayln (~a "  Thunderbolt effectiveness: " electric-vs-ground "x"))
  (displayln "  No effect!"))

;; ============================================================================
;; Multi-Turn Battle
;; ============================================================================

(module+ multi-turn
  (displayln "\n=== Multi-Turn Battle Probability ===\n")

  ;; Probability of landing 3 hits in a row with 85% accuracy
  (define hit-1 (move-hits? 0.85))
  (define hit-2 (move-hits? 0.85))
  (define hit-3 (move-hits? 0.85))

  (define all-hit (and hit-1 hit-2 hit-3))

  (displayln "Using a move with 85% accuracy 3 times:")
  (displayln (~a "  P(all 3 hit) = " all-hit))
  (displayln (~a "  Expected: 0.85³ ≈ " (expt 0.85 3)))

  (newline)

  ;; What if we KNOW at least 2 hit?
  (define at-least-2 (or (and hit-1 hit-2)
                         (and hit-1 hit-3)
                         (and hit-2 hit-3)))

  (displayln "Observing at least 2 hits landed:")
  (observe! at-least-2)

  (displayln (~a "  P(all 3 hit | at least 2) = " all-hit))
  (displayln "  Conditional probability increases!"))
