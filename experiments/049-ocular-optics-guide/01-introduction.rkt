#lang racket

;; Experiment 049: Ocular-Patdown Optics Guide
;; Part 1: Introduction - What are Optics?
;;
;; Based on: https://docs.racket-lang.org/ocular-patdown/optics-guide.html

(require racket/struct)
(require ocular-patdown)

;; ============================================================================
;; The Problem: Nested Immutable Updates
;; ============================================================================

;; Consider updating deeply nested data structures

(struct person (name address) #:transparent)
(struct address (street city state zip) #:transparent)

(define alice
  (person "Alice"
          (address "123 Main St" "Boston" "MA" "02101")))

;; Without optics: Manual nested copying

(define (update-zip-manual p new-zip)
  (struct-copy person p
               [address (struct-copy address (person-address p)
                                     [zip new-zip])]))

(displayln "=== Manual Nested Update ===")
(displayln alice)
(displayln (update-zip-manual alice "02102"))
(newline)

;; ============================================================================
;; The Solution: Optics as First-Class References
;; ============================================================================

;; Optics provide composable references to nested parts

;; Define lenses for each field using struct-lens
(define person-name-lens (struct-lens person name))
(define person-address-lens (struct-lens person address))
(define address-zip-lens (struct-lens address zip))

;; Compose lenses to reach nested fields
(define person-zip-lens
  (optic-compose person-address-lens address-zip-lens))

;; Use the composed lens
(displayln "=== Optics-Based Update ===")
(displayln alice)
(displayln (optic-set person-zip-lens alice "02102"))
(newline)

;; ============================================================================
;; Core Insight: Optics are Composable References
;; ============================================================================

(displayln "=== Core Operations ===")

;; View: Extract the focused value
(displayln (format "View zip: ~a" (optic-get person-zip-lens alice)))

;; Set: Replace the focused value
(define alice-new-zip (optic-set person-zip-lens alice "02103"))
(displayln (format "Set zip: ~a" alice-new-zip))

;; Transform: Apply function to focused value
(define alice-upper-zip (optic-modify person-zip-lens alice string-upcase))
(displayln (format "Transform zip: ~a" alice-upper-zip))
(newline)

;; ============================================================================
;; Why Optics?
;; ============================================================================

(displayln "=== Why Optics? ===")
(displayln "1. Composability: Build complex paths from simple pieces")
(displayln "2. Reusability: Define once, use everywhere")
(displayln "3. Type Safety: Compile-time guarantees")
(displayln "4. Abstraction: Hide implementation details")
(newline)

;; ============================================================================
;; Comparison: Manual vs Optics
;; ============================================================================

(define bob
  (person "Bob"
          (address "456 Oak Ave" "Cambridge" "MA" "02138")))

;; Manual approach: Requires knowing the entire path
(define bob-manual
  (struct-copy person bob
               [address (struct-copy address (person-address bob)
                                     [city "Somerville"]
                                     [zip "02144"])]))

;; Optics approach: Compose focused updates
(define address-city-lens (struct-lens address city))
(define person-city-lens (optic-compose person-address-lens address-city-lens))

(define bob-optics
  (optic-modify person-zip-lens
                (optic-set person-city-lens bob "Somerville")
                (λ (_) "02144")))

(displayln "=== Manual vs Optics ===")
(displayln (format "Manual: ~a" bob-manual))
(displayln (format "Optics: ~a" bob-optics))
(newline)

;; ============================================================================
;; Next Steps
;; ============================================================================

(displayln "=== Next: Lenses ===")
(displayln "Lenses focus on exactly one subpart")
(displayln "See 02-lenses.rkt for details")
