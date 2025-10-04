#lang racket

;; Optics Composition: TARGET and FOCUS
;; Demonstrating how composition maintains the target/focus relationship

(provide (all-defined-out))

;; ============================================================================
;; COMPOSING OPTICS: TARGET → FOCUS
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         OPTICS COMPOSITION: TARGET and FOCUS                 ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "KEY CONCEPT:")
(displayln "  TARGET = the whole structure you're working with")
(displayln "  FOCUS  = the specific part you're viewing/updating")
(newline)

(displayln "When you COMPOSE optics:")
(displayln "  • The TARGET stays the same")
(displayln "  • The FOCUS gets narrower")
(displayln "  • You zoom in deeper into the structure")
(newline)

;; ============================================================================
;; BASIC LENS DEFINITION
;; ============================================================================

(struct lens (getter setter) #:transparent)

(define (view l target)
  "View the FOCUS from the TARGET."
  ((lens-getter l) target))

(define (set l target new-focus)
  "Update the FOCUS in the TARGET, return new TARGET."
  ((lens-setter l) target new-focus))

(define (over l target fn)
  "Modify the FOCUS in the TARGET using function."
  (set l target (fn (view l target))))

;; ============================================================================
;; COMPOSING TWO LENSES
;; ============================================================================

(displayln "═══ COMPOSING LENSES ═══")
(newline)

(define (compose-lens outer inner)
  "Compose two lenses: outer focuses on intermediate, inner on final focus.

   TARGET --outer--> INTERMEDIATE --inner--> FOCUS

   Result: TARGET ----------------> FOCUS"
  (lens
    ; getter: get outer, then get inner from that
    (λ (target)
      (define intermediate (view outer target))
      (view inner intermediate))

    ; setter: get outer, update inner in it, set outer back
    (λ (target new-focus)
      (define intermediate (view outer target))
      (define new-intermediate (set inner intermediate new-focus))
      (set outer target new-intermediate))))

(displayln "Composition: outer ∘ inner")
(displayln "  • outer: TARGET → INTERMEDIATE")
(displayln "  • inner: INTERMEDIATE → FOCUS")
(displayln "  • composed: TARGET → FOCUS")
(newline)

;; ============================================================================
;; EXAMPLE: NESTED STRUCTURES
;; ============================================================================

(displayln "═══ EXAMPLE: Nested Structures ═══")
(newline)

(struct address (street city state zip) #:transparent)
(struct person (name age addr) #:transparent)
(struct company (name ceo) #:transparent)

;; Sample data
(define alice-addr (address "123 Main St" "Boston" "MA" "02101"))
(define alice (person "Alice" 30 alice-addr))
(define tech-corp (company "Tech Corp" alice))

(displayln "Data structure:")
(displayln "  company")
(displayln "    └─ ceo (person)")
(displayln "         └─ addr (address)")
(displayln "              └─ city (string)")
(newline)

(displayln "TARGET: company")
(displayln (format "  ~a" tech-corp))
(newline)

;; ============================================================================
;; BUILDING LENSES STEP BY STEP
;; ============================================================================

(displayln "═══ LENSES AT EACH LEVEL ═══")
(newline)

;; Level 1: company → person (ceo)
(define ceo-lens
  (lens
    company-ceo
    (λ (comp new-ceo) (struct-copy company comp [ceo new-ceo]))))

(displayln "1. ceo-lens: company → person")
(displayln (format "   TARGET: ~a" tech-corp))
(displayln (format "   FOCUS:  ~a" (view ceo-lens tech-corp)))
(newline)

;; Level 2: person → address
(define addr-lens
  (lens
    person-addr
    (λ (p new-addr) (struct-copy person p [addr new-addr]))))

(displayln "2. addr-lens: person → address")
(define alice-person (view ceo-lens tech-corp))
(displayln (format "   TARGET: ~a" alice-person))
(displayln (format "   FOCUS:  ~a" (view addr-lens alice-person)))
(newline)

;; Level 3: address → city
(define city-lens
  (lens
    address-city
    (λ (a new-city) (struct-copy address a [city new-city]))))

(displayln "3. city-lens: address → city")
(define alice-address (view addr-lens alice-person))
(displayln (format "   TARGET: ~a" alice-address))
(displayln (format "   FOCUS:  ~a" (view city-lens alice-address)))
(newline)

;; ============================================================================
;; COMPOSING THE LENSES
;; ============================================================================

(displayln "═══ COMPOSITION: company → city ═══")
(newline)

;; Compose: company → person → address
(define ceo-addr-lens (compose-lens ceo-lens addr-lens))

(displayln "Step 1: ceo-lens ∘ addr-lens")
(displayln "  company → person → address")
(displayln (format "  TARGET: ~a" tech-corp))
(displayln (format "  FOCUS:  ~a" (view ceo-addr-lens tech-corp)))
(newline)

;; Compose: company → person → address → city
(define ceo-city-lens (compose-lens ceo-addr-lens city-lens))

(displayln "Step 2: (ceo-lens ∘ addr-lens) ∘ city-lens")
(displayln "  company → person → address → city")
(displayln (format "  TARGET: ~a" tech-corp))
(displayln (format "  FOCUS:  ~a" (view ceo-city-lens tech-corp)))
(newline)

;; ============================================================================
;; USING THE COMPOSED LENS
;; ============================================================================

(displayln "═══ USING THE COMPOSED LENS ═══")
(newline)

(displayln "View CEO's city:")
(displayln (format "  ~a" (view ceo-city-lens tech-corp)))
(newline)

(displayln "Change CEO's city to 'Cambridge':")
(define updated-company (set ceo-city-lens tech-corp "Cambridge"))
(displayln (format "  Before: ~a" tech-corp))
(displayln (format "  After:  ~a" updated-company))
(newline)

(displayln "Notice:")
(displayln "  • TARGET changed: entire company struct")
(displayln "  • Only FOCUS updated: just the city string")
(displayln "  • Everything else preserved: name, age, street, etc.")
(newline)

;; ============================================================================
;; THE PATH FROM TARGET TO FOCUS
;; ============================================================================

(displayln "═══ THE PATH: TARGET → FOCUS ═══")
(newline)

(displayln "Path visualization:")
(displayln "")
(displayln "  TARGET: (company ...)         ← Start here")
(displayln "          │")
(displayln "          ├─ ceo-lens")
(displayln "          │")
(displayln "          ▼")
(displayln "         (person ...)           ← Intermediate")
(displayln "          │")
(displayln "          ├─ addr-lens")
(displayln "          │")
(displayln "          ▼")
(displayln "         (address ...)          ← Intermediate")
(displayln "          │")
(displayln "          ├─ city-lens")
(displayln "          │")
(displayln "          ▼")
(displayln "         \\\"Boston\\\"               ← FOCUS")
(newline)

(displayln "Composed lens jumps directly:")
(displayln "")
(displayln "  TARGET: (company ...)")
(displayln "          │")
(displayln "          ├─ ceo-city-lens (composed)")
(displayln "          │")
(displayln "          ▼")
(displayln "         \\\"Boston\\\"               ← FOCUS")
(newline)

;; ============================================================================
;; MULTIPLE COMPOSITIONS
;; ============================================================================

(displayln "═══ DIFFERENT COMPOSITIONS, DIFFERENT FOCI ═══")
(newline)

;; CEO's name
(define name-lens
  (lens
    person-name
    (λ (p new-name) (struct-copy person p [name new-name]))))

(define ceo-name-lens (compose-lens ceo-lens name-lens))

;; CEO's zip code
(define zip-lens
  (lens
    address-zip
    (λ (a new-zip) (struct-copy address a [zip new-zip]))))

(define ceo-zip-lens (compose-lens ceo-addr-lens zip-lens))

(displayln "Same TARGET, different FOCI:")
(displayln (format "  TARGET: ~a" tech-corp))
(newline)

(displayln "  FOCUS 1 (ceo-name-lens): ~a"
          (view ceo-name-lens tech-corp))

(displayln "  FOCUS 2 (ceo-city-lens): ~a"
          (view ceo-city-lens tech-corp))

(displayln "  FOCUS 3 (ceo-zip-lens):  ~a"
          (view ceo-zip-lens tech-corp))
(newline)

;; ============================================================================
;; UPDATE PRESERVES STRUCTURE
;; ============================================================================

(displayln "═══ UPDATE PRESERVES STRUCTURE ═══")
(newline)

(displayln "Original TARGET:")
(displayln (format "  ~a" tech-corp))
(newline)

(displayln "Update CEO name:")
(define c1 (set ceo-name-lens tech-corp "Bob"))
(displayln (format "  ~a" c1))
(displayln "  → Only name changed!")
(newline)

(displayln "Update CEO city:")
(define c2 (set ceo-city-lens tech-corp "Cambridge"))
(displayln (format "  ~a" c2))
(displayln "  → Only city changed!")
(newline)

(displayln "Update both (chain operations):")
(define c3 (set ceo-name-lens c2 "Charlie"))
(displayln (format "  ~a" c3))
(displayln "  → Name AND city changed!")
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "COMPOSITION KEY POINTS:")
(newline)

(displayln "1. TARGET vs FOCUS")
(displayln "   • TARGET: the whole structure (stays constant)")
(displayln "   • FOCUS: the part you're zooming into (gets narrower)")
(newline)

(displayln "2. COMPOSING LENSES")
(displayln "   • outer ∘ inner")
(displayln "   • Creates a path: TARGET → ... → FOCUS")
(displayln "   • Composition is associative: (a ∘ b) ∘ c = a ∘ (b ∘ c)")
(newline)

(displayln "3. GETTING (view)")
(displayln "   • Follow the path forward")
(displayln "   • TARGET → INTERMEDIATE → ... → FOCUS")
(newline)

(displayln "4. SETTING (set)")
(displayln "   • Follow path forward to get old FOCUS")
(displayln "   • Replace with new FOCUS")
(displayln "   • Follow path backward, rebuilding structure")
(displayln "   • Return new TARGET")
(newline)

(displayln "5. PRESERVATION")
(displayln "   • Only the FOCUS changes")
(displayln "   • Everything else in TARGET stays the same")
(displayln "   • Immutable updates!")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 045: Optics Composition")
(displayln "═══════════════════════════════════════════════════════════════")
