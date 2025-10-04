#lang racket

;; Isomorphisms in the Optics Hierarchy
;; Answering: Are isomorphisms lenses? Are they traversals?

(provide (all-defined-out))

;; ============================================================================
;; THE OPTICS HIERARCHY WITH ISO
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         ISOMORPHISM IN THE OPTICS HIERARCHY                  ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "Question: Are isomorphisms lenses? Are they traversals?")
(displayln "Answer:   YES and YES!")
(newline)

(displayln "The hierarchy (from most specific to most general):")
(newline)
(displayln "  Iso          ← Most specific (1 focus, bidirectional)")
(displayln "    ↓")
(displayln "  Lens         ← 1 focus, always succeeds")
(displayln "    ↓")
(displayln "  Prism        ← 0 or 1 focus")
(displayln "    ↓")
(displayln "  Traversal    ← 0 to N foci")
(displayln "    ↓")
(displayln "  Fold         ← 0 to N foci, read-only")
(newline)

(displayln "Key insight:")
(displayln "  • Every Iso IS a Lens (can get and set)")
(displayln "  • Every Lens IS a Traversal (with exactly 1 focus)")
(displayln "  • Therefore: Every Iso IS a Traversal")
(newline)

;; ============================================================================
;; DEFINING EACH OPTIC
;; ============================================================================

(struct iso (to from) #:transparent)
(struct lens (getter setter) #:transparent)
(struct traversal (view-all over) #:transparent)

;; ============================================================================
;; ISO → LENS CONVERSION
;; ============================================================================

(displayln "═══ ISO → LENS ═══")
(newline)

(define (iso->lens i)
  "Every isomorphism is a lens.
   Getter: convert to target representation
   Setter: convert from source, apply new value, convert back"
  (lens
    ; getter: just convert to target
    (iso-to i)
    ; setter: convert, replace entirely, no need to convert back
    (λ (source new-target) ((iso-from i) new-target))))

(displayln "Conversion: iso->lens")
(displayln "  An ISO can act as a LENS")
(displayln "  • get: use 'to' direction")
(displayln "  • set: use 'from' direction")
(newline)

;; Example: Celsius ↔ Fahrenheit
(define celsius-fahrenheit-iso
  (iso
    ; to: celsius -> fahrenheit
    (λ (c) (+ (* c 9/5) 32))
    ; from: fahrenheit -> celsius
    (λ (f) (* (- f 32) 5/9))))

(define temp-lens (iso->lens celsius-fahrenheit-iso))

(displayln "Example: Celsius ↔ Fahrenheit")
(define temp-c 20)
(displayln (format "  Celsius: ~a°C" temp-c))
(displayln (format "  View as Fahrenheit: ~a°F" ((lens-getter temp-lens) temp-c)))
(displayln (format "  Set to 77°F: ~a°C" ((lens-setter temp-lens) temp-c 77)))
(newline)

;; ============================================================================
;; LENS → TRAVERSAL CONVERSION
;; ============================================================================

(displayln "═══ LENS → TRAVERSAL ═══")
(newline)

(define (lens->traversal l)
  "Every lens is a traversal with exactly 1 focus."
  (traversal
    ; view-all: return single-element list
    (λ (target) (list ((lens-getter l) target)))
    ; over: modify the single focus
    (λ (target f)
      ((lens-setter l) target (f ((lens-getter l) target))))))

(displayln "Conversion: lens->traversal")
(displayln "  A LENS can act as a TRAVERSAL")
(displayln "  • view-all: returns [focus] (list with 1 element)")
(displayln "  • over: modifies the single focus")
(newline)

(define temp-traversal (lens->traversal temp-lens))

(displayln "Example: temp-lens as traversal")
(displayln (format "  Celsius: ~a°C" temp-c))
(displayln (format "  View-all: ~a" ((traversal-view-all temp-traversal) temp-c)))
(displayln (format "  Over (* 1.1): ~a°C"
                  ((traversal-over temp-traversal) temp-c (λ (f) (* f 1.1)))))
(newline)

;; ============================================================================
;; ISO → TRAVERSAL (DIRECT)
;; ============================================================================

(displayln "═══ ISO → TRAVERSAL (Direct) ═══")
(newline)

(define (iso->traversal i)
  "Every isomorphism is a traversal.
   This is iso->lens->traversal composed."
  (lens->traversal (iso->lens i)))

(displayln "Conversion: iso->traversal")
(displayln "  An ISO can act as a TRAVERSAL")
(displayln "  • Composition: iso->lens->traversal")
(displayln "  • Has exactly 1 focus")
(newline)

;; ============================================================================
;; ALL THREE TOGETHER
;; ============================================================================

(displayln "═══ USING THE SAME ISO AS ISO, LENS, TRAVERSAL ═══")
(newline)

(define my-iso celsius-fahrenheit-iso)
(define my-lens (iso->lens my-iso))
(define my-traversal (iso->traversal my-iso))

(define temp 25)

(displayln (format "Starting temperature: ~a°C" temp))
(newline)

(displayln "AS ISO:")
(displayln (format "  to (C→F):   ~a°F" ((iso-to my-iso) temp)))
(displayln (format "  from (F→C): ~a°C" ((iso-from my-iso) 77)))
(newline)

(displayln "AS LENS:")
(displayln (format "  view:       ~a°F" ((lens-getter my-lens) temp)))
(displayln (format "  set to 86°F: ~a°C" ((lens-setter my-lens) temp 86)))
(newline)

(displayln "AS TRAVERSAL:")
(displayln (format "  view-all:   ~a°F" ((traversal-view-all my-traversal) temp)))
(displayln (format "  over (* 2): ~a°C"
                  ((traversal-over my-traversal) temp (λ (f) (* f 2)))))
(newline)

;; ============================================================================
;; NUMBER OF FOCI
;; ============================================================================

(displayln "═══ NUMBER OF FOCI ═══")
(newline)

(displayln "┌──────────────┬─────────────┬──────────────┐")
(displayln "│ Optic        │ # of Foci   │ Bidirectional│")
(displayln "├──────────────┼─────────────┼──────────────┤")
(displayln "│ Iso          │ Exactly 1   │ Yes ✓        │")
(displayln "│ Lens         │ Exactly 1   │ No           │")
(displayln "│ Prism        │ 0 or 1      │ No           │")
(displayln "│ Traversal    │ 0 to N      │ No           │")
(displayln "│ Fold         │ 0 to N      │ No           │")
(displayln "└──────────────┴─────────────┴──────────────┘")
(newline)

(displayln "Key differences:")
(displayln "  • ISO is special: it's BIDIRECTIONAL")
(displayln "  • LENS only goes one way (set doesn't reverse)")
(displayln "  • ISO ⊂ LENS ⊂ TRAVERSAL")
(newline)

;; ============================================================================
;; WHY ISO IS SPECIAL
;; ============================================================================

(displayln "═══ WHY ISO IS SPECIAL ═══")
(newline)

(displayln "Isomorphism Laws:")
(displayln "  1. to ∘ from = identity")
(displayln "  2. from ∘ to = identity")
(newline)

(displayln "This means:")
(displayln "  • LOSSLESS conversion both ways")
(displayln "  • Can freely switch representations")
(displayln "  • Stronger than lens (lens may lose info)")
(newline)

;; Example: Lens that loses information
(struct person (name age) #:transparent)

(define name-lens
  (lens
    person-name
    (λ (p new-name) (struct-copy person p [name new-name]))))

(displayln "Example: name-lens is NOT an iso")
(define alice (person "Alice" 30))
(displayln (format "  Person: ~a" alice))
(displayln (format "  View name: ~a" ((lens-getter name-lens) alice)))
(displayln (format "  Set name: ~a" ((lens-setter name-lens) alice "Bob")))
(displayln "  → We LOST the age information! (still 30)")
(displayln "  → Can't recover original from just \\\"Alice\\\"")
(displayln "  → NOT bidirectional, so NOT an iso")
(newline)

;; ============================================================================
;; COMPOSITION
;; ============================================================================

(displayln "═══ COMPOSITION ═══")
(newline)

(displayln "Composition preserves the hierarchy:")
(newline)

(displayln "  Iso ∘ Iso = Iso")
(displayln "  Iso ∘ Lens = Lens")
(displayln "  Lens ∘ Lens = Lens")
(displayln "  Lens ∘ Traversal = Traversal")
(displayln "  Traversal ∘ Traversal = Traversal")
(newline)

(displayln "Rule: Result is the WEAKER of the two")
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "Q: Are isomorphisms lenses?")
(displayln "A: YES! Every iso can be used as a lens.")
(displayln "   • get = iso.to")
(displayln "   • set = iso.from")
(newline)

(displayln "Q: Are isomorphisms traversals?")
(displayln "A: YES! Every iso can be used as a traversal.")
(displayln "   • view-all = [iso.to(target)]")
(displayln "   • over = modify through iso")
(newline)

(displayln "Q: What makes iso special?")
(displayln "A: BIDIRECTIONALITY!")
(displayln "   • Lens: can set, but setter doesn't invert getter")
(displayln "   • Iso: setter EXACTLY inverts getter")
(displayln "   • Iso ⊂ Lens ⊂ Traversal ⊂ Fold")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 045: Iso in Optics Hierarchy")
(displayln "═══════════════════════════════════════════════════════════════")
