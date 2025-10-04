#lang racket

;; Optics Summary: Lenses vs Traversals
;; Key insight: Lenses are to Traversals as Casting is to Mapping

(provide lens
         traversal
         view
         modify
         fold-of
         to-list-of)

;; ============================================================================
;; TYPE SIGNATURES
;; ============================================================================

(displayln "╔══════════════════════════════════════════════════════════╗")
(displayln "║            OPTICS TYPE SIGNATURES                        ║")
(displayln "╚══════════════════════════════════════════════════════════╝")
(newline)

(displayln "LENS: Exactly 1 focus")
(displayln "  Type: Lens s a")
(displayln "  Where: s = whole structure (source)")
(displayln "         a = focused part (focus)")
(newline)

(displayln "TRAVERSAL: 0 or more foci")
(displayln "  Type: Traversal s a")
(displayln "  Where: s = whole structure")
(displayln "         a = each focused element")
(newline)

;; ============================================================================
;; LENS DEFINITION
;; ============================================================================

(struct lens (getter setter) #:transparent)

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  LENS: One Focus")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

(displayln "Operations:")
(displayln "  • get     : Lens s a → s → a")
(displayln "  • modify  : Lens s a → s → (a → a) → s")
(displayln "  • field   : Access single field")
(newline)

(define (view l target)
  "GET operation: Extract the single focus."
  ((lens-getter l) target))

(define (modify l target fn)
  "MODIFY operation: Update the single focus."
  ((lens-setter l) target (fn ((lens-getter l) target))))

;; Example lens
(define car-lens
  (lens
    car                          ; getter: extract first
    (λ (lst v) (cons v (cdr lst))))) ; setter: replace first

(displayln "Example: car-lens")
(define lst '(10 20 30))
(displayln (format "  List: ~a" lst))
(displayln (format "  view car-lens: ~a" (view car-lens lst)))
(displayln (format "  modify car-lens (* 2): ~a" (modify car-lens lst (λ (x) (* x 2)))))
(displayln "  → Operates on EXACTLY ONE focus (the first element)")
(newline)

;; ============================================================================
;; TRAVERSAL DEFINITION
;; ============================================================================

(struct traversal (folder mapper) #:transparent)

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  TRAVERSAL: Zero or More Foci")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

(displayln "Operations:")
(displayln "  • fold    : Traversal s a → s → b → (b → a → b) → b")
(displayln "  • modify  : Traversal s a → s → (a → a) → s")
(displayln "  • elements: Extract all foci as list")
(newline)

(define (fold-of t target init fn)
  "FOLD operation: Combine all foci."
  ((traversal-folder t) target init fn))

(define (modify-all t target fn)
  "MODIFY operation: Update all foci."
  ((traversal-mapper t) target fn))

(define (to-list-of t target)
  "ELEMENTS operation: Extract all foci as list."
  (reverse (fold-of t target '() (λ (acc x) (cons x acc)))))

;; Example traversal
(define list-traversal
  (traversal
    ; folder: fold over all elements
    (λ (lst init fn)
      (foldl fn init lst))
    ; mapper: map over all elements
    (λ (lst fn)
      (map fn lst))))

(displayln "Example: list-traversal")
(define numbers '(1 2 3 4 5))
(displayln (format "  List: ~a" numbers))
(displayln (format "  to-list-of: ~a" (to-list-of list-traversal numbers)))
(displayln (format "  fold-of (+): ~a" (fold-of list-traversal numbers 0 +)))
(displayln (format "  modify-all (* 2): ~a" (modify-all list-traversal numbers (λ (x) (* x 2)))))
(displayln "  → Operates on MULTIPLE foci (all elements)")
(newline)

;; ============================================================================
;; KEY ANALOGY: LENSES TO TRAVERSALS AS CASTING TO MAPPING
;; ============================================================================

(displayln "╔══════════════════════════════════════════════════════════╗")
(displayln "║     LENSES : TRAVERSALS  ::  CASTING : MAPPING           ║")
(displayln "╚══════════════════════════════════════════════════════════╝")
(newline)

(displayln "LENS is like CASTING:")
(displayln "  • Operates on a SINGLE value")
(displayln "  • Type: a → b")
(displayln "  • Example: (string->number \"42\") → 42")
(displayln "  • One input → one output")
(newline)

(displayln "TRAVERSAL is like MAPPING:")
(displayln "  • Operates on MULTIPLE values")
(displayln "  • Type: (listof a) → (listof b)")
(displayln "  • Example: (map string->number '(\"1\" \"2\" \"3\")) → '(1 2 3)")
(displayln "  • Many inputs → many outputs")
(newline)

;; ============================================================================
;; SIDE-BY-SIDE COMPARISON
;; ============================================================================

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  SIDE-BY-SIDE COMPARISON")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

(displayln "┌─────────────┬──────────────────┬──────────────────────┐")
(displayln "│ Aspect      │ LENS             │ TRAVERSAL            │")
(displayln "├─────────────┼──────────────────┼──────────────────────┤")
(displayln "│ # of foci   │ Exactly 1        │ 0 or more (0..N)     │")
(displayln "│ Operations  │ get, modify      │ fold, modify, elems  │")
(displayln "│ Use case    │ Single field     │ Collection elements  │")
(displayln "│ Analogy     │ Casting (a → b)  │ Mapping (map f list) │")
(displayln "│ Example     │ car-lens         │ list-traversal       │")
(displayln "│ Always OK?  │ Yes (1 focus)    │ No (may be 0 foci)   │")
(displayln "└─────────────┴──────────────────┴──────────────────────┘")
(newline)

;; ============================================================================
;; CONVERSION: LENS → TRAVERSAL
;; ============================================================================

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  LENS → TRAVERSAL CONVERSION (Upcasting)")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

(define (lens->traversal l)
  "Convert a lens (1 focus) to a traversal (1 focus).
   Like upcasting: every lens IS a traversal."
  (traversal
    ; fold: apply function to the single focus
    (λ (target init fn)
      (fn init (view l target)))
    ; map: modify the single focus
    (λ (target fn)
      (modify l target fn))))

(displayln "Every LENS can be viewed as a TRAVERSAL with exactly 1 focus:")
(displayln "  (lens->traversal car-lens)")
(newline)

(define car-as-traversal (lens->traversal car-lens))

(displayln (format "  Original lens:  ~a" (view car-lens '(10 20 30))))
(displayln (format "  As traversal:   ~a" (to-list-of car-as-traversal '(10 20 30))))
(displayln (format "  fold-of (+):    ~a" (fold-of car-as-traversal '(10 20 30) 0 +)))
(displayln (format "  modify (* 2):   ~a" (modify-all car-as-traversal '(10 20 30) (λ (x) (* x 2)))))
(newline)

(displayln "This is like casting:")
(displayln "  Int → Number  (specific to general)")
(displayln "  Lens → Traversal  (1 focus to 0+ foci)")
(newline)

;; ============================================================================
;; PRACTICAL EXAMPLES
;; ============================================================================

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  PRACTICAL EXAMPLES")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

;; LENS: Single field access
(struct person (name age) #:transparent)

(define name-lens
  (lens
    person-name
    (λ (p v) (struct-copy person p [name v]))))

(define alice (person "Alice" 30))

(displayln "LENS example: person name field")
(displayln (format "  Person: ~a" alice))
(displayln (format "  view name-lens: ~a" (view name-lens alice)))
(displayln (format "  modify (upcase): ~a" (modify name-lens alice string-upcase)))
(displayln "  → Single field, single focus")
(newline)

;; TRAVERSAL: Multiple elements
(define team (list (person "Alice" 30)
                   (person "Bob" 25)
                   (person "Charlie" 35)))

(define people-traversal
  (traversal
    (λ (people init fn) (foldl fn init people))
    (λ (people fn) (map fn people))))

(displayln "TRAVERSAL example: all people in team")
(displayln (format "  Team size: ~a" (length team)))
(displayln (format "  All people: ~a" (to-list-of people-traversal team)))
(displayln (format "  Total age: ~a"
                  (fold-of people-traversal team 0
                          (λ (acc p) (+ acc (person-age p))))))
(displayln (format "  All ages +1: ~a"
                  (modify-all people-traversal team
                             (λ (p) (struct-copy person p [age (add1 (person-age p))])))))
(displayln "  → Multiple elements, multiple foci")
(newline)

;; ============================================================================
;; COMPOSITION
;; ============================================================================

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  COMPOSITION")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

(displayln "Lens ∘ Lens = Lens")
(displayln "  Composing two lenses gives a lens (1 ∘ 1 = 1 focus)")
(newline)

(displayln "Lens ∘ Traversal = Traversal")
(displayln "  Lens focuses on 1, then traversal on N → N foci total")
(newline)

(displayln "Traversal ∘ Lens = Traversal")
(displayln "  Traversal gives N foci, lens focuses each → N foci")
(newline)

(displayln "Traversal ∘ Traversal = Traversal")
(displayln "  M foci, each with N foci → M×N foci total")
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔══════════════════════════════════════════════════════════╗")
(displayln "║                      SUMMARY                             ║")
(displayln "╚══════════════════════════════════════════════════════════╝")
(newline)

(displayln "LENS:")
(displayln "  • One focus")
(displayln "  • Operations: get, modify")
(displayln "  • Use: Single field access")
(displayln "  • Like: Casting (a → b)")
(newline)

(displayln "TRAVERSAL:")
(displayln "  • Zero or more foci")
(displayln "  • Operations: fold, modify, elements")
(displayln "  • Use: Collection operations")
(displayln "  • Like: Mapping (map f list)")
(newline)

(displayln "RELATIONSHIP:")
(displayln "  Lens → Traversal  (always valid, like upcasting)")
(displayln "  Traversal → Lens  (NOT valid, may have ≠1 foci)")
(newline)

(displayln "ANALOGY:")
(displayln "  Lenses : Traversals  ::  Casting : Mapping")
(displayln "  Single : Multiple    ::  One    : Many")
(newline)

(displayln "═══════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 045: Optics Summary")
(displayln "═══════════════════════════════════════════════════════════")
