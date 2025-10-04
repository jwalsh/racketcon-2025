#lang racket

;; Match, Mutation, and Lenses
;; Addressing: How does match work? What about set! and missing values?

(provide (all-defined-out))

;; ============================================================================
;; HOW MATCH WORKS
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║              HOW MATCH WORKS IN RACKET                       ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "match is PATTERN MATCHING - deconstruct and bind in one step")
(newline)

;; ============================================================================
;; BASIC MATCH EXAMPLES
;; ============================================================================

(displayln "═══ BASIC MATCH ═══")
(newline)

(define (describe-list lst)
  (match lst
    ['() "empty list"]
    [(list x) (format "single element: ~a" x)]
    [(list x y) (format "two elements: ~a and ~a" x y)]
    [(list x y z) (format "three elements: ~a, ~a, ~a" x y z)]
    [_ "many elements"]))

(displayln "Example: pattern match on list structure")
(displayln (format "  ~a → ~a" '() (describe-list '())))
(displayln (format "  ~a → ~a" '(1) (describe-list '(1))))
(displayln (format "  ~a → ~a" '(1 2) (describe-list '(1 2))))
(displayln (format "  ~a → ~a" '(1 2 3) (describe-list '(1 2 3))))
(displayln (format "  ~a → ~a" '(1 2 3 4) (describe-list '(1 2 3 4))))
(newline)

;; ============================================================================
;; MATCH WITH STRUCTS
;; ============================================================================

(displayln "═══ MATCH WITH STRUCTS ═══")
(newline)

(struct posn (x y) #:transparent)

(define (classify-point p)
  (match p
    [(posn 0 0) "origin"]
    [(posn x 0) (format "on x-axis at ~a" x)]
    [(posn 0 y) (format "on y-axis at ~a" y)]
    [(posn x y) #:when (= x y) (format "on diagonal at (~a, ~a)" x y)]
    [(posn x y) (format "general point (~a, ~a)" x y)]))

(displayln "Example: pattern match on posn struct")
(displayln (format "  ~a → ~a" (posn 0 0) (classify-point (posn 0 0))))
(displayln (format "  ~a → ~a" (posn 5 0) (classify-point (posn 5 0))))
(displayln (format "  ~a → ~a" (posn 0 3) (classify-point (posn 0 3))))
(displayln (format "  ~a → ~a" (posn 4 4) (classify-point (posn 4 4))))
(displayln (format "  ~a → ~a" (posn 3 7) (classify-point (posn 3 7))))
(newline)

;; ============================================================================
;; MATCH FOR DESTRUCTURING
;; ============================================================================

(displayln "═══ MATCH FOR DESTRUCTURING ═══")
(newline)

(define (distance-from-origin p)
  (match p
    [(posn x y)
     (sqrt (+ (* x x) (* y y)))]))

(displayln "Example: extract x and y in one step")
(define p1 (posn 3 4))
(displayln (format "  Point: ~a" p1))
(displayln (format "  Distance: ~a" (distance-from-origin p1)))
(newline)

(displayln "Without match (verbose):")
(displayln "  (define x (posn-x p1))")
(displayln "  (define y (posn-y p1))")
(displayln "  (sqrt (+ (* x x) (* y y)))")
(newline)

(displayln "With match (concise):")
(displayln "  (match p1")
(displayln "    [(posn x y)")
(displayln "     (sqrt (+ (* x x) (* y y)))])")
(newline)

;; ============================================================================
;; MUTATION: set! and set-posn-x!
;; ============================================================================

(displayln "═══ MUTATION: set! and Mutable Structs ═══")
(newline)

(displayln "Question: Could we use set! for x in posn?")
(displayln "Answer:   Only if the struct is MUTABLE")
(newline)

;; Immutable struct (default)
(struct immutable-posn (x y) #:transparent)

;; Mutable struct
(struct mutable-posn (x y) #:transparent #:mutable)

(displayln "Two kinds of structs:")
(displayln "  1. IMMUTABLE: (struct posn (x y))")
(displayln "  2. MUTABLE:   (struct posn (x y) #:mutable)")
(newline)

;; ============================================================================
;; IMMUTABLE STRUCT: No set!
;; ============================================================================

(displayln "═══ IMMUTABLE STRUCT ═══")
(newline)

(define p-imm (immutable-posn 10 20))

(displayln "Immutable posn:")
(displayln (format "  ~a" p-imm))
(newline)

(displayln "Can we use (set! (immutable-posn-x p-imm) 99)?")
(displayln "  NO! Error: 'not an identifier for mutating'")
(newline)

(displayln "Solution: Create NEW posn with struct-copy")
(define p-imm-updated (struct-copy immutable-posn p-imm [x 99]))
(displayln (format "  Original: ~a" p-imm))
(displayln (format "  Updated:  ~a" p-imm-updated))
(displayln "  → Original unchanged! Immutable data.")
(newline)

;; ============================================================================
;; MUTABLE STRUCT: Can use set!
;; ============================================================================

(displayln "═══ MUTABLE STRUCT ═══")
(newline)

(define p-mut (mutable-posn 10 20))

(displayln "Mutable posn:")
(displayln (format "  Before: ~a" p-mut))

(set-mutable-posn-x! p-mut 99)

(displayln (format "  After:  ~a" p-mut))
(displayln "  → Original CHANGED! Mutation happened.")
(newline)

(displayln "Mutable structs provide:")
(displayln "  • set-<struct>-<field>! for each field")
(displayln "  • Direct mutation of the struct")
(newline)

;; ============================================================================
;; LENSES vs MUTATION
;; ============================================================================

(displayln "═══ LENSES vs MUTATION ═══")
(newline)

(displayln "MUTATION (mutable struct):")
(displayln "  • Changes the TARGET in place")
(displayln "  • Original object is modified")
(displayln "  • set-posn-x! mutates")
(newline)

(displayln "LENSES (immutable struct):")
(displayln "  • Returns NEW target")
(displayln "  • Original object unchanged")
(displayln "  • Uses struct-copy internally")
(newline)

(struct lens (getter setter) #:transparent)

(define x-lens
  (lens
    immutable-posn-x
    (λ (p new-x) (struct-copy immutable-posn p [x new-x]))))

(define p-orig (immutable-posn 5 10))

(displayln "Example: x-lens on immutable posn")
(displayln (format "  Original: ~a" p-orig))

(define p-updated ((lens-setter x-lens) p-orig 50))

(displayln (format "  Updated:  ~a" p-updated))
(displayln (format "  Original still: ~a" p-orig))
(displayln "  → Lens creates NEW value, preserves original")
(newline)

;; ============================================================================
;; WHAT IF YOU DON'T HAVE THE POSN?
;; ============================================================================

(displayln "═══ WHAT IF YOU DON'T HAVE THE POSN? ═══")
(newline)

(displayln "Question: What if the posn is missing or optional?")
(displayln "Answer:   Use PRISM (0 or 1 focus) or Maybe type")
(newline)

;; Optional posn example
(struct player (name score position) #:transparent)

(define player-1 (player "Alice" 100 (posn 10 20)))
(define player-2 (player "Bob" 50 #f))  ; No position

(displayln "Players:")
(displayln (format "  ~a" player-1))
(displayln (format "  ~a (no position)" player-2))
(newline)

;; ============================================================================
;; PRISM: 0 or 1 FOCUS
;; ============================================================================

(struct prism (match? getter setter) #:transparent)

(define position-prism
  (prism
    ; match?: does player have a position?
    (λ (player) (and (player-position player) #t))
    ; getter: extract position (assumes it exists)
    player-position
    ; setter: update position
    (λ (player new-pos) (struct-copy player player [position new-pos]))))

(define (view-prism p target [default #f])
  (if ((prism-match? p) target)
      ((prism-getter p) target)
      default))

(define (set-prism p target value)
  (if ((prism-match? p) target)
      ((prism-setter p) target value)
      target))

(displayln "Using position-prism:")
(newline)

(displayln "Player 1 (has position):")
(displayln (format "  Has position? ~a" ((prism-match? position-prism) player-1)))
(displayln (format "  View: ~a" (view-prism position-prism player-1)))
(define player-1-moved (set-prism position-prism player-1 (posn 15 25)))
(displayln (format "  After move: ~a" player-1-moved))
(newline)

(displayln "Player 2 (no position):")
(displayln (format "  Has position? ~a" ((prism-match? position-prism) player-2)))
(displayln (format "  View: ~a" (view-prism position-prism player-2)))
(define player-2-moved (set-prism position-prism player-2 (posn 5 5)))
(displayln (format "  After move: ~a" player-2-moved))
(displayln "  → No change! Prism has no focus.")
(newline)

;; ============================================================================
;; LOSING THE CONNECTION: TARGET and FOCUS
;; ============================================================================

(displayln "═══ LOSING THE CONNECTION: TARGET ↔ FOCUS ═══")
(newline)

(displayln "Question: What if you lose the connection back to TARGET?")
(displayln "Answer:   This is the KEY problem lenses solve!")
(newline)

(displayln "WITHOUT LENSES (manual):")
(displayln "  1. Extract the focus: x = posn-x(p)")
(displayln "  2. Modify it: new-x = x + 10")
(displayln "  3. PROBLEM: How do we get back to the target?")
(displayln "     → We lost the connection!")
(displayln "     → We need to manually rebuild: struct-copy")
(newline)

(define manual-p (immutable-posn 5 10))
(define manual-x (immutable-posn-x manual-p))
(define manual-new-x (+ manual-x 10))
; NOW WHAT? We have 15, but how to create new posn?
; We need: (struct-copy immutable-posn manual-p [x manual-new-x])

(displayln "Example:")
(displayln (format "  1. Extract: x = ~a" manual-x))
(displayln (format "  2. Modify: new-x = ~a" manual-new-x))
(displayln "  3. Rebuild: (struct-copy immutable-posn p [x new-x])")
(displayln (format "     Result: ~a" (struct-copy immutable-posn manual-p [x manual-new-x])))
(newline)

(displayln "WITH LENSES (automatic):")
(displayln "  • Lens REMEMBERS the path back to target")
(displayln "  • view extracts focus")
(displayln "  • set/over updates focus AND rebuilds target")
(displayln "  • NO lost connection!")
(newline)

(define lens-p (immutable-posn 5 10))

(displayln "Example:")
(displayln (format "  Target: ~a" lens-p))
(displayln (format "  Focus:  ~a" ((lens-getter x-lens) lens-p)))

(define lens-updated
  ((lens-setter x-lens) lens-p (+ ((lens-getter x-lens) lens-p) 10)))

(displayln (format "  Updated: ~a" lens-updated))
(displayln "  → Lens handles the connection automatically!")
(newline)

;; ============================================================================
;; NESTED EXAMPLE: Why Connection Matters
;; ============================================================================

(displayln "═══ NESTED STRUCTURES: Connection Crucial ═══")
(newline)

(struct rect (top-left width height) #:transparent)

(define my-rect (rect (immutable-posn 10 20) 50 30))

(displayln "Structure: rect contains posn")
(displayln (format "  ~a" my-rect))
(newline)

(displayln "WITHOUT LENSES (manual path):")
(displayln "  1. Get posn: p = rect-top-left(r)")
(displayln "  2. Get x: x = posn-x(p)")
(displayln "  3. Update x: new-x = x + 5")
(displayln "  4. Build new posn: new-p = struct-copy(p, [x new-x])")
(displayln "  5. Build new rect: new-r = struct-copy(r, [top-left new-p])")
(displayln "  → FIVE STEPS! Easy to mess up!")
(newline)

(define manual-rect my-rect)
(define manual-posn (rect-top-left manual-rect))
(define manual-x-2 (immutable-posn-x manual-posn))
(define manual-new-x-2 (+ manual-x-2 5))
(define manual-new-posn (struct-copy immutable-posn manual-posn [x manual-new-x-2]))
(define manual-new-rect (struct-copy rect manual-rect [top-left manual-new-posn]))

(displayln (format "  Before: ~a" manual-rect))
(displayln (format "  After:  ~a" manual-new-rect))
(newline)

(displayln "WITH LENSES (composed):")

(define top-left-lens
  (lens
    rect-top-left
    (λ (r new-tl) (struct-copy rect r [top-left new-tl]))))

(define (compose-lens outer inner)
  (lens
    (λ (t) ((lens-getter inner) ((lens-getter outer) t)))
    (λ (t v) ((lens-setter outer) t ((lens-setter inner) ((lens-getter outer) t) v)))))

(define rect-x-lens (compose-lens top-left-lens x-lens))

(define lens-rect my-rect)
(define lens-new-rect
  ((lens-setter rect-x-lens) lens-rect
   (+ ((lens-getter rect-x-lens) lens-rect) 5)))

(displayln "  1. Compose: rect-x-lens = top-left-lens ∘ x-lens")
(displayln "  2. Update: set rect-x-lens r (+ (view rect-x-lens r) 5)")
(displayln "  → TWO STEPS! Lens handles the path.")
(newline)

(displayln (format "  Before: ~a" lens-rect))
(displayln (format "  After:  ~a" lens-new-rect))
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "Q1: How does match work?")
(displayln "A:  Pattern matching - deconstruct and bind in one step")
(displayln "    (match p [(posn x y) (+ x y)])")
(newline)

(displayln "Q2: Can we use set! on posn-x?")
(displayln "A:  Only if #:mutable. Otherwise, use struct-copy or lenses")
(newline)

(displayln "Q3: What if posn is missing?")
(displayln "A:  Use PRISM (0 or 1 focus). It handles the optional case.")
(newline)

(displayln "Q4: What about losing the connection to target?")
(displayln "A:  THIS IS WHY LENSES EXIST!")
(displayln "    • Manual: extract → modify → lost connection → rebuild")
(displayln "    • Lens: view → modify → automatic rebuild")
(displayln "    • Lens remembers the path back to target")
(newline)

(displayln "KEY INSIGHT:")
(displayln "  Lenses solve the 'lost connection' problem")
(displayln "  • getter: TARGET → FOCUS")
(displayln "  • setter: TARGET × FOCUS → TARGET")
(displayln "  • The setter rebuilds the entire target with new focus")
(displayln "  • No lost connection!")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 045: Match and Mutation")
(displayln "═══════════════════════════════════════════════════════════════")
