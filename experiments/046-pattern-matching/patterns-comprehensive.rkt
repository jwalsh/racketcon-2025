#lang racket

;; Pattern Matching: Comprehensive Guide
;; Showing how patterns relate to lenses and optics

(provide (all-defined-out))

;; ============================================================================
;; PATTERN MATCHING FUNDAMENTALS
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         PATTERN MATCHING: PATTERNS AND LENSES                ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "Key insight:")
(displayln "  • PATTERN: deconstructs and binds")
(displayln "  • LENS: focuses and updates")
(displayln "  • Pattern matching is like a read-only lens")
(newline)

;; ============================================================================
;; PATTERN SYNTAX
;; ============================================================================

(displayln "═══ PATTERN SYNTAX ═══")
(newline)

(displayln "Pattern types:")
(displayln "  _          : wildcard (matches anything, doesn't bind)")
(displayln "  var        : variable (binds to matched value)")
(displayln "  42         : literal (matches exact value)")
(displayln "  \"hello\"     : literal string")
(displayln "  'symbol    : literal symbol")
(displayln "  (list p1 p2) : list with specific patterns")
(displayln "  (cons p1 p2) : cons cell pattern")
(displayln "  (struct-name p1 p2) : struct pattern")
(displayln "  (? pred? p) : predicate pattern")
(newline)

;; ============================================================================
;; BASIC PATTERNS
;; ============================================================================

(displayln "═══ BASIC PATTERNS ═══")
(newline)

(define (classify-value v)
  (match v
    [0 "zero"]
    [1 "one"]
    [42 "the answer"]
    [_ "something else"]))

(displayln "Literal patterns:")
(displayln (format "  ~a → ~a" 0 (classify-value 0)))
(displayln (format "  ~a → ~a" 1 (classify-value 1)))
(displayln (format "  ~a → ~a" 42 (classify-value 42)))
(displayln (format "  ~a → ~a" 99 (classify-value 99)))
(newline)

;; ============================================================================
;; LIST PATTERNS
;; ============================================================================

(displayln "═══ LIST PATTERNS ═══")
(newline)

(displayln "Example: (list a b c) = (cons a (cons b (cons c '())))")
(newline)

(define test-list '(1 2 3))

(displayln "Different ways to match the same list:")
(newline)

(displayln "Pattern 1: (list a b c)")
(match test-list
  [(list a b c)
   (displayln (format "  Matched! a=~a b=~a c=~a" a b c))])
(newline)

(displayln "Pattern 2: (cons a (cons b (cons c '())))")
(match test-list
  [(cons a (cons b (cons c '())))
   (displayln (format "  Matched! a=~a b=~a c=~a" a b c))])
(newline)

(displayln "Pattern 3: Mixed")
(match test-list
  [(cons a (list b c))
   (displayln (format "  Matched! a=~a b=~a c=~a" a b c))])
(newline)

(displayln "These are EQUIVALENT patterns!")
(displayln "  (list a b c)")
(displayln "  (cons a (cons b (cons c '())))")
(displayln "  (cons a (list b c))")
(newline)

;; ============================================================================
;; VARIABLE BINDING
;; ============================================================================

(displayln "═══ VARIABLE BINDING ═══")
(newline)

(define (sum-pair p)
  (match p
    [(cons a b)
     (displayln (format "  First: ~a, Second: ~a" a b))
     (+ a b)]))

(displayln "Pattern (cons a b) binds two variables:")
(displayln (format "  ~a → sum = ~a" '(10 . 20) (sum-pair '(10 . 20))))
(newline)

;; ============================================================================
;; STRUCT PATTERNS
;; ============================================================================

(displayln "═══ STRUCT PATTERNS ═══")
(newline)

(struct posn (x y) #:transparent)

(define (describe-posn p)
  (match p
    [(posn 0 0) "origin"]
    [(posn x 0) (format "x-axis at x=~a" x)]
    [(posn 0 y) (format "y-axis at y=~a" y)]
    [(posn x y) (format "point (~a, ~a)" x y)]))

(displayln "Struct pattern: (posn x y)")
(displayln (format "  ~a → ~a" (posn 0 0) (describe-posn (posn 0 0))))
(displayln (format "  ~a → ~a" (posn 5 0) (describe-posn (posn 5 0))))
(displayln (format "  ~a → ~a" (posn 0 3) (describe-posn (posn 0 3))))
(displayln (format "  ~a → ~a" (posn 3 4) (describe-posn (posn 3 4))))
(newline)

;; ============================================================================
;; NESTED PATTERNS
;; ============================================================================

(displayln "═══ NESTED PATTERNS ═══")
(newline)

(struct rect (top-left width height) #:transparent)

(define (rect-at-origin? r)
  (match r
    [(rect (posn 0 0) _ _) #t]
    [_ #f]))

(displayln "Nested pattern: (rect (posn 0 0) _ _)")

(define r1 (rect (posn 0 0) 10 20))
(define r2 (rect (posn 5 5) 10 20))

(displayln (format "  ~a → ~a" r1 (rect-at-origin? r1)))
(displayln (format "  ~a → ~a" r2 (rect-at-origin? r2)))
(newline)

;; ============================================================================
;; PATTERNS AS READ-ONLY LENSES
;; ============================================================================

(displayln "═══ PATTERNS AS READ-ONLY LENSES ═══")
(newline)

(displayln "Pattern (cons a b):")
(displayln "  • Extracts first and second")
(displayln "  • Like two lenses: car-lens and cdr-lens")
(displayln "  • But read-only!")
(newline)

(struct lens (getter setter) #:transparent)

(define car-lens
  (lens
    car
    (λ (lst v) (cons v (cdr lst)))))

(define cdr-lens
  (lens
    cdr
    (λ (lst v) (cons (car lst) v))))

(define test-pair '(10 . 20))

(displayln "Using match (read-only):")
(match test-pair
  [(cons a b)
   (displayln (format "  a = ~a, b = ~a" a b))])
(newline)

(displayln "Using lenses (can update):")
(displayln (format "  car-lens view: ~a" ((lens-getter car-lens) test-pair)))
(displayln (format "  cdr-lens view: ~a" ((lens-getter cdr-lens) test-pair)))
(displayln (format "  set car to 99: ~a" ((lens-setter car-lens) test-pair 99)))
(displayln (format "  set cdr to 88: ~a" ((lens-setter cdr-lens) test-pair 88)))
(newline)

;; ============================================================================
;; DUPDATE: Pattern-Based Update
;; ============================================================================

(displayln "═══ DUPDATE: Pattern + Lens Hybrid ═══")
(newline)

(displayln "Concept: Use pattern syntax for updates")
(displayln "  (dupdate data pattern new-value)")
(newline)

(define (dupdate-car lst new-value)
  "Update first element of pair/list."
  (match lst
    [(cons _ rest) (cons new-value rest)]))

(define (dupdate-cdr lst new-value)
  "Update second element of pair."
  (match lst
    [(cons first _) (cons first new-value)]))

(displayln "dupdate-car: like [car-lens, new-value]")
(displayln (format "  ~a → ~a" test-pair (dupdate-car test-pair 99)))
(newline)

(displayln "dupdate-cdr: like [cdr-lens, new-value]")
(displayln (format "  ~a → ~a" test-pair (dupdate-cdr test-pair 88)))
(newline)

;; ============================================================================
;; GENERAL DUPDATE CONCEPT
;; ============================================================================

(displayln "═══ GENERAL DUPDATE CONCEPT ═══")
(newline)

(displayln "Mike Delmonaco's DSL idea:")
(displayln "  (dupdate data [pattern expr])")
(newline)

(displayln "Examples:")
(displayln "  (dupdate pair [car-lens (* 2 a)])")
(displayln "  (dupdate rect [top-left.x (+ x 10)])")
(displayln "  (dupdate tree [left.right.value 42])")
(newline)

(displayln "This is like:")
(displayln "  • Pattern: identifies FOCUS")
(displayln "  • Expression: computes new value")
(displayln "  • Result: updated TARGET")
(newline)

;; ============================================================================
;; COMPARISON TABLE
;; ============================================================================

(displayln "═══ PATTERNS vs LENSES vs DUPDATE ═══")
(newline)

(displayln "┌─────────────┬──────────────┬──────────────┬──────────────┐")
(displayln "│ Feature     │ Pattern      │ Lens         │ dupdate      │")
(displayln "├─────────────┼──────────────┼──────────────┼──────────────┤")
(displayln "│ Extract     │ Yes          │ Yes (view)   │ Yes          │")
(displayln "│ Update      │ No           │ Yes (set)    │ Yes          │")
(displayln "│ Syntax      │ Declarative  │ Programmatic │ Declarative  │")
(displayln "│ Compose     │ Nested       │ compose-lens │ Dot notation │")
(displayln "│ Use case    │ Dispatch     │ General      │ Deep updates │")
(displayln "└─────────────┴──────────────┴──────────────┴──────────────┘")
(newline)

;; ============================================================================
;; PRACTICAL EXAMPLE: All Three
;; ============================================================================

(displayln "═══ PRACTICAL EXAMPLE: All Three ═══")
(newline)

(struct person (name age address) #:transparent)
(struct address (street city state) #:transparent)

(define alice
  (person "Alice" 30
          (address "123 Main" "Boston" "MA")))

(displayln "Data:")
(displayln (format "  ~a" alice))
(newline)

;; Using match (read-only)
(displayln "1. PATTERN (read-only):")
(match alice
  [(person name age (address street city state))
   (displayln (format "   Name: ~a, City: ~a" name city))])
(newline)

;; Using lenses (update)
(displayln "2. LENS (update):")

(define name-lens
  (lens
    person-name
    (λ (p v) (struct-copy person p [name v]))))

(define address-lens
  (lens
    person-address
    (λ (p v) (struct-copy person p [address v]))))

(define city-lens
  (lens
    address-city
    (λ (a v) (struct-copy address a [city v]))))

(define (compose-lens outer inner)
  (lens
    (λ (t) ((lens-getter inner) ((lens-getter outer) t)))
    (λ (t v) ((lens-setter outer) t
              ((lens-setter inner) ((lens-getter outer) t) v)))))

(define person-city-lens (compose-lens address-lens city-lens))

(displayln (format "   City: ~a" ((lens-getter person-city-lens) alice)))

(define alice-moved
  ((lens-setter person-city-lens) alice "Cambridge"))

(displayln (format "   Moved: ~a" alice-moved))
(newline)

;; Conceptual dupdate
(displayln "3. DUPDATE (conceptual DSL):")
(displayln "   Syntax: (dupdate alice [address.city \\\"Cambridge\\\"])")
(displayln "   This would be equivalent to the lens approach")
(displayln "   But with declarative pattern-like syntax!")
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "PATTERN MATCHING:")
(displayln "  • Deconstructs data")
(displayln "  • Binds variables")
(displayln "  • Read-only")
(displayln "  • Great for dispatch")
(newline)

(displayln "LENSES:")
(displayln "  • Focus on part")
(displayln "  • Can read (view) and write (set)")
(displayln "  • Compose well")
(displayln "  • Programmatic")
(newline)

(displayln "DUPDATE (conceptual):")
(displayln "  • Pattern-like syntax")
(displayln "  • For updates")
(displayln "  • Declarative")
(displayln "  • Best of both worlds")
(newline)

(displayln "KEY RELATIONSHIPS:")
(displayln "  • (list a b c) = (cons a (cons b (cons c '())))")
(displayln "  • Pattern deconstruction ~ Lens view")
(displayln "  • dupdate [car-lens v] ~ set car-lens target v")
(displayln "  • Patterns are read-only lenses")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 046: Pattern Matching")
(displayln "═══════════════════════════════════════════════════════════════")
