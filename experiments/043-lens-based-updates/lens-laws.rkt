#lang racket

;; Lens Laws: The Three Fundamental Properties
;; Demonstrating why lenses must satisfy these laws

(require "lens-struct-example.rkt")

;; ============================================================================
;; THE THREE LENS LAWS
;; ============================================================================

(displayln "╔════════════════════════════════════════════════════════════════╗")
(displayln "║                    THE THREE LENS LAWS                        ║")
(displayln "╚════════════════════════════════════════════════════════════════╝")
(newline)

;; ============================================================================
;; LAW 1: PUT-GET (You get what you put)
;; ============================================================================

(displayln "═══ LAW 1: PUT-GET (You Get What You Put) ═══\n")

(displayln "Formal: get(set(target, focus)) = focus")
(displayln "Racket: (view l (set l target focus)) ≡ focus")
(newline)

(displayln "Intuition:")
(displayln "  If you SET a value, then immediately GET it back,")
(displayln "  you should receive exactly what you just SET.")
(newline)

;; Example 1: Hash lens
(displayln "Example 1: Hash field")
(define person (hash 'name "Alice" 'age 30))
(define name-lens (hash-lens 'name))

(define focus-value "Bob")
(displayln (format "  target:      ~a" person))
(displayln (format "  focus:       ~a" focus-value))
(displayln (format "  set(t, f):   ~a" (set name-lens person focus-value)))
(displayln (format "  get(set(t, f)): ~a" (view name-lens (set name-lens person focus-value))))
(displayln (format "  f:           ~a" focus-value))
(displayln (format "  Law holds?   ~a" (equal? (view name-lens (set name-lens person focus-value))
                                                focus-value)))
(newline)

;; Example 2: Posn struct
(struct posn (x y) #:transparent)

(define posn-x-lens
  (lens posn-x
        (λ (p v) (struct-copy posn p [x v]))))

(displayln "Example 2: Struct field")
(define p (posn 10 20))
(define new-x 99)
(displayln (format "  target:      ~a" p))
(displayln (format "  focus:       ~a" new-x))
(displayln (format "  set(t, f):   ~a" (set posn-x-lens p new-x)))
(displayln (format "  get(set(t, f)): ~a" (view posn-x-lens (set posn-x-lens p new-x))))
(displayln (format "  f:           ~a" new-x))
(displayln (format "  Law holds?   ~a" (equal? (view posn-x-lens (set posn-x-lens p new-x))
                                                new-x)))
(newline)

;; What happens if this law is violated?
(displayln "What if this law is violated?")
(displayln "  Bad lens: Sets a value but returns something different when viewed")
(displayln "  Example: (set l t \"Alice\") but (view l result) => \"ALICE\"")
(displayln "  Problem: Non-deterministic behavior, can't trust the lens")
(newline)

;; ============================================================================
;; LAW 2: GET-PUT (Setting what you got does nothing)
;; ============================================================================

(displayln "═══ LAW 2: GET-PUT (Setting What You Got Does Nothing) ═══\n")

(displayln "Formal: set(target, get(target)) = target")
(displayln "Racket: (set l target (view l target)) ≡ target")
(newline)

(displayln "Intuition:")
(displayln "  If you GET the current value, then SET it back unchanged,")
(displayln "  the target should remain identical (structural equality).")
(newline)

;; Example 1: Hash lens
(displayln "Example 1: Hash field")
(displayln (format "  target:           ~a" person))
(displayln (format "  get(t):           ~a" (view name-lens person)))
(displayln (format "  set(t, get(t)):   ~a" (set name-lens person (view name-lens person))))
(displayln (format "  target:           ~a" person))
(displayln (format "  Law holds?        ~a" (equal? (set name-lens person (view name-lens person))
                                                     person)))
(newline)

;; Example 2: Posn struct
(displayln "Example 2: Struct field")
(displayln (format "  target:           ~a" p))
(displayln (format "  get(t):           ~a" (view posn-x-lens p)))
(displayln (format "  set(t, get(t)):   ~a" (set posn-x-lens p (view posn-x-lens p))))
(displayln (format "  target:           ~a" p))
(displayln (format "  Law holds?        ~a" (equal? (set posn-x-lens p (view posn-x-lens p))
                                                     p)))
(newline)

;; What happens if this law is violated?
(displayln "What if this law is violated?")
(displayln "  Bad lens: Changes other fields when setting current value")
(displayln "  Example: Setting 'name also resets 'age to 0")
(displayln "  Problem: Unexpected side effects, violates locality")
(newline)

;; ============================================================================
;; LAW 3: PUT-PUT (Second set wins)
;; ============================================================================

(displayln "═══ LAW 3: PUT-PUT (Second Set Wins) ═══\n")

(displayln "Formal: set(set(target, f1), f2) = set(target, f2)")
(displayln "Racket: (set l (set l target f1) f2) ≡ (set l target f2)")
(newline)

(displayln "Intuition:")
(displayln "  If you SET a value twice in a row, only the second SET matters.")
(displayln "  The first SET is completely overwritten.")
(newline)

;; Example 1: Hash lens
(displayln "Example 1: Hash field")
(define f1 "Bob")
(define f2 "Charlie")
(displayln (format "  target:                ~a" person))
(displayln (format "  f1:                    ~a" f1))
(displayln (format "  f2:                    ~a" f2))
(displayln (format "  set(set(t, f1), f2):   ~a" (set name-lens (set name-lens person f1) f2)))
(displayln (format "  set(t, f2):            ~a" (set name-lens person f2)))
(displayln (format "  Law holds?             ~a" (equal? (set name-lens (set name-lens person f1) f2)
                                                          (set name-lens person f2))))
(newline)

;; Example 2: Posn struct
(displayln "Example 2: Struct field")
(define x1 50)
(define x2 100)
(displayln (format "  target:                ~a" p))
(displayln (format "  f1:                    ~a" x1))
(displayln (format "  f2:                    ~a" x2))
(displayln (format "  set(set(t, f1), f2):   ~a" (set posn-x-lens (set posn-x-lens p x1) x2)))
(displayln (format "  set(t, f2):            ~a" (set posn-x-lens p x2)))
(displayln (format "  Law holds?             ~a" (equal? (set posn-x-lens (set posn-x-lens p x1) x2)
                                                          (set posn-x-lens p x2))))
(newline)

;; What happens if this law is violated?
(displayln "What if this law is violated?")
(displayln "  Bad lens: Accumulates multiple sets instead of replacing")
(displayln "  Example: Setting twice appends values instead of replacing")
(displayln "  Problem: Stateful behavior, order-dependent updates")
(newline)

;; ============================================================================
;; WHY THESE LAWS MATTER
;; ============================================================================

(displayln "╔════════════════════════════════════════════════════════════════╗")
(displayln "║                  WHY THESE LAWS MATTER                        ║")
(displayln "╚════════════════════════════════════════════════════════════════╝")
(newline)

(displayln "1. COMPOSABILITY")
(displayln "   - Laws ensure composed lenses also obey the laws")
(displayln "   - (compose-lens l1 l2) is a valid lens")
(newline)

(displayln "2. PREDICTABILITY")
(displayln "   - Get/set operations behave as expected")
(displayln "   - No hidden state or side effects")
(newline)

(displayln "3. REASONING")
(displayln "   - Can perform algebraic reasoning about code")
(displayln "   - Refactoring preserves semantics")
(newline)

(displayln "4. OPTIMIZATION")
(displayln "   - Compiler can apply transformations safely")
(displayln "   - Redundant operations can be eliminated")
(newline)

;; ============================================================================
;; TESTING LENS LAWS
;; ============================================================================

(displayln "╔════════════════════════════════════════════════════════════════╗")
(displayln "║                 TESTING LENS LAWS                             ║")
(displayln "╚════════════════════════════════════════════════════════════════╝")
(newline)

(define (test-lens-laws lens-name l target focus1 focus2)
  "Test all three lens laws for a given lens."
  (displayln (format "Testing: ~a" lens-name))
  (displayln (format "  Target: ~a" target))
  (newline)

  ;; Law 1: PUT-GET
  (define law1-holds?
    (equal? (view l (set l target focus1)) focus1))
  (displayln (format "  Law 1 (PUT-GET):  ~a" law1-holds?))
  (when (not law1-holds?)
    (displayln (format "    Expected: ~a" focus1))
    (displayln (format "    Got:      ~a" (view l (set l target focus1)))))

  ;; Law 2: GET-PUT
  (define law2-holds?
    (equal? (set l target (view l target)) target))
  (displayln (format "  Law 2 (GET-PUT):  ~a" law2-holds?))
  (when (not law2-holds?)
    (displayln (format "    Expected: ~a" target))
    (displayln (format "    Got:      ~a" (set l target (view l target)))))

  ;; Law 3: PUT-PUT
  (define law3-holds?
    (equal? (set l (set l target focus1) focus2)
            (set l target focus2)))
  (displayln (format "  Law 3 (PUT-PUT):  ~a" law3-holds?))
  (when (not law3-holds?)
    (displayln (format "    Expected: ~a" (set l target focus2)))
    (displayln (format "    Got:      ~a" (set l (set l target focus1) focus2))))

  (define all-pass? (and law1-holds? law2-holds? law3-holds?))
  (displayln (format "  Overall: ~a" (if all-pass? "✓ ALL LAWS SATISFIED" "✗ LAWS VIOLATED")))
  (newline)
  all-pass?)

;; Test various lenses
(test-lens-laws "name-lens (hash)" name-lens person "Bob" "Charlie")

(test-lens-laws "posn-x-lens (struct)" posn-x-lens (posn 10 20) 50 100)

(define age-lens (hash-lens 'age))
(test-lens-laws "age-lens (hash)" age-lens person 35 40)

;; Test composed lens
(struct rect (top-left width height) #:transparent)

(define rect-top-left-lens
  (lens rect-top-left
        (λ (r v) (struct-copy rect r [top-left v]))))

(define rect-x-lens (compose-lens rect-top-left-lens posn-x-lens))

(test-lens-laws "rect-x-lens (composed)"
                rect-x-lens
                (rect (posn 10 20) 100 50)
                15
                25)

;; ============================================================================
;; BAD LENS EXAMPLES (Violating Laws)
;; ============================================================================

(displayln "╔════════════════════════════════════════════════════════════════╗")
(displayln "║              EXAMPLES OF BAD LENSES                           ║")
(displayln "╚════════════════════════════════════════════════════════════════╝")
(newline)

;; Bad lens 1: Violates PUT-GET (transforms on set)
(displayln "Bad Lens 1: Transforms value on set (violates PUT-GET)")
(define bad-name-lens-1
  (lens (λ (h) (hash-ref h 'name))
        (λ (h v) (hash-set h 'name (string-upcase v)))))  ; BUG: transforms!

(displayln "  Set 'Bob', but get returns 'BOB'")
(displayln (format "  Result: ~a" (view bad-name-lens-1 (set bad-name-lens-1 person "Bob"))))
(displayln (format "  Expected: Bob"))
(displayln (format "  Law 1 violated: ~a"
                  (not (equal? (view bad-name-lens-1 (set bad-name-lens-1 person "Bob"))
                              "Bob"))))
(newline)

;; Bad lens 2: Violates GET-PUT (has side effects)
(displayln "Bad Lens 2: Has side effects (violates GET-PUT)")
(define bad-name-lens-2
  (lens (λ (h) (hash-ref h 'name))
        (λ (h v) (hash-set (hash-set h 'name v) 'age 0))))  ; BUG: modifies age!

(displayln "  Setting name also resets age to 0")
(define person-with-age (hash 'name "Alice" 'age 30))
(displayln (format "  Original: ~a" person-with-age))
(displayln (format "  After set: ~a"
                  (set bad-name-lens-2 person-with-age (view bad-name-lens-2 person-with-age))))
(displayln (format "  Law 2 violated: ~a"
                  (not (equal? (set bad-name-lens-2 person-with-age
                                   (view bad-name-lens-2 person-with-age))
                              person-with-age))))
(newline)

;; Bad lens 3: Violates PUT-PUT (accumulates)
(displayln "Bad Lens 3: Accumulates values (violates PUT-PUT)")
(define bad-name-lens-3
  (lens (λ (h) (hash-ref h 'name))
        (λ (h v) (hash-set h 'name
                          (string-append (hash-ref h 'name) "," v)))))  ; BUG: appends!

(displayln "  Setting twice appends values instead of replacing")
(displayln (format "  set(set(t, 'Bob'), 'Charlie'): ~a"
                  (view bad-name-lens-3
                       (set bad-name-lens-3
                            (set bad-name-lens-3 person "Bob")
                            "Charlie"))))
(displayln (format "  Expected: Charlie"))
(displayln (format "  Got: Alice,Bob,Charlie"))
(displayln (format "  Law 3 violated: ~a"
                  (not (equal? (set bad-name-lens-3 (set bad-name-lens-3 person "Bob") "Charlie")
                              (set bad-name-lens-3 person "Charlie")))))
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔════════════════════════════════════════════════════════════════╗")
(displayln "║                         SUMMARY                               ║")
(displayln "╚════════════════════════════════════════════════════════════════╝")
(newline)

(displayln "The three lens laws ensure:")
(displayln "")
(displayln "  Law 1 (PUT-GET): get(set(t, f)) = f")
(displayln "          You get what you put")
(displayln "          → No transformations on set")
(displayln "")
(displayln "  Law 2 (GET-PUT): set(t, get(t)) = t")
(displayln "          Setting what you got does nothing")
(displayln "          → No side effects or extra modifications")
(displayln "")
(displayln "  Law 3 (PUT-PUT): set(set(t, f1), f2) = set(t, f2)")
(displayln "          Second set wins")
(displayln "          → No accumulation or state")
(displayln "")
(displayln "These laws make lenses:")
(displayln "  ✓ Composable")
(displayln "  ✓ Predictable")
(displayln "  ✓ Optimizable")
(displayln "  ✓ Safe to refactor")
