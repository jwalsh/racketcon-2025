#lang racket

;; The Optics Hierarchy: Classifying by Number of Foci

(provide (all-defined-out))

;; ============================================================================
;; NUMBER OF FOCI: The Optics Spectrum
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         THE OPTICS HIERARCHY: NUMBER OF FOCI                 ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

;; ============================================================================
;; 1. LENS: Exactly 1 Focus (Always Succeeds)
;; ============================================================================

(displayln "═══ LENS: Exactly 1 Focus ═══\n")

(displayln "A lens always has EXACTLY ONE focus.")
(displayln "- It always succeeds (total function)")
(displayln "- view returns exactly one value")
(displayln "- set updates exactly one location")
(newline)

(struct lens (getter setter) #:transparent)

(define (hash-lens key)
  (lens
    (λ (h) (hash-ref h key))
    (λ (h v) (hash-set h key v))))

(define person (hash 'name "Alice" 'age 30 'city "Seattle"))
(define name-lens (hash-lens 'name))

(displayln "Example: name-lens on person")
(displayln (format "  Target: ~a" person))
(displayln (format "  Foci count: 1"))
(displayln (format "  Focus: ~a" ((lens-getter name-lens) person)))
(newline)

(displayln "Visual:")
(displayln "  person")
(displayln "    ├─ name: \"Alice\"  ← FOCUS (1 value)")
(displayln "    ├─ age: 30")
(displayln "    └─ city: \"Seattle\"")
(newline)

;; ============================================================================
;; 2. PRISM: 0 or 1 Focus (Partial, May Fail)
;; ============================================================================

(displayln "═══ PRISM: 0 or 1 Focus ═══\n")

(displayln "A prism has ZERO OR ONE focus.")
(displayln "- It may fail (partial function)")
(displayln "- view returns Maybe/Option type")
(displayln "- Used for optional fields, variants, sum types")
(newline)

(struct prism (match? getter setter) #:transparent)

(define (optional-hash-prism key)
  (prism
    (λ (h) (hash-has-key? h key))
    (λ (h) (hash-ref h key))
    (λ (h v) (hash-set h key v))))

(define person-with-phone
  (hash 'name "Alice" 'age 30 'phone "555-1234"))

(define person-without-phone
  (hash 'name "Bob" 'age 25))

(define phone-prism (optional-hash-prism 'phone))

(displayln "Example: phone-prism (optional field)")
(displayln (format "  Person 1: ~a" person-with-phone))
(displayln (format "  Foci count: ~a" (if ((prism-match? phone-prism) person-with-phone) 1 0)))
(displayln (format "  Focus: ~a"
                  (if ((prism-match? phone-prism) person-with-phone)
                      ((prism-getter phone-prism) person-with-phone)
                      "None")))
(newline)

(displayln (format "  Person 2: ~a" person-without-phone))
(displayln (format "  Foci count: ~a" (if ((prism-match? phone-prism) person-without-phone) 1 0)))
(displayln (format "  Focus: ~a"
                  (if ((prism-match? phone-prism) person-without-phone)
                      ((prism-getter phone-prism) person-without-phone)
                      "None")))
(newline)

(displayln "Visual:")
(displayln "  person-with-phone")
(displayln "    ├─ name: \"Alice\"")
(displayln "    ├─ age: 30")
(displayln "    └─ phone: \"555-1234\"  ← FOCUS (1 value)")
(newline)
(displayln "  person-without-phone")
(displayln "    ├─ name: \"Bob\"")
(displayln "    └─ age: 25")
(displayln "    (no phone field)       ← FOCUS (0 values)")
(newline)

;; ============================================================================
;; 3. TRAVERSAL: 0 to N Foci (Multiple Targets)
;; ============================================================================

(displayln "═══ TRAVERSAL: 0 to N Foci ═══\n")

(displayln "A traversal has ZERO TO MANY foci.")
(displayln "- Can focus on multiple parts simultaneously")
(displayln "- view returns a list of values")
(displayln "- set/over updates all matching locations")
(newline)

(struct traversal (get-all set-all) #:transparent)

;; Traversal over list elements
(define list-traversal
  (traversal
    ; get-all: extract all elements
    (λ (lst) lst)
    ; set-all: replace all elements
    (λ (lst f) (map f lst))))

(define scores '(85 90 92 78 95))

(displayln "Example: list-traversal (all elements)")
(displayln (format "  Target: ~a" scores))
(displayln (format "  Foci count: ~a" (length ((traversal-get-all list-traversal) scores))))
(displayln (format "  Foci: ~a" ((traversal-get-all list-traversal) scores)))
(newline)

(displayln "Visual:")
(displayln "  scores")
(displayln "    ├─ [0]: 85  ← FOCUS 1")
(displayln "    ├─ [1]: 90  ← FOCUS 2")
(displayln "    ├─ [2]: 92  ← FOCUS 3")
(displayln "    ├─ [3]: 78  ← FOCUS 4")
(displayln "    └─ [4]: 95  ← FOCUS 5")
(displayln "  Total foci: 5")
(newline)

;; Filtered traversal (variable number of foci)
(define (filtered-traversal pred?)
  (traversal
    ; get-all: only matching elements
    (λ (lst) (filter pred? lst))
    ; set-all: update only matching
    (λ (lst f)
      (for/list ([item lst])
        (if (pred? item) (f item) item)))))

(define passing-traversal (filtered-traversal (λ (s) (>= s 90))))

(displayln "Example: filtered-traversal (passing grades >= 90)")
(displayln (format "  Target: ~a" scores))
(displayln (format "  Foci count: ~a" (length ((traversal-get-all passing-traversal) scores))))
(displayln (format "  Foci: ~a" ((traversal-get-all passing-traversal) scores)))
(newline)

(displayln "Visual:")
(displayln "  scores")
(displayln "    ├─ [0]: 85")
(displayln "    ├─ [1]: 90  ← FOCUS 1 (passing)")
(displayln "    ├─ [2]: 92  ← FOCUS 2 (passing)")
(displayln "    ├─ [3]: 78")
(displayln "    └─ [4]: 95  ← FOCUS 3 (passing)")
(displayln "  Total foci: 3")
(newline)

;; Empty traversal (zero foci)
(define empty-list '())
(displayln "Example: traversal on empty list")
(displayln (format "  Target: ~a" empty-list))
(displayln (format "  Foci count: ~a" (length ((traversal-get-all list-traversal) empty-list))))
(displayln (format "  Foci: ~a" ((traversal-get-all list-traversal) empty-list)))
(newline)

;; ============================================================================
;; 4. AFFINE TRAVERSAL: 0 or 1 Focus (Like Prism)
;; ============================================================================

(displayln "═══ AFFINE TRAVERSAL: 0 or 1 Focus ═══\n")

(displayln "An affine traversal has AT MOST ONE focus.")
(displayln "- Combines lens (at most 1) + traversal (may be 0)")
(displayln "- Same as prism in practice")
(displayln "- Example: first element of list (may be empty)")
(newline)

(define first-traversal
  (traversal
    (λ (lst) (if (null? lst) '() (list (car lst))))
    (λ (lst f) (if (null? lst) lst (cons (f (car lst)) (cdr lst))))))

(define non-empty-list '(1 2 3))
(displayln "Example: first-traversal on non-empty list")
(displayln (format "  Target: ~a" non-empty-list))
(displayln (format "  Foci count: ~a" (length ((traversal-get-all first-traversal) non-empty-list))))
(displayln (format "  Focus: ~a" ((traversal-get-all first-traversal) non-empty-list)))
(newline)

(define empty-list-2 '())
(displayln "Example: first-traversal on empty list")
(displayln (format "  Target: ~a" empty-list-2))
(displayln (format "  Foci count: ~a" (length ((traversal-get-all first-traversal) empty-list-2))))
(displayln (format "  Focus: ~a" ((traversal-get-all first-traversal) empty-list-2)))
(newline)

;; ============================================================================
;; 5. FOLD: 0 to N Foci (Read-Only)
;; ============================================================================

(displayln "═══ FOLD: 0 to N Foci (Read-Only) ═══\n")

(displayln "A fold has ZERO TO MANY foci, but is READ-ONLY.")
(displayln "- Like traversal, but cannot set")
(displayln "- Used for viewing/extracting multiple values")
(displayln "- Example: all values in a tree")
(newline)

(struct fold (get-all) #:transparent)

;; Fold over all hash values
(define hash-values-fold
  (fold
    (λ (h) (hash-values h))))

(displayln "Example: hash-values-fold")
(displayln (format "  Target: ~a" person))
(displayln (format "  Foci count: ~a" (length ((fold-get-all hash-values-fold) person))))
(displayln (format "  Foci: ~a" ((fold-get-all hash-values-fold) person)))
(newline)

;; ============================================================================
;; SUMMARY TABLE
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                    OPTICS SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "┌──────────────────┬─────────────┬───────────┬──────────────┐")
(displayln "│ Optic Type       │ # of Foci   │ Can Set?  │ Always Succeeds?│")
(displayln "├──────────────────┼─────────────┼───────────┼──────────────┤")
(displayln "│ Lens             │ Exactly 1   │ Yes       │ Yes          │")
(displayln "│ Prism            │ 0 or 1      │ Yes       │ No           │")
(displayln "│ Affine Traversal │ 0 or 1      │ Yes       │ No           │")
(displayln "│ Traversal        │ 0 to N      │ Yes       │ Yes*         │")
(displayln "│ Fold             │ 0 to N      │ No        │ Yes*         │")
(displayln "│ Getter           │ Exactly 1   │ No        │ Yes          │")
(displayln "│ Setter           │ N/A         │ Yes       │ Yes          │")
(displayln "└──────────────────┴─────────────┴───────────┴──────────────┘")
(displayln "* May return empty list")
(newline)

;; ============================================================================
;; PRACTICAL EXAMPLES
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║              PRACTICAL EXAMPLES BY FOCUS COUNT               ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

;; 1 focus: Lens
(displayln "1 FOCUS: Update single employee's salary")
(define company
  (hash 'name "Tech Corp"
        'ceo (hash 'name "Alice" 'salary 200000)))

(define ceo-lens (hash-lens 'ceo))
(define salary-lens (hash-lens 'salary))

(define (compose-lens l1 l2)
  (lens
    (λ (t) ((lens-getter l2) ((lens-getter l1) t)))
    (λ (t v) ((lens-setter l1) t ((lens-setter l2) ((lens-getter l1) t) v)))))

(define ceo-salary-lens (compose-lens ceo-lens salary-lens))

(displayln (format "  Before: ~a" ((lens-getter ceo-salary-lens) company)))
(displayln (format "  After:  ~a"
                  ((lens-getter ceo-salary-lens)
                   ((lens-setter ceo-salary-lens) company 220000))))
(displayln "  Foci: 1 (exactly Alice's salary)")
(newline)

;; 0 or 1 focus: Prism
(displayln "0 OR 1 FOCUS: Add phone only if missing")
(define person-maybe-phone (hash 'name "Bob"))
(displayln (format "  Before: ~a" person-maybe-phone))
(displayln (format "  Has phone? ~a" ((prism-match? phone-prism) person-maybe-phone)))
(displayln (format "  After:  ~a"
                  ((prism-setter phone-prism) person-maybe-phone "555-5678")))
(displayln "  Foci: 0 initially, then 1 after adding")
(newline)

;; N foci: Traversal
(displayln "N FOCI: Give raises to all employees")
(define team
  (list
    (hash 'name "Alice" 'salary 100000)
    (hash 'name "Bob" 'salary 95000)
    (hash 'name "Charlie" 'salary 90000)))

(define all-salaries-traversal
  (traversal
    (λ (team) (map (λ (emp) (hash-ref emp 'salary)) team))
    (λ (team f)
      (for/list ([emp team])
        (hash-set emp 'salary (f (hash-ref emp 'salary)))))))

(displayln (format "  Salaries before: ~a" ((traversal-get-all all-salaries-traversal) team)))
(define team-after ((traversal-set-all all-salaries-traversal) team (λ (s) (* s 1.1)))
(displayln (format "  Salaries after:  ~a" ((traversal-get-all all-salaries-traversal) team-after)))
(displayln (format "  Foci: ~a (all 3 salaries)" (length team)))
(newline)

;; ============================================================================
;; THE POWER OF DIFFERENT FOCUS COUNTS
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║            WHY DIFFERENT FOCUS COUNTS MATTER                 ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "EXACTLY 1 (Lens):")
(displayln "  ✓ Always succeeds")
(displayln "  ✓ Type-safe (no Maybe/Option needed)")
(displayln "  ✓ Simple to reason about")
(displayln "  Example: Accessing a required struct field")
(newline)

(displayln "0 OR 1 (Prism):")
(displayln "  ✓ Handles optional data gracefully")
(displayln "  ✓ Type-safe failure")
(displayln "  ✓ Good for sum types/variants")
(displayln "  Example: Accessing optional phone number")
(newline)

(displayln "0 TO N (Traversal):")
(displayln "  ✓ Batch operations")
(displayln "  ✓ Filter-map patterns")
(displayln "  ✓ Update multiple locations at once")
(displayln "  Example: Updating all matching records")
(newline)

(displayln "The number of foci determines:")
(displayln "  • Whether the optic can fail")
(displayln "  • What type the view operation returns")
(displayln "  • How powerful the abstraction is")
(displayln "  • What guarantees you get")
