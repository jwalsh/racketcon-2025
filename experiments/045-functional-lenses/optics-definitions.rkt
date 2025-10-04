#lang racket

;; Precise Definitions of Optics
;; Understanding Fold, Getter, Traversal, and Composition

(provide (all-defined-out))

;; ============================================================================
;; WHAT ARE OPTICS? - Precise Definitions
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                  OPTICS: PRECISE DEFINITIONS                  ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

;; ============================================================================
;; 1. FOLD - A Getter for Multiple Values
;; ============================================================================

(displayln "═══ FOLD: A Getter for Multiple Values ═══\n")

(displayln "DEFINITION:")
(displayln "  Fold s a = s -> [a]")
(displayln "")
(displayln "A Fold extracts ZERO OR MORE values from a structure.")
(displayln "- It's READ-ONLY (no setter)")
(displayln "- Returns a list of foci")
(displayln "- Think: \"collect all matching values\"")
(newline)

(struct fold (view-all) #:transparent)

(define (make-fold getter)
  "Create a fold from a function: s -> [a]"
  (fold getter))

;; Example: All values in a hash
(define hash-values-fold
  (make-fold hash-values))

(define person (hash 'name "Alice" 'age 30 'city "Seattle"))

(displayln "Example: hash-values-fold")
(displayln (format "  Type: Hash -> [Value]"))
(displayln (format "  Input:  ~a" person))
(displayln (format "  Output: ~a" ((fold-view-all hash-values-fold) person)))
(displayln (format "  Count:  ~a foci" (length ((fold-view-all hash-values-fold) person))))
(newline)

;; Example: All even numbers in a list
(define evens-fold
  (make-fold (λ (lst) (filter even? lst))))

(define numbers '(1 2 3 4 5 6 7 8 9 10))

(displayln "Example: evens-fold")
(displayln (format "  Type: [Number] -> [Number]"))
(displayln (format "  Input:  ~a" numbers))
(displayln (format "  Output: ~a" ((fold-view-all evens-fold) numbers)))
(displayln (format "  Count:  ~a foci" (length ((fold-view-all evens-fold) numbers))))
(newline)

(displayln "KEY POINT: Fold is a GETTING operation only")
(displayln "  ✓ Can extract/view multiple values")
(displayln "  ✗ Cannot set or update")
(newline)

;; ============================================================================
;; 2. TRAVERSAL - Fold + Setter
;; ============================================================================

(displayln "═══ TRAVERSAL: Fold + Setter ═══\n")

(displayln "DEFINITION:")
(displayln "  Traversal s t a b = {")
(displayln "    view:  s -> [a]         -- Get all foci")
(displayln "    over:  s -> (a -> b) -> t  -- Update all foci")
(displayln "  }")
(displayln "")
(displayln "A Traversal is a Fold that can also UPDATE.")
(displayln "- Can get multiple values (like Fold)")
(displayln "- Can set/modify multiple values")
(displayln "- Think: \"map over all matching locations\"")
(newline)

(struct traversal (view-all over) #:transparent)

(define (make-traversal getter mapper)
  "Create traversal from getter and mapper functions"
  (traversal getter mapper))

;; Example: Traversal over list
(define list-traversal
  (make-traversal
    ; view-all: get all elements
    (λ (lst) lst)
    ; over: map function over all elements
    (λ (lst f) (map f lst))))

(define scores '(85 90 92))

(displayln "Example: list-traversal")
(displayln (format "  Type: [a] -> [a]"))
(displayln (format "  View:  ~a" ((traversal-view-all list-traversal) scores)))
(displayln (format "  Over:  ~a"
                  ((traversal-over list-traversal) scores (λ (s) (+ s 5)))))
(newline)

;; Example: Filtered traversal
(define passing-traversal
  (make-traversal
    ; view-all: only scores >= 90
    (λ (lst) (filter (λ (s) (>= s 90)) lst))
    ; over: update only scores >= 90
    (λ (lst f)
      (for/list ([s lst])
        (if (>= s 90) (f s) s)))))

(displayln "Example: passing-traversal (>= 90)")
(displayln (format "  View:  ~a" ((traversal-view-all passing-traversal) scores)))
(displayln (format "  Over:  ~a"
                  ((traversal-over passing-traversal) scores (λ (s) (+ s 5)))))
(displayln "  (Only passing scores get +5 bonus)")
(newline)

;; ============================================================================
;; 3. TRAVERSAL TYPE SIGNATURE
;; ============================================================================

(displayln "═══ TRAVERSAL: Type Signature ═══\n")

(displayln "In Haskell notation:")
(displayln "  Traversal s t a b")
(displayln "  where:")
(displayln "    s = source structure type")
(displayln "    t = target structure type (after update)")
(displayln "    a = old focus type")
(displayln "    b = new focus type")
(newline)

(displayln "Simple case (Traversal' s a):")
(displayln "  s = t  (structure type doesn't change)")
(displayln "  a = b  (focus type doesn't change)")
(newline)

(displayln "Example: Traversal (List X) (List X) X X")
(displayln "  - Structure: List X")
(displayln "  - Focus: X (each element)")
(displayln "  - Updates elements but list stays List X")
(newline)

;; ============================================================================
;; 4. COMPOSING TRAVERSALS
;; ============================================================================

(displayln "═══ COMPOSING TRAVERSALS ═══\n")

(displayln "YES! Traversals compose!")
(displayln "")
(displayln "If you have:")
(displayln "  t1: Traversal A B")
(displayln "  t2: Traversal B C")
(displayln "Then:")
(displayln "  (compose t1 t2): Traversal A C")
(newline)

(define (compose-traversal outer inner)
  "Compose two traversals"
  (traversal
    ; view-all: get from outer, then get from each inner
    (λ (target)
      (define outer-foci ((traversal-view-all outer) target))
      (append* (map (traversal-view-all inner) outer-foci)))
    ; over: update outer, then update inner in each
    (λ (target f)
      ((traversal-over outer) target
        (λ (outer-focus)
          ((traversal-over inner) outer-focus f))))))

;; Example: Nested lists
(struct department (name employees) #:transparent)
(struct employee (name salary) #:transparent)

(define company
  (list
    (department "Engineering"
                (list (employee "Alice" 100000)
                      (employee "Bob" 95000)))
    (department "Sales"
                (list (employee "Charlie" 80000)
                      (employee "Diana" 85000)))))

(displayln "Example: Compose department traversal + employee traversal")
(newline)

;; Traversal over all departments
(define dept-traversal list-traversal)

;; Traversal over employees in a department
(define employees-traversal
  (make-traversal
    (λ (dept) (department-employees dept))
    (λ (dept f)
      (struct-copy department dept
                   [employees (map f (department-employees dept))]))))

;; Composed: all employees in all departments
(define all-employees-traversal
  (compose-traversal dept-traversal employees-traversal))

(displayln "Viewing all employees:")
(define all-emps ((traversal-view-all all-employees-traversal) company))
(for ([emp all-emps])
  (displayln (format "  - ~a: $~a" (employee-name emp) (employee-salary emp))))
(newline)

(displayln "Giving everyone a 10% raise:")
(define raised-company
  ((traversal-over all-employees-traversal) company
    (λ (emp) (struct-copy employee emp
                         [salary (* (employee-salary emp) 1.1)]))))

(define raised-emps ((traversal-view-all all-employees-traversal) raised-company))
(for ([emp raised-emps])
  (displayln (format "  - ~a: $~a" (employee-name emp) (employee-salary emp))))
(newline)

;; ============================================================================
;; 5. TRAVERSAL COMPOSITION: MORE EXAMPLES
;; ============================================================================

(displayln "═══ MORE TRAVERSAL COMPOSITION EXAMPLES ═══\n")

;; Example 1: List of lists
(define matrix '((1 2 3) (4 5 6) (7 8 9)))

(define rows-traversal list-traversal)
(define cols-traversal list-traversal)
(define all-elements-traversal
  (compose-traversal rows-traversal cols-traversal))

(displayln "Example 1: Matrix (list of lists)")
(displayln (format "  Matrix: ~a" matrix))
(displayln (format "  All elements: ~a"
                  ((traversal-view-all all-elements-traversal) matrix)))
(displayln (format "  Double all: ~a"
                  ((traversal-over all-elements-traversal) matrix (λ (x) (* x 2)))))
(newline)

;; Example 2: Hash of lists
(define teams
  (hash 'red '(100 90 85)
        'blue '(95 92 88)
        'green '(87 84 82)))

(define teams-traversal
  (make-traversal
    (λ (h) (hash-values h))
    (λ (h f)
      (for/hash ([(k v) (in-hash h)])
        (values k (f v))))))

(define team-scores-traversal
  (compose-traversal teams-traversal list-traversal))

(displayln "Example 2: Hash of lists (teams -> scores)")
(displayln (format "  Teams: ~a" teams))
(displayln (format "  All scores: ~a"
                  ((traversal-view-all team-scores-traversal) teams)))
(displayln (format "  +5 bonus: ~a"
                  ((traversal-over team-scores-traversal) teams (λ (s) (+ s 5)))))
(newline)

;; ============================================================================
;; 6. THE OPTICS HIERARCHY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                  THE OPTICS HIERARCHY                         ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "From most general to most specific:")
(newline)

(displayln "  Fold        (0..N foci, read-only)")
(displayln "    ↓")
(displayln "  Traversal   (0..N foci, read-write)")
(displayln "    ↓")
(displayln "  Prism       (0..1 foci, read-write, may fail)")
(displayln "    ↓")
(displayln "  Lens        (exactly 1 focus, always succeeds)")
(displayln "    ↓")
(displayln "  Iso         (bijection, can go both ways)")
(newline)

(displayln "Composition rules:")
(displayln "  - Lens ∘ Lens = Lens")
(displayln "  - Lens ∘ Prism = Prism")
(displayln "  - Prism ∘ Prism = Prism")
(displayln "  - Traversal ∘ Traversal = Traversal")
(displayln "  - Lens ∘ Traversal = Traversal")
(displayln "  - Any ∘ Fold = Fold")
(newline)

;; ============================================================================
;; 7. PRACTICAL TRAVERSAL PATTERNS
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║              PRACTICAL TRAVERSAL PATTERNS                     ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "Pattern 1: Filter-Map")
(displayln "  Traversal that focuses on matching elements")
(displayln "  Example: All even numbers in a list")
(newline)

(displayln "Pattern 2: Nested Structures")
(displayln "  Compose traversals to reach deep elements")
(displayln "  Example: All salaries in all departments")
(newline)

(displayln "Pattern 3: Multiple Fields")
(displayln "  Traverse multiple fields of a struct")
(displayln "  Example: Both x and y coordinates")
(newline)

(displayln "Pattern 4: Recursive Structures")
(displayln "  Traverse trees, graphs, etc.")
(displayln "  Example: All nodes in a tree")
(newline)

;; ============================================================================
;; 8. SUMMARY: FOLD vs TRAVERSAL
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                FOLD vs TRAVERSAL SUMMARY                      ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "┌──────────────┬────────────────────┬─────────────────────────┐")
(displayln "│ Feature      │ Fold               │ Traversal               │")
(displayln "├──────────────┼────────────────────┼─────────────────────────┤")
(displayln "│ Purpose      │ Extract values     │ Extract AND update      │")
(displayln "│ Operations   │ view-all           │ view-all, over, set     │")
(displayln "│ Read/Write   │ Read-only          │ Read-write              │")
(displayln "│ # of foci    │ 0..N               │ 0..N                    │")
(displayln "│ Type         │ s -> [a]           │ s -> [a]                │")
(displayln "│              │                    │ s -> (a->b) -> t        │")
(displayln "│ Composable   │ Yes                │ Yes                     │")
(displayln "│ Example      │ hash-values        │ map over list           │")
(displayln "└──────────────┴────────────────────┴─────────────────────────┘")
(newline)

(displayln "KEY INSIGHT:")
(displayln "  Fold = Getter for multiple values")
(displayln "  Traversal = Fold + Setter")
(displayln "  Both compose naturally!")
