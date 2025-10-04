#lang racket

;; Ocular Patdown: Traversal Examples
;; Based on official documentation

(require ocular-patdown)

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         OCULAR PATDOWN: TRAVERSAL EXAMPLES                   ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

;; ============================================================================
;; BASIC TRAVERSAL OPERATIONS
;; ============================================================================

(displayln "═══ BASIC TRAVERSAL OPERATIONS ═══")
(newline)

(define lst '(1 2 3 4 5))

(displayln "List: '(1 2 3 4 5)")
(displayln "")

(displayln "traversal-map (transform all foci):")
(displayln (format "  (traversal-map list-traversal lst (* 2)) => ~a"
                  (traversal-map list-traversal lst (λ (x) (* x 2)))))
(displayln (format "  (traversal-map list-traversal lst add1) => ~a"
                  (traversal-map list-traversal lst add1)))
(newline)

(displayln "traversal->list (extract all foci):")
(displayln (format "  (traversal->list list-traversal lst) => ~a"
                  (traversal->list list-traversal lst)))
(newline)

(displayln "traversal-foldl (fold over foci):")
(displayln (format "  (traversal-foldl list-traversal lst 0 +) => ~a"
                  (traversal-foldl list-traversal lst 0 +)))
(displayln (format "  (traversal-foldl list-traversal lst 1 *) => ~a"
                  (traversal-foldl list-traversal lst 1 *)))
(newline)

;; ============================================================================
;; BUILT-IN TRAVERSALS
;; ============================================================================

(displayln "═══ BUILT-IN TRAVERSALS ═══")
(newline)

(displayln "list-traversal:")
(define nums '(10 20 30))
(displayln (format "  ~a => ~a"
                  nums
                  (traversal-map list-traversal nums (λ (x) (+ x 5)))))
(newline)

(displayln "vector-traversal:")
(define vec #(1 2 3 4))
(displayln (format "  ~a => ~a"
                  vec
                  (traversal-map vector-traversal vec (λ (x) (* x 10)))))
(newline)

;; ============================================================================
;; TRAVERSAL COMPOSITION
;; ============================================================================

(displayln "═══ TRAVERSAL COMPOSITION ═══")
(newline)

(define nested-list '((1 2) (3 4) (5 6)))

(displayln "Nested list: '((1 2) (3 4) (5 6))")
(displayln "")

;; Compose traversals to access all inner elements
(define all-elements-traversal
  (traversal-compose list-traversal list-traversal))

(displayln "Composed traversal (outer list -> inner lists -> elements):")
(displayln (format "  (traversal->list all-elements-traversal nested-list) => ~a"
                  (traversal->list all-elements-traversal nested-list)))
(displayln (format "  (traversal-map all-elements-traversal nested-list (* 10)) => ~a"
                  (traversal-map all-elements-traversal nested-list
                                (λ (x) (* x 10)))))
(newline)

;; ============================================================================
;; LENS + TRAVERSAL COMPOSITION
;; ============================================================================

(displayln "═══ LENS + TRAVERSAL COMPOSITION ═══")
(newline)

(struct person (name scores) #:transparent)

(define students
  (list (person "Alice" '(85 90 92))
        (person "Bob" '(78 88 95))
        (person "Charlie" '(90 85 88))))

(displayln "Students with scores:")
(for ([s students])
  (displayln (format "  ~a" s)))
(newline)

;; Lens to access scores field
(define scores-lens (struct-lens person scores))

;; Compose: list of students -> scores -> each score
(define all-scores-traversal
  (traversal-compose
    (traversal-compose list-traversal scores-lens)
    list-traversal))

(displayln "All scores (via composed traversal):")
(displayln (format "  ~a" (traversal->list all-scores-traversal students)))
(newline)

(displayln "Add 5 bonus points to all scores:")
(define updated-students
  (traversal-map all-scores-traversal students (λ (s) (+ s 5))))
(for ([s updated-students])
  (displayln (format "  ~a" s)))
(newline)

;; ============================================================================
;; FILTERING WITH TRAVERSALS
;; ============================================================================

(displayln "═══ FILTERING (Conceptual) ═══")
(newline)

(displayln "Note: Filtered traversals focus only on matching elements")
(displayln "")

(define scores '(55 85 65 90 70 45 95))
(displayln (format "Scores: ~a" scores))
(newline)

;; Map over all, but only some values "matter"
(displayln "Boost failing scores (<70) by 10:")
(define boosted
  (traversal-map list-traversal scores
                (λ (s) (if (< s 70) (+ s 10) s))))
(displayln (format "  Result: ~a" boosted))
(newline)

;; ============================================================================
;; PRACTICAL EXAMPLES
;; ============================================================================

(displayln "═══ PRACTICAL EXAMPLES ═══")
(newline)

(displayln "Example 1: Normalize all scores to percentage")
(define raw-scores '(43 47 50 48 45))
(define max-score 50)
(displayln (format "  Raw scores (out of ~a): ~a" max-score raw-scores))
(define percentages
  (traversal-map list-traversal raw-scores
                (λ (s) (* (/ s max-score) 100))))
(displayln (format "  Percentages: ~a" percentages))
(newline)

(displayln "Example 2: Matrix operations")
(define matrix '((1 2 3) (4 5 6) (7 8 9)))
(displayln (format "  Matrix: ~a" matrix))

(define doubled-matrix
  (traversal-map
    (traversal-compose list-traversal list-traversal)
    matrix
    (λ (x) (* x 2))))
(displayln (format "  Doubled: ~a" doubled-matrix))
(newline)

(displayln "Example 3: Sum all nested values")
(define sum-all
  (traversal-foldl
    (traversal-compose list-traversal list-traversal)
    matrix
    0
    +))
(displayln (format "  Sum of all elements: ~a" sum-all))
(newline)

;; ============================================================================
;; TRAVERSALS vs LENSES
;; ============================================================================

(displayln "═══ TRAVERSALS vs LENSES ═══")
(newline)

(displayln "KEY DIFFERENCES:")
(displayln "")
(displayln "LENS:")
(displayln "  • Exactly 1 focus")
(displayln "  • lens-get returns single value")
(displayln "  • Always succeeds")
(newline)

(displayln "TRAVERSAL:")
(displayln "  • 0 to N foci")
(displayln "  • traversal->list returns list of values")
(displayln "  • May have zero elements")
(newline)

(displayln "RELATIONSHIP:")
(displayln "  • Every lens IS a traversal (with 1 focus)")
(displayln "  • Can compose lenses and traversals")
(displayln "  • Result: lens ∘ traversal = traversal")
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "TRAVERSAL OPERATIONS:")
(displayln "  • traversal-map: transform all foci")
(displayln "  • traversal->list: extract all foci")
(displayln "  • traversal-foldl: fold over foci")
(displayln "  • traversal-compose: combine traversals")
(newline)

(displayln "BUILT-IN TRAVERSALS:")
(displayln "  • list-traversal: all list elements")
(displayln "  • vector-traversal: all vector elements")
(displayln "  • Additional traversals for trees, etc.")
(newline)

(displayln "KEY FEATURES:")
(displayln "  • Multiple foci (0 to N)")
(displayln "  • Composable with lenses")
(displayln "  • Map/fold operations")
(displayln "  • Immutable updates")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 047: Ocular Patdown Traversals")
(displayln "═══════════════════════════════════════════════════════════════")
