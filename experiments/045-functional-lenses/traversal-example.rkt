#lang racket

;; Traversal Example: Multiple Foci
;; Demonstrates how optics can focus on 0, 1, or many values

(require "lens-laws.rkt")

;; ============================================================================
;; OPTICS BY NUMBER OF FOCI
;; ============================================================================

(displayln "╔══════════════════════════════════════════════════════════╗")
(displayln "║        OPTICS CLASSIFIED BY NUMBER OF FOCI              ║")
(displayln "╚══════════════════════════════════════════════════════════╝")
(newline)

(displayln "LENS:       Exactly 1 focus")
(displayln "            Always succeeds, focuses on one part")
(displayln "            Example: car-lens on '(1 2 3) → 1")
(newline)

(displayln "PRISM:      0 or 1 focus")
(displayln "            May fail, focuses on optional part")
(displayln "            Example: maybe-phone-lens → phone or nothing")
(newline)

(displayln "TRAVERSAL:  0 to N foci")
(displayln "            Focuses on multiple parts")
(displayln "            Example: all-elements-lens → entire list")
(newline)

;; ============================================================================
;; Example 1: LENS (Exactly 1 Focus)
;; ============================================================================

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  Example 1: LENS - Exactly 1 Focus")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

(define lst '(10 20 30))
(displayln (format "List: ~a" lst))
(newline)

(displayln "car-lens focuses on exactly 1 element (the first):")
(displayln (format "  view car-lens: ~a" (view car-lens lst)))
(displayln (format "  set car-lens to 99: ~a" (set car-lens lst 99)))
(displayln (format "  over car-lens (* 2): ~a" (over car-lens lst (λ (x) (* x 2)))))
(newline)

(displayln "Number of foci: 1 ✓")
(newline)

;; ============================================================================
;; Example 2: PRISM (0 or 1 Focus)
;; ============================================================================

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  Example 2: PRISM - 0 or 1 Focus")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

(struct prism (match? getter setter) #:transparent)

(define (prism-view p target [default #f])
  "View through prism, returning default if no focus exists."
  (if ((prism-match? p) target)
      ((prism-getter p) target)
      default))

(define (prism-set p target value)
  "Set through prism, only if focus exists."
  (if ((prism-match? p) target)
      ((prism-setter p) target value)
      target))

;; Prism for non-empty lists
(define head-prism
  (prism
    ; match?: has focus if list is non-empty
    pair?
    ; getter: extract first
    car
    ; setter: replace first
    (λ (lst v) (cons v (cdr lst)))))

(displayln "head-prism focuses on first element if list is non-empty:")
(newline)

(define non-empty '(1 2 3))
(displayln (format "Non-empty list: ~a" non-empty))
(displayln (format "  Has focus? ~a" ((prism-match? head-prism) non-empty)))
(displayln (format "  view: ~a" (prism-view head-prism non-empty)))
(displayln (format "  Number of foci: 1"))
(newline)

(define empty '())
(displayln (format "Empty list: ~a" empty))
(displayln (format "  Has focus? ~a" ((prism-match? head-prism) empty)))
(displayln (format "  view: ~a" (prism-view head-prism empty)))
(displayln (format "  Number of foci: 0"))
(newline)

;; ============================================================================
;; Example 3: TRAVERSAL (0 to N Foci)
;; ============================================================================

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  Example 3: TRAVERSAL - 0 to N Foci")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

(define (list-traversal)
  "Traversal over all elements of a list.
   Each element is a separate focus."
  (λ (fn)
    (λ (lst)
      (map fn lst))))

(define (view-all traversal target)
  "View all foci through traversal."
  ((traversal identity) target))

(define (modify-all traversal target fn)
  "Modify all foci through traversal."
  ((traversal fn) target))

(displayln "list-traversal focuses on ALL elements:")
(newline)

(define numbers '(1 2 3 4 5))
(displayln (format "List: ~a" numbers))
(displayln (format "  Number of foci: ~a" (length numbers)))
(displayln (format "  view-all: ~a" (view-all (list-traversal) numbers)))
(displayln (format "  modify-all (* 2): ~a" (modify-all (list-traversal) numbers (λ (x) (* x 2)))))
(displayln (format "  modify-all (+ 10): ~a" (modify-all (list-traversal) numbers (λ (x) (+ x 10)))))
(newline)

(define empty-list '())
(displayln (format "Empty list: ~a" empty-list))
(displayln (format "  Number of foci: ~a" (length empty-list)))
(displayln (format "  view-all: ~a" (view-all (list-traversal) empty-list)))
(newline)

;; ============================================================================
;; Example 4: FILTERED TRAVERSAL (Variable Number of Foci)
;; ============================================================================

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  Example 4: FILTERED TRAVERSAL - Variable Foci")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

(define (filtered-traversal pred?)
  "Traversal over elements matching predicate.
   Number of foci depends on how many match."
  (λ (fn)
    (λ (lst)
      (for/list ([item lst])
        (if (pred? item)
            (fn item)
            item)))))

(define scores '(55 85 65 90 70 45 95))
(displayln (format "Scores: ~a" scores))
(newline)

(define failing-traversal (filtered-traversal (λ (x) (< x 70))))
(displayln "failing-traversal (scores < 70):")
(define failing-scores (filter (λ (x) (< x 70)) scores))
(displayln (format "  Matching elements: ~a" failing-scores))
(displayln (format "  Number of foci: ~a" (length failing-scores)))
(displayln (format "  Boost by 10: ~a"
                  (modify-all failing-traversal scores (λ (x) (+ x 10)))))
(newline)

(define passing-traversal (filtered-traversal (λ (x) (>= x 90))))
(displayln "passing-traversal (scores >= 90):")
(define passing-scores (filter (λ (x) (>= x 90)) scores))
(displayln (format "  Matching elements: ~a" passing-scores))
(displayln (format "  Number of foci: ~a" (length passing-scores)))
(displayln (format "  Cap at 100: ~a"
                  (modify-all passing-traversal scores (λ (x) (min x 100)))))
(newline)

;; ============================================================================
;; Example 5: Nested Traversal (Product of Foci)
;; ============================================================================

(displayln "═══════════════════════════════════════════════════════════")
(displayln "  Example 5: NESTED TRAVERSAL - Product of Foci")
(displayln "═══════════════════════════════════════════════════════════")
(newline)

(struct company (name departments) #:transparent)
(struct department (name employees) #:transparent)
(struct employee (name salary) #:transparent)

(define tech-corp
  (company "Tech Corp"
           (list
             (department "Engineering"
                        (list (employee "Alice" 100000)
                              (employee "Bob" 95000)))
             (department "Sales"
                        (list (employee "Charlie" 80000)
                              (employee "Diana" 85000)))
             (department "Marketing"
                        (list (employee "Eve" 75000))))))

(displayln (format "Company: ~a" (company-name tech-corp)))
(displayln (format "  Departments: ~a" (length (company-departments tech-corp))))

(define total-employees
  (apply + (map (λ (d) (length (department-employees d)))
                (company-departments tech-corp))))

(displayln (format "  Total employees: ~a" total-employees))
(displayln (format "  Number of foci (all employees): ~a" total-employees))
(newline)

;; Traversal over all employees in all departments
(define (all-employees-traversal)
  (λ (fn)
    (λ (comp)
      (company
        (company-name comp)
        (for/list ([dept (company-departments comp)])
          (department
            (department-name dept)
            (for/list ([emp (department-employees dept)])
              (fn emp))))))))

(displayln "Give everyone a 10% raise:")
(define raised-company
  ((all-employees-traversal
     (λ (emp)
       (employee (employee-name emp)
                (* (employee-salary emp) 1.1))))
   tech-corp))

(for ([dept (company-departments raised-company)])
  (displayln (format "  ~a:" (department-name dept)))
  (for ([emp (department-employees dept)])
    (displayln (format "    ~a: $~a"
                      (employee-name emp)
                      (employee-salary emp)))))
(newline)

;; ============================================================================
;; SUMMARY TABLE
;; ============================================================================

(displayln "╔══════════════════════════════════════════════════════════╗")
(displayln "║                    SUMMARY TABLE                         ║")
(displayln "╚══════════════════════════════════════════════════════════╝")
(newline)

(displayln "┌────────────┬─────────────┬──────────────────────────────┐")
(displayln "│ Optic Type │ # of Foci   │ Example                      │")
(displayln "├────────────┼─────────────┼──────────────────────────────┤")
(displayln "│ Lens       │ Exactly 1   │ car-lens                     │")
(displayln "│ Prism      │ 0 or 1      │ head-prism (maybe empty)     │")
(displayln "│ Traversal  │ 0 to N      │ list-traversal (all elems)   │")
(displayln "│ Filtered   │ 0 to N      │ filtered-traversal (matches) │")
(displayln "│ Nested     │ Product     │ all-employees (M × N)        │")
(displayln "└────────────┴─────────────┴──────────────────────────────┘")
(newline)

(displayln "KEY INSIGHT:")
(displayln "  - Lens:       1 focus    (always succeeds)")
(displayln "  - Prism:      0-1 foci   (may fail)")
(displayln "  - Traversal:  0-N foci   (varies)")
(displayln "")
(displayln "All are 'optics' - composable getter/setter abstractions!")
(newline)

;; ============================================================================
;; Version Info
;; ============================================================================

(displayln "═══════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 045")
(displayln "═══════════════════════════════════════════════════════════")
