#lang racket

;; Lens as a simple struct with getter and setter
;; Demonstrating first-class lens values

(provide (struct-out lens)
         make-lens
         view
         set
         over
         compose-lens)

;; ============================================================================
;; Lens Definition: Just a struct!
;; ============================================================================

(struct lens (getter setter) #:transparent)

;; Alternative constructor name for clarity
(define make-lens lens)

;; ============================================================================
;; Core Operations
;; ============================================================================

(define (view l target)
  "Extract focus from target using lens getter."
  ((lens-getter l) target))

(define (set l target new-value)
  "Update target with new focus value using lens setter."
  ((lens-setter l) target new-value))

(define (over l target fn)
  "Modify focus in target by applying function."
  (set l target (fn (view l target))))

;; ============================================================================
;; Lens Composition
;; ============================================================================

(define (compose-lens outer inner)
  "Compose two lenses: outer then inner.

   TARGET --[outer]--> intermediate --[inner]--> FOCUS"
  (lens
    ; Composed getter
    (λ (target)
      (define intermediate ((lens-getter outer) target))
      ((lens-getter inner) intermediate))
    ; Composed setter
    (λ (target new-focus)
      (define intermediate ((lens-getter outer) target))
      (define new-intermediate ((lens-setter inner) intermediate new-focus))
      ((lens-setter outer) target new-intermediate))))

;; ============================================================================
;; Example: Hash Lenses
;; ============================================================================

(define (hash-lens key)
  "Create a lens for a hash field."
  (lens
    ; getter: extract field
    (λ (h) (hash-ref h key))
    ; setter: update field immutably
    (λ (h v) (hash-set h key v))))

;; ============================================================================
;; Example: Struct Lenses
;; ============================================================================

(struct posn (x y) #:transparent)

(define posn-x-lens
  (lens
    ; getter: extract x
    posn-x
    ; setter: update x immutably
    (λ (p new-x) (struct-copy posn p [x new-x]))))

(define posn-y-lens
  (lens
    ; getter: extract y
    posn-y
    ; setter: update y immutably
    (λ (p new-y) (struct-copy posn p [y new-y]))))

;; ============================================================================
;; Example: Rectangle with nested posn
;; ============================================================================

(struct rect (top-left width height) #:transparent)

(define rect-top-left-lens
  (lens
    rect-top-left
    (λ (r new-tl) (struct-copy rect r [top-left new-tl]))))

;; Compose to focus on x coordinate of rectangle's top-left
(define rect-x-lens
  (compose-lens rect-top-left-lens posn-x-lens))

(define rect-y-lens
  (compose-lens rect-top-left-lens posn-y-lens))

;; ============================================================================
;; Rectangle Movement Functions
;; ============================================================================

(define (move-rect-right r dx)
  "Move rectangle right by dx pixels."
  (over rect-x-lens r (λ (x) (+ x dx))))

(define (move-rect-down r dy)
  "Move rectangle down by dy pixels."
  (over rect-y-lens r (λ (y) (+ y dy))))

(define (move-rect r dx dy)
  "Move rectangle by (dx, dy)."
  (move-rect-down (move-rect-right r dx) dy))

;; Alternative: update top-left directly
(define (move-rect-direct r dx dy)
  "Move rectangle by updating top-left posn directly."
  (over rect-top-left-lens r
    (λ (p)
      (posn (+ (posn-x p) dx)
            (+ (posn-y p) dy)))))

;; ============================================================================
;; Demonstration
;; ============================================================================

(module+ main
  (displayln "=== LENS AS A SIMPLE STRUCT ===\n")

  (displayln "Definition:")
  (displayln "(struct lens (getter setter) #:transparent)")
  (newline)

  ;; Show lens structure
  (define name-lens (hash-lens 'name))
  (displayln "Example lens:")
  (displayln name-lens)
  (displayln (format "  getter: ~a" (lens-getter name-lens)))
  (displayln (format "  setter: ~a" (lens-setter name-lens)))
  (newline)

  ;; Lenses are first-class values
  (displayln "=== LENSES ARE FIRST-CLASS VALUES ===\n")

  (define my-lenses
    (list (hash-lens 'name)
          (hash-lens 'age)
          (hash-lens 'city)))

  (displayln "List of lenses:")
  (displayln my-lenses)
  (newline)

  ;; Pass lenses as arguments
  (define (bulk-update data lenses values)
    (for/fold ([result data])
              ([l lenses]
               [v values])
      (set l result v)))

  (define person (hash 'name "Alice" 'age 30 'city "Seattle"))
  (define updated
    (bulk-update person
                 (list (hash-lens 'name) (hash-lens 'city))
                 '("Bob" "Portland")))

  (displayln "Bulk update using list of lenses:")
  (displayln (format "Before: ~a" person))
  (displayln (format "After:  ~a" updated))
  (newline)

  ;; Struct example
  (displayln "=== STRUCT EXAMPLE ===\n")

  (define p (posn 10 20))
  (displayln (format "Original posn: ~a" p))

  (displayln "\nUsing posn-x-lens:")
  (displayln (format "View x: ~a" (view posn-x-lens p)))
  (displayln (format "Set x to 15: ~a" (set posn-x-lens p 15)))
  (displayln (format "Increment x: ~a" (over posn-x-lens p add1)))
  (newline)

  ;; Rectangle example
  (displayln "=== RECTANGLE MOVEMENT ===\n")

  (define r (rect (posn 10 20) 100 50))
  (displayln (format "Original rect: ~a" r))

  (displayln "\nMove right 5:")
  (displayln (move-rect-right r 5))

  (displayln "\nMove down 10:")
  (displayln (move-rect-down r 10))

  (displayln "\nMove (5, 10) using composition:")
  (displayln (move-rect r 5 10))

  (displayln "\nMove (5, 10) updating top-left directly:")
  (displayln (move-rect-direct r 5 10))
  (newline)

  ;; Show lens composition structure
  (displayln "=== LENS COMPOSITION ===\n")

  (displayln "rect-x-lens is composed of:")
  (displayln "  rect-top-left-lens (outer)")
  (displayln "  posn-x-lens (inner)")
  (newline)

  (displayln "The composed lens:")
  (displayln rect-x-lens)
  (newline)

  (displayln "Visualization:")
  (displayln "  rect")
  (displayln "    ├─ top-left: posn")
  (displayln "    │    ├─ x: 10  ← FOCUS of rect-x-lens")
  (displayln "    │    └─ y: 20")
  (displayln "    ├─ width: 100")
  (displayln "    └─ height: 50")
  (newline)

  ;; Multiple composition
  (displayln "=== DEEP COMPOSITION ===\n")

  (struct company (name departments) #:transparent)
  (struct department (name manager) #:transparent)
  (struct employee (name salary) #:transparent)

  (define company-depts-lens
    (lens company-departments
          (λ (c v) (struct-copy company c [departments v]))))

  (define first-dept-lens
    (lens (λ (depts) (first depts))
          (λ (depts v) (cons v (rest depts)))))

  (define dept-manager-lens
    (lens department-manager
          (λ (d v) (struct-copy department d [manager v]))))

  (define emp-salary-lens
    (lens employee-salary
          (λ (e v) (struct-copy employee e [salary v]))))

  ;; Compose all four lenses
  (define first-manager-salary-lens
    (compose-lens
      (compose-lens
        (compose-lens company-depts-lens first-dept-lens)
        dept-manager-lens)
      emp-salary-lens))

  (define tech-corp
    (company "Tech Corp"
             (list
               (department "Engineering"
                          (employee "Alice" 100000)))))

  (displayln "Company structure:")
  (displayln tech-corp)
  (newline)

  (displayln "Focus on first manager's salary:")
  (displayln (format "Current: ~a" (view first-manager-salary-lens tech-corp)))

  (displayln "\nGive 10% raise:")
  (displayln (over first-manager-salary-lens tech-corp (λ (s) (* s 1.1))))
  (newline)

  ;; Lens as struct benefits
  (displayln "=== WHY LENS AS A SIMPLE STRUCT? ===\n")
  (displayln "1. TRANSPARENT: Easy to inspect and debug")
  (displayln "2. FIRST-CLASS: Can store in lists, pass to functions")
  (displayln "3. COMPOSABLE: Just two functions that work together")
  (displayln "4. IMMUTABLE: Lenses themselves are immutable data")
  (displayln "5. MINIMAL: No magic, just getter + setter")
  (displayln "6. FLEXIBLE: Works with any data structure")
  (newline))

;; ============================================================================
;; Tests
;; ============================================================================

(module+ test
  (require rackunit)

  ;; Test basic lens operations
  (define p (posn 10 20))

  (check-equal? (view posn-x-lens p) 10)
  (check-equal? (set posn-x-lens p 15) (posn 15 20))
  (check-equal? (over posn-x-lens p (λ (x) (* x 2))) (posn 20 20))

  ;; Test rectangle movement
  (define r (rect (posn 10 20) 100 50))

  (check-equal? (view rect-x-lens r) 10)
  (check-equal? (view rect-y-lens r) 20)

  (check-equal? (move-rect-right r 5)
                (rect (posn 15 20) 100 50))

  (check-equal? (move-rect-down r 10)
                (rect (posn 10 30) 100 50))

  (check-equal? (move-rect r 5 10)
                (rect (posn 15 30) 100 50))

  ;; Test that both movement methods are equivalent
  (check-equal? (move-rect r 5 10)
                (move-rect-direct r 5 10))

  (displayln "All tests passed!"))
