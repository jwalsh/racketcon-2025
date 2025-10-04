#lang racket

;; Complete lens implementation with target/focus examples

(provide lens
         lens-get
         lens-set
         view
         set
         over
         compose-lens
         hash-lens
         list-lens
         vector-lens)

;; ============================================================================
;; Core Lens Structure
;; ============================================================================

(struct lens (get set) #:transparent
  #:property prop:procedure
  (λ (self fn)
    (λ (target)
      ((lens-set self) target (fn ((lens-get self) target))))))

;; ============================================================================
;; Fundamental Operations: Target → Focus
;; ============================================================================

(define (view l target)
  "Extract FOCUS from TARGET using lens."
  ((lens-get l) target))

(define (set l target new-focus)
  "Replace FOCUS in TARGET with new value."
  ((lens-set l) target new-focus))

(define (over l target fn)
  "Modify FOCUS in TARGET by applying function."
  ((lens-set l) target
    (fn ((lens-get l) target))))

;; ============================================================================
;; Lens Composition
;; ============================================================================

(define (compose-lens outer inner)
  "Compose two lenses: OUTER focuses on container, INNER on nested part.

   TARGET → (outer) → intermediate → (inner) → FOCUS"
  (lens
    ; Getter: TARGET → intermediate → FOCUS
    (λ (target)
      (define intermediate ((lens-get outer) target))
      ((lens-get inner) intermediate))
    ; Setter: TARGET + new-FOCUS → new-TARGET
    (λ (target new-focus)
      (define intermediate ((lens-get outer) target))
      (define new-intermediate ((lens-set inner) intermediate new-focus))
      ((lens-set outer) target new-intermediate))))

;; ============================================================================
;; Concrete Lens Constructors
;; ============================================================================

(define (hash-lens key)
  "Lens for hash field.
   TARGET: hash
   FOCUS: value at key"
  (lens
    (λ (h) (hash-ref h key))
    (λ (h v) (hash-set h key v))))

(define (list-lens idx)
  "Lens for list element.
   TARGET: list
   FOCUS: element at index"
  (lens
    (λ (lst) (list-ref lst idx))
    (λ (lst v) (list-set lst idx v))))

(define (vector-lens idx)
  "Lens for vector element.
   TARGET: vector
   FOCUS: element at index"
  (lens
    (λ (vec) (vector-ref vec idx))
    (λ (vec v)
      (define new-vec (vector-copy vec))
      (vector-set! new-vec idx v)
      new-vec)))

;; ============================================================================
;; Examples: Target and Focus
;; ============================================================================

(module+ main
  (displayln "=== LENS BASICS: Target and Focus ===\n")

  ;; Example 1: Simple hash
  (displayln "Example 1: Hash lens")
  (displayln "-" #:separator "")

  (define person (hash 'name "Alice" 'age 30))
  (displayln (format "TARGET: ~a" person))

  (define name-lens (hash-lens 'name))
  (displayln "LENS: name-lens (focuses on 'name field)")

  (define focus (view name-lens person))
  (displayln (format "FOCUS: ~a" focus))

  (define new-target (set name-lens person "Bob"))
  (displayln (format "NEW TARGET: ~a" new-target))
  (newline)

  ;; Example 2: Nested structure
  (displayln "Example 2: Composed lenses")
  (displayln "-" #:separator "")

  (define data
    (hash 'user (hash 'name "Alice"
                      'address (hash 'city "Seattle"))))
  (displayln (format "TARGET: ~a" data))

  (define user-lens (hash-lens 'user))
  (define address-lens (hash-lens 'address))
  (define city-lens (hash-lens 'city))

  (define user-city-lens
    (compose-lens user-lens
      (compose-lens address-lens city-lens)))

  (displayln "LENS: user → address → city (3-level composition)")

  (define city-focus (view user-city-lens data))
  (displayln (format "FOCUS: ~a" city-focus))

  (define updated-data (set user-city-lens data "Portland"))
  (displayln (format "NEW TARGET: ~a" updated-data))
  (newline)

  ;; Example 3: List lens
  (displayln "Example 3: List lens")
  (displayln "-" #:separator "")

  (define scores '(85 90 92))
  (displayln (format "TARGET: ~a" scores))

  (define second-lens (list-lens 1))
  (displayln "LENS: second element (index 1)")

  (define score-focus (view second-lens scores))
  (displayln (format "FOCUS: ~a" score-focus))

  (define boosted-scores (over second-lens scores (λ (s) (+ s 5))))
  (displayln (format "NEW TARGET (after +5): ~a" boosted-scores))
  (newline)

  ;; Example 4: Struct lens
  (struct posn (x y) #:transparent)

  (define posn-x-lens
    (lens posn-x
          (λ (p v) (struct-copy posn p [x v]))))

  (displayln "Example 4: Struct lens")
  (displayln "-" #:separator "")

  (define p (posn 10 20))
  (displayln (format "TARGET: ~a" p))

  (displayln "LENS: posn-x-lens (focuses on x coordinate)")

  (define x-focus (view posn-x-lens p))
  (displayln (format "FOCUS: ~a" x-focus))

  (define moved-p (set posn-x-lens p 15))
  (displayln (format "NEW TARGET: ~a" moved-p))
  (newline)

  ;; Example 5: Target-Focus diagram
  (displayln "=== TARGET → FOCUS Visualization ===\n")

  (define company
    (hash 'name "Tech Corp"
          'departments
            (list
              (hash 'name "Engineering"
                    'employees (list (hash 'name "Alice" 'salary 100000))))))

  (displayln "TARGET (entire company):")
  (displayln company)
  (newline)

  (define dept-lens (hash-lens 'departments))
  (define first-dept-lens (list-lens 0))
  (define employees-lens (hash-lens 'employees))
  (define first-emp-lens (list-lens 0))
  (define salary-lens (hash-lens 'salary))

  (define alice-salary-lens
    (compose-lens dept-lens
      (compose-lens first-dept-lens
        (compose-lens employees-lens
          (compose-lens first-emp-lens salary-lens)))))

  (displayln "LENS CHAIN:")
  (displayln "  company")
  (displayln "    → departments (hash)")
  (displayln "      → [0] (list index)")
  (displayln "        → employees (hash)")
  (displayln "          → [0] (list index)")
  (displayln "            → salary (hash)")
  (newline)

  (define salary-focus (view alice-salary-lens company))
  (displayln (format "FOCUS (Alice's salary): ~a" salary-focus))
  (newline)

  (define raised-company (over alice-salary-lens company (λ (s) (* s 1.1))))
  (displayln "NEW TARGET (after 10% raise):")
  (displayln raised-company)
  (newline)

  ;; Example 6: Terminology
  (displayln "=== KEY TERMINOLOGY ===\n")
  (displayln "TARGET:  The whole data structure you're operating on")
  (displayln "FOCUS:   The specific part you're zooming into")
  (displayln "LENS:    The getter/setter pair that connects them")
  (displayln "VIEW:    Extract FOCUS from TARGET")
  (displayln "SET:     Replace FOCUS in TARGET")
  (displayln "OVER:    Modify FOCUS in TARGET with a function")
  (displayln "COMPOSE: Chain lenses to navigate nested structures")
  (newline)

  (displayln "Analogy: Camera lens")
  (displayln "  TARGET = entire scene")
  (displayln "  FOCUS = what you zoom into")
  (displayln "  LENS = optical system connecting them"))

;; ============================================================================
;; Tests
;; ============================================================================

(module+ test
  (require rackunit)

  ;; Test basic operations
  (define person (hash 'name "Alice" 'age 30))
  (define name-lens (hash-lens 'name))

  (check-equal? (view name-lens person) "Alice")
  (check-equal? (view name-lens (set name-lens person "Bob")) "Bob")
  (check-equal? (view name-lens (over name-lens person string-upcase)) "ALICE")

  ;; Test composition
  (define nested (hash 'a (hash 'b (hash 'c 42))))
  (define a-lens (hash-lens 'a))
  (define b-lens (hash-lens 'b))
  (define c-lens (hash-lens 'c))
  (define abc-lens (compose-lens a-lens (compose-lens b-lens c-lens)))

  (check-equal? (view abc-lens nested) 42)
  (check-equal? (view abc-lens (set abc-lens nested 100)) 100)

  ;; Lens laws
  (displayln "Testing lens laws...")

  ;; Law 1: Get-Put
  (check-equal? (set name-lens person (view name-lens person))
                person
                "Law 1: Get-Put")

  ;; Law 2: Put-Get
  (check-equal? (view name-lens (set name-lens person "Bob"))
                "Bob"
                "Law 2: Put-Get")

  ;; Law 3: Put-Put
  (check-equal? (set name-lens (set name-lens person "Bob") "Charlie")
                (set name-lens person "Charlie")
                "Law 3: Put-Put")

  (displayln "All tests passed!"))
