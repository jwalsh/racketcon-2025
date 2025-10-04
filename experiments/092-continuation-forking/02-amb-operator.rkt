#lang racket

;; AMB Operator - Non-deterministic Choice
;; McCarthy's ambiguous operator with backtracking

(provide amb fail assert require)

;; ============================================================================
;; AMB Implementation
;; ============================================================================

;; Stack of choice points (continuations)
(define choice-stack '())

;; Current failure continuation
(define fail-continuation #f)

;; AMB: non-deterministic choice
(define-syntax-rule (amb choice ...)
  (let ([choices (list choice ...)])
    (call/cc
     (lambda (k)
       (define (try-choice c rest)
         (call/cc
          (lambda (fail-k)
            (set! choice-stack (cons (lambda () (try-rest rest)) choice-stack))
            (set! fail-continuation fail-k)
            (k c))))

       (define (try-rest choices)
         (if (null? choices)
             (fail)
             (try-choice (car choices) (cdr choices))))

       (try-rest choices)))))

;; FAIL: backtrack to previous choice point
(define (fail)
  (if (null? choice-stack)
      (error "No more choices - AMB tree exhausted")
      (let ([next-choice (car choice-stack)])
        (set! choice-stack (cdr choice-stack))
        (next-choice))))

;; ASSERT: fail if condition is false
(define (assert condition)
  (unless condition
    (fail)))

;; REQUIRE: synonym for assert
(define (require condition)
  (assert condition))

;; ============================================================================
;; Example 1: Basic AMB
;; ============================================================================

(module+ main
  (displayln "=== AMB Operator Examples ===\n")

  ;; Reset state
  (set! choice-stack '())

  (displayln "Example 1: Simple AMB choice")

  (define result
    (call/cc
     (lambda (return)
       (with-handlers ([exn:fail? (lambda (e) (return 'exhausted))])
         (let loop ()
           (define x (amb 1 2 3))
           (displayln (~a "Try x = " x))
           (assert (> x 1))  ; Only accept x > 1
           (return x)
           (loop))))))

  (displayln (~a "First solution: " result))
  (newline))

;; ============================================================================
;; Example 2: Multiple AMB Points
;; ============================================================================

(module+ multiple
  (displayln "=== Multiple AMB Points ===\n")

  (set! choice-stack '())

  (displayln "Example 2: Two AMB points with constraints")

  (define result
    (call/cc
     (lambda (return)
       (with-handlers ([exn:fail? (lambda (e) (return 'no-solution))])
         (let loop ()
           (define x (amb 1 2 3 4))
           (define y (amb 1 2 3 4))

           (displayln (~a "Try x=" x " y=" y))

           ;; Constraint: x + y = 5
           (assert (= (+ x y) 5))

           (return (list x y))
           (loop))))))

  (displayln (~a "Solution: " result))
  (newline))

;; ============================================================================
;; Example 3: All Solutions
;; ============================================================================

(module+ all-solutions
  (displayln "=== Finding All Solutions ===\n")

  (set! choice-stack '())

  (displayln "Example 3: Collect all solutions")

  (define (find-all-solutions thunk)
    (define solutions '())

    (call/cc
     (lambda (return)
       (with-handlers ([exn:fail? (lambda (e) (return (reverse solutions)))])
         (let loop ()
           (define solution (thunk))
           (set! solutions (cons solution solutions))
           (displayln (~a "Found: " solution))
           (fail)  ; Force backtracking
           (loop)))))

    solutions)

  (define all-solutions
    (find-all-solutions
     (lambda ()
       (define x (amb 1 2 3))
       (define y (amb 1 2 3))
       (assert (= (+ x y) 4))
       (list x y))))

  (displayln (~a "\nAll solutions: " all-solutions))
  (newline))

;; ============================================================================
;; Example 4: Pythagorean Triples
;; ============================================================================

(module+ pythagoras
  (displayln "=== Pythagorean Triples ===\n")

  (set! choice-stack '())

  (displayln "Example 4: Find Pythagorean triple (a² + b² = c²)")

  (define (find-pythagorean-triple max-n)
    (define solutions '())

    (call/cc
     (lambda (return)
       (with-handlers ([exn:fail? (lambda (e) (return (reverse solutions)))])
         (let loop ()
           (define a (amb 1 2 3 4 5 6 7 8 9 10))
           (define b (amb 1 2 3 4 5 6 7 8 9 10))
           (define c (amb 1 2 3 4 5 6 7 8 9 10))

           ;; Constraint: a < b < c
           (assert (< a b c))

           ;; Pythagorean relation
           (assert (= (+ (* a a) (* b b)) (* c c)))

           (define solution (list a b c))
           (set! solutions (cons solution solutions))
           (displayln (~a "Found triple: " solution))

           (fail)
           (loop))))))

  (define triples (find-pythagorean-triple 10))
  (displayln (~a "\nAll triples ≤ 10: " triples))
  (newline))

;; ============================================================================
;; Example 5: Logic Puzzle
;; ============================================================================

(module+ logic
  (displayln "=== Logic Puzzle ===\n")

  (set! choice-stack '())

  (displayln "Example 5: Who owns the zebra?")
  (displayln "(Simplified version)")

  (define (zebra-puzzle)
    (call/cc
     (lambda (return)
       (with-handlers ([exn:fail? (lambda (e) (return 'no-solution))])
         (let loop ()
           ;; House colors
           (define red-house (amb 1 2 3))
           (define green-house (amb 1 2 3))
           (define blue-house (amb 1 2 3))

           ;; All different
           (assert (not (= red-house green-house)))
           (assert (not (= red-house blue-house)))
           (assert (not (= green-house blue-house)))

           ;; Pets
           (define dog-house (amb 1 2 3))
           (define cat-house (amb 1 2 3))
           (define zebra-house (amb 1 2 3))

           ;; All different
           (assert (not (= dog-house cat-house)))
           (assert (not (= dog-house zebra-house)))
           (assert (not (= cat-house zebra-house)))

           ;; Constraints
           (assert (= green-house dog-house))  ; Green house has dog
           (assert (not (= red-house cat-house))) ; Red house doesn't have cat

           (return (hash 'red red-house
                        'green green-house
                        'blue blue-house
                        'dog dog-house
                        'cat cat-house
                        'zebra zebra-house))
           (loop))))))

  (define solution (zebra-puzzle))
  (displayln (~a "Solution: " solution))
  (displayln (~a "Zebra is in house " (hash-ref solution 'zebra)))
  (newline))

;; ============================================================================
;; Example 6: Map Coloring
;; ============================================================================

(module+ coloring
  (displayln "=== Map Coloring ===\n")

  (set! choice-stack '())

  (displayln "Example 6: Color a simple map (3 regions)")

  (define (color-map)
    (call/cc
     (lambda (return)
       (with-handlers ([exn:fail? (lambda (e) (return 'no-solution))])
         (let loop ()
           (define r1 (amb 'red 'blue 'green))
           (define r2 (amb 'red 'blue 'green))
           (define r3 (amb 'red 'blue 'green))

           ;; Adjacent regions must differ
           (assert (not (eq? r1 r2)))
           (assert (not (eq? r2 r3)))
           (assert (not (eq? r1 r3)))

           (return (list r1 r2 r3))
           (loop))))))

  (define coloring (color-map))
  (displayln (~a "Map coloring: " coloring))
  (newline))

;; ============================================================================
;; Example 7: N-Queens (Simplified)
;; ============================================================================

(module+ queens
  (displayln "=== N-Queens (4x4) ===\n")

  (set! choice-stack '())

  (displayln "Example 7: Place 4 queens on 4x4 board")

  (define (four-queens)
    (call/cc
     (lambda (return)
       (with-handlers ([exn:fail? (lambda (e) (return 'no-solution))])
         (let loop ()
           ;; Each row must have one queen
           (define q1 (amb 1 2 3 4))  ; Queen in row 1, column q1
           (define q2 (amb 1 2 3 4))
           (define q3 (amb 1 2 3 4))
           (define q4 (amb 1 2 3 4))

           ;; No two queens in same column
           (assert (not (= q1 q2)))
           (assert (not (= q1 q3)))
           (assert (not (= q1 q4)))
           (assert (not (= q2 q3)))
           (assert (not (= q2 q4)))
           (assert (not (= q3 q4)))

           ;; No two queens on same diagonal
           (assert (not (= (abs (- q1 q2)) 1)))
           (assert (not (= (abs (- q1 q3)) 2)))
           (assert (not (= (abs (- q1 q4)) 3)))
           (assert (not (= (abs (- q2 q3)) 1)))
           (assert (not (= (abs (- q2 q4)) 2)))
           (assert (not (= (abs (- q3 q4)) 1)))

           (return (list q1 q2 q3 q4))
           (loop))))))

  (define solution (four-queens))
  (displayln (~a "Solution: " solution))

  ;; Display board
  (when (list? solution)
    (displayln "\nBoard:")
    (for ([row (in-range 1 5)])
      (define col (list-ref solution (- row 1)))
      (define line
        (string-join
         (for/list ([c (in-range 1 5)])
           (if (= c col) "Q" "."))
         " "))
      (displayln line)))

  (newline))

;; ============================================================================
;; Example 8: AMB vs Roulette
;; ============================================================================

(module+ comparison
  (displayln "=== AMB vs Roulette ===\n")

  (displayln "AMB (deterministic search):")
  (displayln "  - Explores ALL possibilities")
  (displayln "  - Backtracks on failure")
  (displayln "  - Finds exact solutions")
  (displayln "  - No probabilities")

  (newline)

  (displayln "Roulette (probabilistic):")
  (displayln "  - Explores weighted paths")
  (displayln "  - Computes distributions")
  (displayln "  - Statistical inference")
  (displayln "  - Probability attached")

  (newline)

  (displayln "=== AMB Operator Examples Complete ==="))

;; ============================================================================
;; Run All Examples
;; ============================================================================

(module+ all
  (require (submod ".." main))
  (require (submod ".." multiple))
  (require (submod ".." all-solutions))
  (require (submod ".." pythagoras))
  (require (submod ".." logic))
  (require (submod ".." coloring))
  (require (submod ".." queens))
  (require (submod ".." comparison)))
