#lang rosette

;; Rosette + Z3 Integration
;; Using Rosette's symbolic evaluation and Z3 solver

(require racket/format)
(provide (all-defined-out))

;; ============================================================================
;; Basic Symbolic Execution
;; ============================================================================

(module+ basic
  (displayln "=== Basic Symbolic Execution ===\n")

  ;; Create symbolic variables
  (define-symbolic x y integer?)

  (displayln "Example 1: Find x, y where x + y = 10 and x > y")

  (define solution1
    (solve
     (begin
       (assert (= (+ x y) 10))
       (assert (> x y)))))

  (displayln (~a "Solution: " solution1))
  (displayln (~a "x = " (evaluate x solution1)))
  (displayln (~a "y = " (evaluate y solution1)))
  (newline)

  ;; Example 2: Multiple constraints
  (displayln "Example 2: Find x where 0 < x < 10 and x² = 25")

  (define-symbolic z integer?)
  (define solution2
    (solve
     (begin
       (assert (> z 0))
       (assert (< z 10))
       (assert (= (* z z) 25)))))

  (if (sat? solution2)
      (displayln (~a "Solution: z = " (evaluate z solution2)))
      (displayln "UNSAT: No solution exists (5² = 25 but 5 < 10 is satisfied, but there's no integer solution)"))
  (newline))

;; ============================================================================
;; Verification Examples
;; ============================================================================

(module+ verification
  (displayln "=== Program Verification ===\n")

  ;; Example 1: Verify max function
  (displayln "Example 1: Verify max function is correct")

  (define (my-max a b)
    (if (> a b) a b))

  (define-symbolic* a b integer?)

  (define verify-max
    (verify
     (begin
       (define result (my-max a b))
       (assert (>= result a))
       (assert (>= result b))
       (assert (or (= result a) (= result b))))))

  (if (unsat? verify-max)
      (displayln "✓ max function verified correct")
      (displayln (~a "✗ Counterexample: " verify-max)))
  (newline)

  ;; Example 2: Verify absolute value
  (displayln "Example 2: Verify absolute value function")

  (define (my-abs x)
    (if (< x 0) (- x) x))

  (define-symbolic n integer?)

  (define verify-abs
    (verify
     (begin
       (define result (my-abs n))
       (assert (>= result 0))
       (assert (or (= result n) (= result (- n)))))))

  (if (unsat? verify-abs)
      (displayln "✓ abs function verified correct")
      (displayln (~a "✗ Counterexample: " verify-abs)))
  (newline)

  ;; Example 3: Find bug in incorrect implementation
  (displayln "Example 3: Find bug in incorrect division")

  (define (buggy-div a b)
    ;; Bug: doesn't check for division by zero
    (quotient a b))

  (define-symbolic* p q integer?)

  (define bug-search
    (verify
     (begin
       (define result (buggy-div p q))
       (assert (integer? result)))))

  (if (sat? bug-search)
      (displayln (~a "✗ Bug found! Counterexample: p="
                    (evaluate p bug-search)
                    ", q="
                    (evaluate q bug-search)))
      (displayln "✓ No bugs found"))
  (newline))

;; ============================================================================
;; Synthesis Examples
;; ============================================================================

(module+ synthesis
  (displayln "=== Program Synthesis ===\n")

  ;; Example 1: Synthesize linear function
  (displayln "Example 1: Synthesize f(x) = ax + b")
  (displayln "Spec: f(0)=2, f(1)=5, f(2)=8\n")

  (define-symbolic* c0 c1 integer?)

  (define (f-template x)
    (+ (* c0 x) c1))

  (define synth1
    (synthesize
     #:forall '()
     #:guarantee
     (begin
       (assert (= (f-template 0) 2))
       (assert (= (f-template 1) 5))
       (assert (= (f-template 2) 8)))))

  (if (sat? synth1)
      (begin
        (displayln (~a "Synthesized: f(x) = "
                      (evaluate c0 synth1) "x + "
                      (evaluate c1 synth1)))
        (displayln (~a "Answer: f(x) = 3x + 2")))
      (displayln "No solution found"))
  (newline)

  ;; Example 2: Synthesize bit manipulation
  (displayln "Example 2: Synthesize bit operation")
  (displayln "Spec: clear lowest set bit\n")

  (require rosette/lib/synthax)

  (define-symbolic* input output (bitvector 8))

  ;; Template: x & (x + k) for some constant k
  (define-symbolic* k (bitvector 8))

  (define (clear-lowest-template x)
    (bvand x (bvadd x k)))

  (define synth2
    (synthesize
     #:forall (list input)
     #:guarantee
     (begin
       ;; Test cases
       (assert (= (clear-lowest-template (bv 12 8)) (bv 8 8)))   ; 1100 -> 1000
       (assert (= (clear-lowest-template (bv 10 8)) (bv 8 8)))   ; 1010 -> 1000
       (assert (= (clear-lowest-template (bv 7 8)) (bv 6 8))))))  ; 0111 -> 0110

  (if (sat? synth2)
      (displayln (~a "Synthesized: x & (x + " (evaluate k synth2) ")"))
      (displayln "No solution found"))
  (newline))

;; ============================================================================
;; Theory-Specific Examples
;; ============================================================================

(module+ theories
  (displayln "=== Multi-Theory Constraints ===\n")

  ;; Example 1: Bit-vectors
  (displayln "Example 1: Bit-vector arithmetic")

  (define-symbolic bv1 bv2 (bitvector 8))

  (define bv-solution
    (solve
     (begin
       (assert (bveq (bvadd bv1 bv2) (bv 255 8)))
       (assert (bvult bv1 (bv 128 8))))))

  (if (sat? bv-solution)
      (displayln (~a "Solution: bv1=" (evaluate bv1 bv-solution)
                    ", bv2=" (evaluate bv2 bv-solution)))
      (displayln "UNSAT"))
  (newline)

  ;; Example 2: Real arithmetic
  (displayln "Example 2: Real number constraints")

  (define-symbolic r1 r2 real?)

  (define real-solution
    (solve
     (begin
       (assert (= (+ r1 r2) 10.5))
       (assert (> r1 r2))
       (assert (> r1 5.0)))))

  (if (sat? real-solution)
      (displayln (~a "Solution: r1=" (evaluate r1 real-solution)
                    ", r2=" (evaluate r2 real-solution)))
      (displayln "UNSAT"))
  (newline)

  ;; Example 3: Mixed theories
  (displayln "Example 3: Integer + Boolean constraints")

  (define-symbolic i integer?)
  (define-symbolic b boolean?)

  (define mixed-solution
    (solve
     (begin
       (assert (if b (> i 10) (< i 0)))
       (assert (= i 15)))))

  (if (sat? mixed-solution)
      (displayln (~a "Solution: i=" (evaluate i mixed-solution)
                    ", b=" (evaluate b mixed-solution)))
      (displayln "UNSAT"))
  (newline))

;; ============================================================================
;; Quantifier Examples
;; ============================================================================

(module+ quantifiers
  (displayln "=== Quantified Formulas ===\n")

  ;; Example 1: Universal quantification
  (displayln "Example 1: ∀x. x > 5 → x² > 25")

  (define-symbolic x integer?)

  (define forall-check
    (verify
     #:assume (assert (> x 5))
     #:guarantee (assert (> (* x x) 25))))

  (if (unsat? forall-check)
      (displayln "✓ Property holds for all x > 5")
      (displayln (~a "✗ Counterexample: x=" (evaluate x forall-check))))
  (newline)

  ;; Example 2: Existential quantification
  (displayln "Example 2: ∃x. x² = 49 ∧ x > 0")

  (define-symbolic y integer?)

  (define exists-check
    (solve
     (begin
       (assert (= (* y y) 49))
       (assert (> y 0)))))

  (if (sat? exists-check)
      (displayln (~a "✓ Witness: y=" (evaluate y exists-check)))
      (displayln "✗ No such x exists"))
  (newline))

;; ============================================================================
;; Array Theory
;; ============================================================================

(module+ arrays
  (displayln "=== Array Theory ===\n")

  (displayln "Example: Array with symbolic indices")

  (define arr (make-vector 5 0))
  (define-symbolic* i j integer?)

  (vector-set! arr 0 10)
  (vector-set! arr 1 20)

  (define arr-solution
    (solve
     (begin
       (assert (>= i 0))
       (assert (< i 5))
       (assert (= (vector-ref arr i) 20)))))

  (if (sat? arr-solution)
      (displayln (~a "Solution: i=" (evaluate i arr-solution)))
      (displayln "UNSAT"))
  (newline))

;; ============================================================================
;; Advanced: Solver Configuration
;; ============================================================================

(module+ config
  (displayln "=== Solver Configuration ===\n")

  (require rosette/solver/z3)

  ;; Use Z3 explicitly
  (current-solver (z3))

  (displayln "Current solver: Z3")
  (displayln (~a "Solver path: " (z3)))

  ;; You can configure timeouts, options, etc.
  ;; (current-solver (z3 #:options '("timeout=1000")))

  (newline))

;; ============================================================================
;; Run All Examples
;; ============================================================================

(module+ all
  (require (submod ".." basic))
  (require (submod ".." verification))
  (require (submod ".." synthesis))
  (require (submod ".." theories))
  (require (submod ".." quantifiers))
  (require (submod ".." arrays))
  (require (submod ".." config))
  (displayln "=== Rosette + Z3 Complete ==="))
