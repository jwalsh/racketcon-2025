;;; -*- geiser-scheme-implementation: guile -*-
;;;
;;; hello-geiser.scm - Example Geiser workflow
;;; Experiment 076: Geiser Fundamentals

(use-modules (srfi srfi-1)   ; List operations
             (ice-9 format)) ; Formatted output

;;; ============================================================================
;;; Basic Functions
;;; ============================================================================

(define (factorial n)
  "Calculate factorial of n recursively."
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-iter n)
  "Calculate factorial of n iteratively."
  (let loop ((i n) (acc 1))
    (if (<= i 1)
        acc
        (loop (- i 1) (* acc i)))))

(define (fibonacci n)
  "Calculate nth Fibonacci number (recursive, slow for large n)."
  (cond
   ((<= n 0) 0)
   ((= n 1) 1)
   (else (+ (fibonacci (- n 1))
            (fibonacci (- n 2))))))

(define (fibonacci-iter n)
  "Calculate nth Fibonacci number iteratively (fast)."
  (let loop ((i n) (a 0) (b 1))
    (if (<= i 0)
        a
        (loop (- i 1) b (+ a b)))))

;;; ============================================================================
;;; List Operations (Using SRFI-1)
;;; ============================================================================

(define (sum-list lst)
  "Sum all numbers in a list."
  (fold + 0 lst))

(define (product-list lst)
  "Multiply all numbers in a list."
  (fold * 1 lst))

(define (filter-evens lst)
  "Filter even numbers from list."
  (filter even? lst))

(define (map-square lst)
  "Square all numbers in list."
  (map (lambda (x) (* x x)) lst))

;;; ============================================================================
;;; Higher-Order Functions
;;; ============================================================================

(define (compose f g)
  "Compose two functions: (compose f g)(x) = f(g(x))"
  (lambda (x)
    (f (g x))))

(define (curry f)
  "Curry a two-argument function."
  (lambda (x)
    (lambda (y)
      (f x y))))

(define (partial f . args)
  "Partial application of function f."
  (lambda rest-args
    (apply f (append args rest-args))))

;;; ============================================================================
;;; Geiser-Specific Demonstrations
;;; ============================================================================

(define (demo-autodoc)
  "Demonstrate autodoc feature.

   Try typing the following in a Geiser buffer:
   (map |)
   (string-append |)
   (fold |)

   The autodoc will show function signatures in the minibuffer."
  'see-minibuffer)

(define (demo-completion)
  "Demonstrate completion feature.

   Try typing and pressing TAB:
   str<TAB>     → completes to string- functions
   fol<TAB>     → completes to fold functions
   with-<TAB>   → shows all with-* forms"
  'use-tab-key)

(define (demo-jump-to-definition)
  "Demonstrate jump-to-definition.

   Place cursor on a function name and press M-.
   Examples:
   - Place on 'factorial' and press M-.
   - Place on 'map' and press M-. (jumps to Guile source)
   - Press M-, to return"
  'use-m-dot)

;;; ============================================================================
;;; Test Functions
;;; ============================================================================

(define (test-factorial)
  "Test factorial implementations."
  (display "=== Factorial Tests ===\n")
  (for-each
   (lambda (n)
     (format #t "  ~a! = ~a (recursive) = ~a (iterative)\n"
             n
             (factorial n)
             (factorial-iter n)))
   (iota 8))
  (newline))

(define (test-fibonacci)
  "Test Fibonacci implementations."
  (display "=== Fibonacci Tests ===\n")
  (for-each
   (lambda (n)
     (format #t "  fib(~a) = ~a (iterative)\n"
             n
             (fibonacci-iter n)))
   (iota 15))
  (newline))

(define (test-list-ops)
  "Test list operations."
  (display "=== List Operations Tests ===\n")
  (let ((nums '(1 2 3 4 5 6 7 8 9 10)))
    (format #t "  Original: ~a\n" nums)
    (format #t "  Sum: ~a\n" (sum-list nums))
    (format #t "  Product: ~a\n" (product-list nums))
    (format #t "  Evens: ~a\n" (filter-evens nums))
    (format #t "  Squared: ~a\n" (map-square nums))
    (newline)))

(define (test-higher-order)
  "Test higher-order functions."
  (display "=== Higher-Order Functions Tests ===\n")

  ;; Composition
  (let ((add1-then-square (compose (lambda (x) (* x x))
                                   (lambda (x) (+ x 1)))))
    (format #t "  (add1 then square)(5) = ~a\n"
            (add1-then-square 5)))

  ;; Currying
  (let ((add-curried (curry +)))
    (format #t "  (curried +)(10)(5) = ~a\n"
            ((add-curried 10) 5)))

  ;; Partial application
  (let ((add-10 (partial + 10)))
    (format #t "  (partial + 10)(5) = ~a\n"
            (add-10 5)))

  (newline))

;;; ============================================================================
;;; Interactive Workflow Example
;;; ============================================================================

(define (run-all-tests)
  "Run all tests - use this in Geiser REPL.

   Workflow:
   1. Open this file in Emacs
   2. Start Geiser REPL: M-x run-guile
   3. Eval buffer: C-c C-b
   4. In REPL, run: (run-all-tests)
   5. Try M-. on any function to jump to definition
   6. Use C-c C-d d to show documentation"
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════╗\n")
  (display "║         GEISER EXPERIMENT 076: Test Suite               ║\n")
  (display "╚══════════════════════════════════════════════════════════╝\n")
  (display "\n")
  (test-factorial)
  (test-fibonacci)
  (test-list-ops)
  (test-higher-order)
  (display "✓ All tests complete!\n")
  (display "\n")
  (display "Try these Geiser features:\n")
  (display "  • M-. on any function name (jump to definition)\n")
  (display "  • C-c C-d d on any symbol (show documentation)\n")
  (display "  • C-c C-m x on any expression (macroexpand)\n")
  (display "  • TAB after typing 'str' (completion)\n")
  (newline))

;;; ============================================================================
;;; Module Export (for Guile 3)
;;; ============================================================================

;; If you want to use this as a module:
;; (define-module (hello-geiser)
;;   #:export (factorial
;;             fibonacci
;;             run-all-tests))
