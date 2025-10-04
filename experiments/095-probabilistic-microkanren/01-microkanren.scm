;; microKanren: Minimal Logic Programming Core
;; Based on Hemann & Friedman (2013)
;; R7RS Scheme implementation

(import (scheme base)
        (scheme write))

;; ============================================================================
;; State Representation
;; ============================================================================

;; State = (substitution . counter)
(define empty-state '(() . 0))

;; ============================================================================
;; Variables
;; ============================================================================

(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x y) (= (vector-ref x 0) (vector-ref y 0)))

;; ============================================================================
;; Substitution
;; ============================================================================

;; Walk: follow variable bindings
(define (walk u s)
  (let ((pr (and (var? u) (assv u s))))
    (if pr (walk (cdr pr) s) u)))

;; Extend substitution
(define (ext-s x v s)
  (cons `(,x . ,v) s))

;; ============================================================================
;; Unification
;; ============================================================================

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

;; ============================================================================
;; State Constructor
;; ============================================================================

(define (state s c) (cons s c))
(define (state-s st) (car st))
(define (state-c st) (cdr st))

;; ============================================================================
;; Goal Constructors
;; ============================================================================

;; Unification goal
(define (≡ u v)
  (lambda (st)
    (let ((s (unify u v (state-s st))))
      (if s (list (state s (state-c st))) '()))))

;; Fresh variable
(define (call/fresh f)
  (lambda (st)
    (let ((c (state-c st)))
      ((f (var c)) (state (state-s st) (+ c 1))))))

;; ============================================================================
;; Stream Operations
;; ============================================================================

;; Merge streams (disjunction)
(define (mplus s1 s2)
  (cond
    ((null? s1) s2)
    ((procedure? s1) (lambda () (mplus s2 (s1))))
    (else (cons (car s1) (mplus (cdr s1) s2)))))

;; Bind goal to stream (conjunction)
(define (bind s g)
  (cond
    ((null? s) '())
    ((procedure? s) (lambda () (bind (s) g)))
    (else (mplus (g (car s)) (bind (cdr s) g)))))

;; ============================================================================
;; Goal Combinators
;; ============================================================================

;; Disjunction: succeed if either goal succeeds
(define (disj g1 g2)
  (lambda (st) (mplus (g1 st) (g2 st))))

;; Conjunction: succeed if both goals succeed
(define (conj g1 g2)
  (lambda (st) (bind (g1 st) g2)))

;; ============================================================================
;; Interface Macros
;; ============================================================================

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj g0 (conj g ...)))
    ((_ (x) g0 g ...)
     (call/fresh (lambda (x) (conj g0 (conj g ...)))))
    ((_ (x y ...) g0 g ...)
     (call/fresh
      (lambda (x)
        (fresh (y ...) g0 g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...)
     (disj (conj g0 (conj g ...)) ...))))

;; Helper for multiple disj
(define-syntax disj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (disj g0 (disj+ g ...)))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (conj g0 (conj+ g ...)))))

;; ============================================================================
;; Reification (extract answers)
;; ============================================================================

(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s)
                      (walk* (cdr v) s)))
      (else v))))

(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v)
       (ext-s v (reify-name (length s)) s))
      ((pair? v)
       (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))

(define (reify-name n)
  (string->symbol
   (string-append "_." (number->string n))))

(define (reify v st)
  (let ((v (walk* v (state-s st))))
    (walk* v (reify-s v '()))))

;; ============================================================================
;; Run Interface
;; ============================================================================

;; Take n answers from stream
(define (take n s)
  (cond
    ((and n (zero? n)) '())
    ((null? s) '())
    ((procedure? s) (take n (s)))
    (else (cons (car s) (take (and n (- n 1)) (cdr s))))))

;; Run goal and take n answers
(define (run n q goal)
  (map (lambda (st) (reify q st))
       (take n (goal empty-state))))

(define-syntax run*
  (syntax-rules ()
    ((_ (q) g0 g ...)
     (run #f q (fresh (q) g0 g ...)))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (let ((answers
            (map (lambda (st) (reify q st))
                 (take n ((fresh (q) g0 g ...) empty-state)))))
       answers))))

;; ============================================================================
;; Common Relations
;; ============================================================================

;; Append relation
(define (appendo l1 l2 out)
  (conde
    [(≡ '() l1) (≡ l2 out)]
    [(fresh (a d res)
       (≡ `(,a . ,d) l1)
       (≡ `(,a . ,res) out)
       (appendo d l2 res))]))

;; Member relation
(define (membero x l)
  (fresh (a d)
    (≡ `(,a . ,d) l)
    (conde
      [(≡ x a)]
      [(membero x d)])))

;; ============================================================================
;; Examples
;; ============================================================================

(display "=== microKanren Examples ===\n\n")

;; Example 1: Simple unification
(display "Example 1: (run* (q) (≡ q 5))\n")
(display (run* (q) (≡ q 5)))
(newline)
(newline)

;; Example 2: Fresh variable
(display "Example 2: (run* (q) (fresh (x) (≡ x 5) (≡ q x)))\n")
(display (run* (q) (fresh (x) (≡ x 5) (≡ q x))))
(newline)
(newline)

;; Example 3: Disjunction
(display "Example 3: (run* (q) (conde [(≡ q 'tea)] [(≡ q 'coffee)]))\n")
(display (run* (q) (conde [(≡ q 'tea)] [(≡ q 'coffee)])))
(newline)
(newline)

;; Example 4: Appendo
(display "Example 4: (run* (q) (appendo '(1 2) '(3 4) q))\n")
(display (run* (q) (appendo '(1 2) '(3 4) q)))
(newline)
(newline)

;; Example 5: Appendo inverse
(display "Example 5: (run 5 (q) (fresh (x y) (appendo x y '(1 2 3)) (≡ q `(,x ,y))))\n")
(display (run 5 (q) (fresh (x y) (appendo x y '(1 2 3)) (≡ q `(,x ,y)))))
(newline)
(newline)

;; Example 6: Membero
(display "Example 6: (run* (q) (membero q '(a b c)))\n")
(display (run* (q) (membero q '(a b c))))
(newline)
(newline)

(display "=== microKanren Complete ===\n")
