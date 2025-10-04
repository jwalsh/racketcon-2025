#lang racket

;; Pattern Grammar: Formal Definition
;; Using syntax-spec style grammar notation

(provide (all-defined-out))

;; ============================================================================
;; PATTERN GRAMMAR
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║              PATTERN GRAMMAR (Formal BNF)                    ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "PATTERN GRAMMAR:")
(newline)

(displayln "  <pat> := _                          ; wildcard")
(displayln "         | <var>                      ; variable binding")
(displayln "         | <literal>                  ; literal match")
(displayln "         | (cons <pat> <pat>)         ; cons pair")
(displayln "         | (list <pat> ...)           ; list")
(displayln "         | (<struct-name> <pat> ...)  ; struct")
(displayln "         | (? <pred> <pat>)           ; predicate guard")
(displayln "         | (and <pat> ...)            ; conjunction")
(displayln "         | (or <pat> ...)             ; disjunction")
(displayln "         | (not <pat>)                ; negation")
(displayln "         | (quote <datum>)            ; quoted literal")
(displayln "         | (quasiquote <qq-pat>)      ; quasiquote pattern")
(newline)

(displayln "  <literal> := <number>")
(displayln "             | <string>")
(displayln "             | <boolean>")
(displayln "             | <char>")
(newline)

(displayln "  <var> := any identifier")
(newline)

;; ============================================================================
;; PATTERN EXAMPLES BY CATEGORY
;; ============================================================================

(displayln "═══ WILDCARD PATTERN: _ ═══")
(newline)

(define (has-three-elements? lst)
  (match lst
    [(list _ _ _) #t]
    [_ #f]))

(displayln "Example: (list _ _ _) matches any 3-element list")
(displayln (format "  '(1 2 3) → ~a" (has-three-elements? '(1 2 3))))
(displayln (format "  '(a b) → ~a" (has-three-elements? '(a b))))
(newline)

;; ============================================================================
;; VARIABLE PATTERN
;; ============================================================================

(displayln "═══ VARIABLE PATTERN: <var> ═══")
(newline)

(define (first-element lst)
  (match lst
    [(cons x _) x]
    [_ #f]))

(displayln "Example: (cons x _) binds first to x")
(displayln (format "  '(10 20 30) → ~a" (first-element '(10 20 30))))
(newline)

;; ============================================================================
;; LITERAL PATTERN
;; ============================================================================

(displayln "═══ LITERAL PATTERN: <literal> ═══")
(newline)

(define (is-zero? n)
  (match n
    [0 "yes, zero"]
    [_ "not zero"]))

(displayln "Example: literal 0")
(displayln (format "  0 → ~a" (is-zero? 0)))
(displayln (format "  5 → ~a" (is-zero? 5)))
(newline)

;; ============================================================================
;; CONS PATTERN
;; ============================================================================

(displayln "═══ CONS PATTERN: (cons <pat> <pat>) ═══")
(newline)

(define (describe-pair p)
  (match p
    [(cons 'a b) (format "starts with 'a, rest: ~a" b)]
    [(cons a 'b) (format "first: ~a, ends with 'b" a)]
    [(cons a b) (format "general pair: ~a . ~a" a b)]))

(displayln "Example: (cons pattern-1 pattern-2)")
(displayln (format "  '(a . x) → ~a" (describe-pair '(a . x))))
(displayln (format "  '(x . b) → ~a" (describe-pair '(x . b))))
(displayln (format "  '(x . y) → ~a" (describe-pair '(x . y))))
(newline)

;; ============================================================================
;; LIST PATTERN
;; ============================================================================

(displayln "═══ LIST PATTERN: (list <pat> ...) ═══")
(newline)

(displayln "Expansion:")
(displayln "  (list a b c) = (cons a (cons b (cons c '())))")
(newline)

(define (sum-triple lst)
  (match lst
    [(list a b c) (+ a b c)]
    [_ 0]))

(displayln "Example: (list a b c)")
(displayln (format "  '(1 2 3) → sum = ~a" (sum-triple '(1 2 3))))
(displayln (format "  '(1 2) → sum = ~a" (sum-triple '(1 2))))
(newline)

;; ============================================================================
;; STRUCT PATTERN
;; ============================================================================

(displayln "═══ STRUCT PATTERN: (<struct-name> <pat> ...) ═══")
(newline)

(struct point (x y) #:transparent)

(define (point-quadrant p)
  (match p
    [(point (? positive? x) (? positive? y)) "Q1"]
    [(point (? negative? x) (? positive? y)) "Q2"]
    [(point (? negative? x) (? negative? y)) "Q3"]
    [(point (? positive? x) (? negative? y)) "Q4"]
    [_ "origin or axis"]))

(displayln "Example: (point <pat-x> <pat-y>)")
(displayln (format "  ~a → ~a" (point 3 4) (point-quadrant (point 3 4))))
(displayln (format "  ~a → ~a" (point -3 4) (point-quadrant (point -3 4))))
(newline)

;; ============================================================================
;; PREDICATE PATTERN
;; ============================================================================

(displayln "═══ PREDICATE PATTERN: (? <pred> <pat>) ═══")
(newline)

(define (classify-number n)
  (match n
    [(? even? x) (format "even: ~a" x)]
    [(? odd? x) (format "odd: ~a" x)]))

(displayln "Example: (? even? x)")
(displayln (format "  4 → ~a" (classify-number 4)))
(displayln (format "  7 → ~a" (classify-number 7)))
(newline)

;; ============================================================================
;; AND PATTERN
;; ============================================================================

(displayln "═══ AND PATTERN: (and <pat> ...) ═══")
(newline)

(define (check-range n)
  (match n
    [(and (? number?) (? positive?) x) (format "positive number: ~a" x)]
    [_ "not a positive number"]))

(displayln "Example: (and (? number?) (? positive?) x)")
(displayln (format "  5 → ~a" (check-range 5)))
(displayln (format "  -3 → ~a" (check-range -3)))
(displayln (format "  \\\"hi\\\" → ~a" (check-range "hi")))
(newline)

;; ============================================================================
;; OR PATTERN
;; ============================================================================

(displayln "═══ OR PATTERN: (or <pat> ...) ═══")
(newline)

(define (boolean-or-number? v)
  (match v
    [(or (? boolean?) (? number?)) "yes"]
    [_ "no"]))

(displayln "Example: (or (? boolean?) (? number?))")
(displayln (format "  #t → ~a" (boolean-or-number? #t)))
(displayln (format "  42 → ~a" (boolean-or-number? 42)))
(displayln (format "  \\\"hi\\\" → ~a" (boolean-or-number? "hi")))
(newline)

;; ============================================================================
;; QUOTE PATTERN
;; ============================================================================

(displayln "═══ QUOTE PATTERN: (quote <datum>) or '<datum> ═══")
(newline)

(define (symbol-a? s)
  (match s
    ['a "yes, it's 'a"]
    [_ "no"]))

(displayln "Example: 'a (quoted symbol)")
(displayln (format "  'a → ~a" (symbol-a? 'a)))
(displayln (format "  'b → ~a" (symbol-a? 'b)))
(newline)

;; ============================================================================
;; QUASIQUOTE PATTERN
;; ============================================================================

(displayln "═══ QUASIQUOTE PATTERN: `<template> ═══")
(newline)

(define (extract-add expr)
  (match expr
    [`(+ ,a ,b) (format "add ~a and ~a" a b)]
    [_ "not an addition"]))

(displayln "Example: `(+ ,a ,b) matches s-expression")
(displayln (format "  '(+ 1 2) → ~a" (extract-add '(+ 1 2))))
(displayln (format "  '(* 3 4) → ~a" (extract-add '(* 3 4))))
(newline)

;; ============================================================================
;; NESTED PATTERNS
;; ============================================================================

(displayln "═══ NESTED PATTERNS ═══")
(newline)

(struct rect (top-left width height) #:transparent)

(define (origin-rect? r)
  (match r
    [(rect (point 0 0) _ _) "at origin"]
    [_ "not at origin"]))

(displayln "Example: (rect (point 0 0) _ _)")
(displayln (format "  ~a → ~a" (rect (point 0 0) 10 20)
                  (origin-rect? (rect (point 0 0) 10 20))))
(displayln (format "  ~a → ~a" (rect (point 5 5) 10 20)
                  (origin-rect? (rect (point 5 5) 10 20))))
(newline)

;; ============================================================================
;; PATTERN SYNTAX FOR UPDATE-PAIR
;; ============================================================================

(displayln "═══ UPDATE-PAIR SYNTAX ═══")
(newline)

(displayln "Question: What is update-pair []?")
(displayln "Answer:   Syntax for specifying which part to update")
(newline)

(displayln "Conceptual syntax:")
(displayln "  (update-pair [<optic> <expr>])")
(displayln "")
(displayln "Where <optic> identifies the focus:")
(displayln "  car-lens      → first element")
(displayln "  cdr-lens      → second element")
(displayln "  [car cdr]     → nested path")
(newline)

(displayln "Examples:")
(displayln "  (update-pair pair [car (* 2 a)])")
(displayln "    → Update first element using (* 2 a)")
(newline)

(displayln "  (update-pair pair [cdr (+ b 10)])")
(displayln "    → Update second element using (+ b 10)")
(newline)

;; Implementation example
(struct lens (getter setter) #:transparent)

(define car-lens
  (lens car (λ (p v) (cons v (cdr p)))))

(define cdr-lens
  (lens cdr (λ (p v) (cons (car p) v))))

(define (update-with-lens l target expr-fn)
  "Update target using lens and expression function."
  (define old-focus ((lens-getter l) target))
  ((lens-setter l) target (expr-fn old-focus)))

(displayln "Implementation:")
(define test-pair '(10 . 20))

(displayln (format "  pair: ~a" test-pair))

(define updated-car
  (update-with-lens car-lens test-pair (λ (a) (* 2 a))))

(displayln (format "  [car (* 2 a)]: ~a" updated-car))

(define updated-cdr
  (update-with-lens cdr-lens test-pair (λ (b) (+ b 10))))

(displayln (format "  [cdr (+ b 10)]: ~a" updated-cdr))
(newline)

;; ============================================================================
;; SUMMARY TABLE
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                   PATTERN SUMMARY                            ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "┌──────────────────┬──────────────────────┬───────────────────┐")
(displayln "│ Pattern          │ Syntax               │ Example           │")
(displayln "├──────────────────┼──────────────────────┼───────────────────┤")
(displayln "│ Wildcard         │ _                    │ (list _ _)        │")
(displayln "│ Variable         │ x                    │ (cons x rest)     │")
(displayln "│ Literal          │ 42, \\\"hi\\\", #t        │ 0, \\\"hello\\\"        │")
(displayln "│ Cons             │ (cons p1 p2)         │ (cons a b)        │")
(displayln "│ List             │ (list p ...)         │ (list a b c)      │")
(displayln "│ Struct           │ (struct-name p ...)  │ (posn x y)        │")
(displayln "│ Predicate        │ (? pred p)           │ (? even? x)       │")
(displayln "│ And              │ (and p ...)          │ (and p1 p2)       │")
(displayln "│ Or               │ (or p ...)           │ (or p1 p2)        │")
(displayln "│ Quote            │ 'datum               │ 'symbol           │")
(displayln "│ Quasiquote       │ `template            │ `(+ ,a ,b)        │")
(displayln "└──────────────────┴──────────────────────┴───────────────────┘")
(newline)

(displayln "KEY INSIGHT:")
(displayln "  Patterns describe STRUCTURE")
(displayln "  Variables BIND to parts")
(displayln "  Guards add CONSTRAINTS")
(newline)

(displayln "RELATIONSHIP TO LENSES:")
(displayln "  • Pattern identifies structure (like lens path)")
(displayln "  • Variable binding extracts focus (like view)")
(displayln "  • Update syntax combines pattern + lens (dupdate)")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 046: Pattern Grammar")
(displayln "═══════════════════════════════════════════════════════════════")
