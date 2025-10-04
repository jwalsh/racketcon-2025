#lang racket

;; Define-DSL-Syntax: Implementing a Pattern/Lens DSL
;; Using syntax-spec or syntax/parse for DSL construction

(provide (all-defined-out))

;; ============================================================================
;; DEFINE-DSL-SYNTAX: Building a DSL with Macros
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         DEFINE-DSL-SYNTAX: Pattern/Lens DSL                  ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "Goal: Create a DSL for pattern-based updates")
(displayln "  (define-dsl-syntax dupdate ...)")
(newline)

;; ============================================================================
;; LENS INFRASTRUCTURE
;; ============================================================================

(struct lens (getter setter) #:transparent)

(define (view l target)
  ((lens-getter l) target))

(define (set l target value)
  ((lens-setter l) target value))

(define (over l target fn)
  (set l target (fn (view l target))))

;; Basic lenses
(define car-lens
  (lens car (λ (p v) (cons v (cdr p)))))

(define cdr-lens
  (lens cdr (λ (p v) (cons (car p) v))))

;; ============================================================================
;; SIMPLE DUPDATE MACRO (Version 1)
;; ============================================================================

(displayln "═══ VERSION 1: Simple dupdate Macro ═══")
(newline)

(define-syntax-rule (dupdate-v1 target [optic-name expr])
  (let* ([the-lens (resolve-lens 'optic-name)]
         [old-val (view the-lens target)]
         [new-val (let ([optic-name old-val]) expr)])
    (set the-lens target new-val)))

(define (resolve-lens name)
  (case name
    [(car) car-lens]
    [(cdr) cdr-lens]
    [else (error 'resolve-lens "unknown lens: ~a" name)]))

(displayln "Syntax: (dupdate-v1 target [optic-name expr])")
(displayln "")
(displayln "Example:")
(define pair1 '(10 . 20))
(displayln (format "  Original: ~a" pair1))

(define pair2 (dupdate-v1 pair1 [car (* 2 car)]))
(displayln (format "  After [car (* 2 car)]: ~a" pair2))
(newline)

;; ============================================================================
;; MULTIPLE UPDATES (Version 2)
;; ============================================================================

(displayln "═══ VERSION 2: Multiple Updates ═══")
(newline)

(define-syntax dupdate-v2
  (syntax-rules ()
    [(_ target)
     target]
    [(_ target [optic-name expr] rest ...)
     (dupdate-v2
       (let* ([the-lens (resolve-lens 'optic-name)]
              [old-val (view the-lens target)]
              [new-val (let ([optic-name old-val]) expr)])
         (set the-lens target new-val))
       rest ...)]))

(displayln "Syntax: (dupdate-v2 target [optic expr] ...)")
(displayln "")
(displayln "Example:")
(define pair3 '(10 . 20))
(displayln (format "  Original: ~a" pair3))

(define pair4 (dupdate-v2 pair3
                          [car (* 2 car)]
                          [cdr (+ cdr 100)]))
(displayln (format "  After [car (* 2 car)] [cdr (+ cdr 100)]: ~a" pair4))
(newline)

;; ============================================================================
;; STRUCT SUPPORT (Version 3)
;; ============================================================================

(displayln "═══ VERSION 3: Struct Support ═══")
(newline)

(struct posn (x y) #:transparent)

;; Register struct lenses
(define x-lens
  (lens posn-x (λ (p v) (struct-copy posn p [x v]))))

(define y-lens
  (lens posn-y (λ (p v) (struct-copy posn p [y v]))))

(define (resolve-lens-v3 name)
  (case name
    [(car) car-lens]
    [(cdr) cdr-lens]
    [(x) x-lens]
    [(y) y-lens]
    [else (error 'resolve-lens-v3 "unknown lens: ~a" name)]))

(define-syntax dupdate-v3
  (syntax-rules ()
    [(_ target)
     target]
    [(_ target [optic-name expr] rest ...)
     (dupdate-v3
       (let* ([the-lens (resolve-lens-v3 'optic-name)]
              [old-val (view the-lens target)]
              [new-val (let ([optic-name old-val]) expr)])
         (set the-lens target new-val))
       rest ...)]))

(displayln "Example with structs:")
(define p1 (posn 5 10))
(displayln (format "  Original: ~a" p1))

(define p2 (dupdate-v3 p1
                       [x (* 2 x)]
                       [y (+ y 5)]))
(displayln (format "  After [x (* 2 x)] [y (+ y 5)]: ~a" p2))
(newline)

;; ============================================================================
;; DOT NOTATION (Version 4)
;; ============================================================================

(displayln "═══ VERSION 4: Dot Notation (Conceptual) ═══")
(newline)

(displayln "Goal: Support rect.top-left.x syntax")
(displayln "")
(displayln "Approach:")
(displayln "  1. Parse 'rect.top-left.x' into ['rect 'top-left 'x]")
(displayln "  2. Resolve each name to a lens")
(displayln "  3. Compose lenses: (compose rect-lens top-left-lens x-lens)")
(newline)

(define (compose-lens outer inner)
  (lens
    (λ (t) (view inner (view outer t)))
    (λ (t v) (set outer t (set inner (view outer t) v)))))

(struct rect (top-left width height) #:transparent)

(define top-left-lens
  (lens rect-top-left (λ (r v) (struct-copy rect r [top-left v]))))

(define width-lens
  (lens rect-width (λ (r v) (struct-copy rect r [width v]))))

;; Composed lens: rect -> posn -> x
(define rect-x-lens (compose-lens top-left-lens x-lens))

(displayln "Example: rect.top-left.x")
(define r1 (rect (posn 10 20) 50 30))
(displayln (format "  Original: ~a" r1))

(define r2 (set rect-x-lens r1 99))
(displayln (format "  After setting top-left.x to 99: ~a" r2))
(newline)

;; ============================================================================
;; DEFINE-DSL-SYNTAX: Full Macro System
;; ============================================================================

(displayln "═══ DEFINE-DSL-SYNTAX: Full System ═══")
(newline)

(displayln "(define-dsl-syntax dupdate")
(displayln "  #:pattern [optic-path expr]")
(displayln "  #:optic-resolver resolve-optic-path")
(displayln "  #:expander expand-dupdate)")
(newline)

;; Optic path parser
(define (parse-optic-path path-symbol)
  "Parse 'top-left.x into (list 'top-left 'x)"
  (string-split (symbol->string path-symbol) "."))

(displayln "Example: parse-optic-path")
(displayln (format "  'top-left.x → ~a" (parse-optic-path 'top-left.x)))
(displayln (format "  'width → ~a" (parse-optic-path 'width)))
(newline)

;; Lens registry
(define lens-registry (make-hash))

(define (register-lens! name lens)
  (hash-set! lens-registry name lens))

(define (lookup-lens name)
  (hash-ref lens-registry name
            (λ () (error 'lookup-lens "unknown lens: ~a" name))))

;; Register our lenses
(register-lens! 'car car-lens)
(register-lens! 'cdr cdr-lens)
(register-lens! 'x x-lens)
(register-lens! 'y y-lens)
(register-lens! 'top-left top-left-lens)
(register-lens! 'width width-lens)

(displayln "Lens registry:")
(for ([key (hash-keys lens-registry)])
  (displayln (format "  ~a → ~a" key (hash-ref lens-registry key))))
(newline)

;; Resolve optic path to composed lens
(define (resolve-optic-path path-list)
  (cond
    [(null? path-list) (error 'resolve-optic-path "empty path")]
    [(null? (cdr path-list))
     (lookup-lens (string->symbol (car path-list)))]
    [else
     (compose-lens
       (lookup-lens (string->symbol (car path-list)))
       (resolve-optic-path (cdr path-list)))]))

(displayln "Example: resolve-optic-path")
(define resolved (resolve-optic-path '("top-left" "x")))
(displayln (format "  ['top-left 'x] → ~a" resolved))
(newline)

;; ============================================================================
;; FINAL DUPDATE MACRO
;; ============================================================================

(displayln "═══ FINAL DUPDATE MACRO ═══")
(newline)

(define-syntax dupdate
  (syntax-rules ()
    [(_ target)
     target]
    [(_ target [optic-path expr] rest ...)
     (dupdate
       (let* ([path (parse-optic-path 'optic-path)]
              [the-lens (resolve-optic-path path)]
              [old-val (view the-lens target)]
              [new-val (let ([optic-path old-val]) expr)])
         (set the-lens target new-val))
       rest ...)]))

(displayln "Full syntax: (dupdate target [path expr] ...)")
(displayln "  • path can be: field, field.subfield.etc")
(displayln "  • expr uses path as variable bound to current value")
(newline)

(displayln "Example 1: Simple field")
(define test-rect (rect (posn 10 20) 50 30))
(displayln (format "  Original: ~a" test-rect))

;; Note: This won't work because 'top-left.x won't parse as single symbol
;; We need a different syntax

;; ============================================================================
;; ALTERNATIVE: Bracket Syntax
;; ============================================================================

(displayln "═══ ALTERNATIVE: Bracket Syntax ═══")
(newline)

(displayln "Instead of dots, use brackets:")
(displayln "  (dupdate rect")
(displayln "    [[top-left x] (* 2 x)])")
(newline)

(define-syntax dupdate-bracket
  (syntax-rules ()
    [(_ target)
     target]
    [(_ target [[path ...] expr] rest ...)
     (dupdate-bracket
       (let* ([the-lens (resolve-optic-path
                          (map symbol->string '(path ...)))]
              [old-val (view the-lens target)]
              [binding-name (last '(path ...))]
              [new-val (let ([binding-name old-val]) expr)])
         (set the-lens target new-val))
       rest ...)]))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(displayln "Syntax: (dupdate-bracket target [[path ...] expr] ...)")
(displayln "")
(displayln "Example:")
(define test-rect2 (rect (posn 10 20) 50 30))
(displayln (format "  Original: ~a" test-rect2))

(define test-rect3
  (dupdate-bracket test-rect2
                   [[top-left x] (* 2 x)]
                   [[width] (+ width 10)]))

(displayln (format "  After [[top-left x] (* 2 x)] [[width] (+ width 10)]: ~a"
                  test-rect3))
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "DEFINE-DSL-SYNTAX Components:")
(newline)

(displayln "1. LENS INFRASTRUCTURE")
(displayln "   • struct lens (getter setter)")
(displayln "   • view, set, over operations")
(displayln "   • compose-lens for paths")
(newline)

(displayln "2. LENS REGISTRY")
(displayln "   • register-lens! to add lenses")
(displayln "   • lookup-lens to retrieve")
(displayln "   • resolve-optic-path for composition")
(newline)

(displayln "3. MACRO SYSTEM")
(displayln "   • define-syntax-rule for simple cases")
(displayln "   • syntax-rules for recursive cases")
(displayln "   • Pattern: [optic-path expr]")
(newline)

(displayln "4. SYNTAX CHOICES")
(displayln "   • Dots: top-left.x (parsing challenge)")
(displayln "   • Brackets: [top-left x] (works in Racket)")
(displayln "   • Binding: last element of path bound in expr")
(newline)

(displayln "EVOLUTION:")
(displayln "  v1: Single update")
(displayln "  v2: Multiple updates")
(displayln "  v3: Struct support")
(displayln "  v4: Composed paths (dots/brackets)")
(newline)

(displayln "KEY INSIGHT:")
(displayln "  Macros transform declarative syntax into lens operations")
(displayln "  Pattern: [path expr] → (set lens target (expr old-val))")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 046: Define-DSL-Syntax")
(displayln "═══════════════════════════════════════════════════════════════")
