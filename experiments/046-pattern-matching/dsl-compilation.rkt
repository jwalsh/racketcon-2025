#lang racket

;; DSL Compilation: From Surface Syntax to Runtime
;; Shows: runtime context, variable reference, set!, modify! special form

(provide (all-defined-out))

;; ============================================================================
;; COMPILATION PHASES
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         DSL COMPILATION: Surface → Runtime                   ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "Compilation phases:")
(displayln "  1. PARSE: Surface syntax → AST")
(displayln "  2. ELABORATE: Establish runtime context")
(displayln "  3. COMPILE: AST → Core forms (variable reference, set!)")
(displayln "  4. RUNTIME: Execute with context")
(newline)

;; ============================================================================
;; PHASE 1: SURFACE SYNTAX
;; ============================================================================

(displayln "═══ PHASE 1: Surface Syntax ═══")
(newline)

(displayln "User writes:")
(displayln "  (modify! rect [top-left.x (+ x 10)])")
(newline)

(displayln "Surface syntax components:")
(displayln "  • modify! - special form")
(displayln "  • rect - target expression")
(displayln "  • [top-left.x (+ x 10)] - update clause")
(displayln "    - top-left.x - optic path")
(displayln "    - (+ x 10) - update expression")
(displayln "    - x - bound variable (current value)")
(newline)

;; ============================================================================
;; PHASE 2: RUNTIME CONTEXT
;; ============================================================================

(displayln "═══ PHASE 2: Establish Runtime Context ═══")
(newline)

(displayln "Runtime context includes:")
(displayln "  • Target binding")
(displayln "  • Lens/optic resolution")
(displayln "  • Current value extraction")
(displayln "  • Mutable or immutable update")
(newline)

(struct context (bindings) #:transparent #:mutable)

(define (make-context)
  (context (make-hash)))

(define (context-bind! ctx name value)
  (hash-set! (context-bindings ctx) name value))

(define (context-lookup ctx name)
  (hash-ref (context-bindings ctx) name
            (λ () (error 'context-lookup "unbound: ~a" name))))

(displayln "Example: Building context")
(define ctx (make-context))
(context-bind! ctx 'x 42)
(context-bind! ctx 'y 100)
(displayln (format "  Context: ~a" (context-bindings ctx)))
(displayln (format "  Lookup 'x: ~a" (context-lookup ctx 'x)))
(newline)

;; ============================================================================
;; PHASE 3: VARIABLE REFERENCE
;; ============================================================================

(displayln "═══ PHASE 3: Variable Reference ═══")
(newline)

(displayln "In (+ x 10), 'x is a VARIABLE REFERENCE")
(displayln "")
(displayln "Compilation:")
(displayln "  1. Identify free variables: x")
(displayln "  2. Bind x to (view lens target)")
(displayln "  3. Create lexical scope for expression")
(newline)

(struct lens (getter setter) #:transparent)

(define (compile-variable-ref var-name ctx)
  "Compile variable reference to lookup"
  (context-lookup ctx var-name))

(displayln "Example:")
(define test-ctx (make-context))
(context-bind! test-ctx 'x 5)

(displayln (format "  Context has x=~a" (context-lookup test-ctx 'x)))
(displayln (format "  Compile (+ x 10):"))
(displayln (format "    x → ~a" (compile-variable-ref 'x test-ctx)))
(displayln (format "    Result: ~a" (+ (compile-variable-ref 'x test-ctx) 10)))
(newline)

;; ============================================================================
;; PHASE 4: SET! vs MODIFY!
;; ============================================================================

(displayln "═══ PHASE 4: set! vs modify! ═══")
(newline)

(displayln "TWO UPDATE STRATEGIES:")
(newline)

(displayln "1. MUTABLE (set!)")
(displayln "   • Direct mutation of location")
(displayln "   • (set! var new-value)")
(displayln "   • Changes value in-place")
(displayln "   • For mutable structs, boxes, parameters")
(newline)

(displayln "2. IMMUTABLE (modify!)")
(displayln "   • Functional update via lens")
(displayln "   • (modify! target [lens expr])")
(displayln "   • Returns new value")
(displayln "   • For immutable structs, persistent data")
(newline)

;; ============================================================================
;; MUTABLE: set! Example
;; ============================================================================

(displayln "═══ MUTABLE: set! ═══")
(newline)

(struct mutable-point (x y) #:transparent #:mutable)

(define mp (mutable-point 10 20))

(displayln "Mutable struct:")
(displayln (format "  Before: ~a" mp))

(set-mutable-point-x! mp 99)

(displayln (format "  After set-mutable-point-x!: ~a" mp))
(displayln "  → Original mutated!")
(newline)

(displayln "Using set! with box:")
(define my-box (box 42))
(displayln (format "  Before: ~a" (unbox my-box)))

(set-box! my-box 100)

(displayln (format "  After set-box!: ~a" (unbox my-box)))
(newline)

;; ============================================================================
;; IMMUTABLE: modify! via Lens
;; ============================================================================

(displayln "═══ IMMUTABLE: modify! via Lens ═══")
(newline)

(struct point (x y) #:transparent)

(define x-lens
  (lens
    point-x
    (λ (p v) (struct-copy point p [x v]))))

(define (modify-lens! lens target expr-fn)
  "Immutable update: returns NEW target"
  (define old-val ((lens-getter lens) target))
  (define new-val (expr-fn old-val))
  ((lens-setter lens) target new-val))

(define p (point 10 20))

(displayln "Immutable struct:")
(displayln (format "  Original: ~a" p))

(define p2 (modify-lens! x-lens p (λ (x) (* 2 x))))

(displayln (format "  After modify: ~a" p2))
(displayln (format "  Original unchanged: ~a" p))
(newline)

;; ============================================================================
;; SPECIAL FORM: modify!
;; ============================================================================

(displayln "═══ SPECIAL FORM: modify! ═══")
(newline)

(displayln "Syntax: (modify! target [optic expr])")
(displayln "")
(displayln "Desugars to:")
(displayln "  (let* ([lens (resolve-optic 'optic)]")
(displayln "         [old-val (view lens target)]")
(displayln "         [new-val (let ([optic old-val]) expr)])")
(displayln "    (set lens target new-val))")
(newline)

;; Registry for optics
(define optic-registry (make-hash))

(define (register-optic! name lens)
  (hash-set! optic-registry name lens))

(define (resolve-optic name)
  (hash-ref optic-registry name
            (λ () (error 'resolve-optic "unknown: ~a" name))))

;; Register lenses
(register-optic! 'x x-lens)

(define-syntax modify!
  (syntax-rules ()
    [(_ target [optic-name expr])
     (let* ([the-lens (resolve-optic 'optic-name)]
            [old-val ((lens-getter the-lens) target)]
            [new-val (let ([optic-name old-val]) expr)])
       ((lens-setter the-lens) target new-val))]))

(displayln "Example: modify! in action")
(define p3 (point 5 10))
(displayln (format "  Original: ~a" p3))

(define p4 (modify! p3 [x (* 2 x)]))

(displayln (format "  After (modify! p3 [x (* 2 x)]): ~a" p4))
(displayln (format "  Original: ~a" p3))
(newline)

;; ============================================================================
;; COMPILATION: Full Expansion
;; ============================================================================

(displayln "═══ COMPILATION: Full Expansion ═══")
(newline)

(displayln "Surface:")
(displayln "  (modify! p [x (+ x 10)])")
(newline)

(displayln "Expands to:")
(displayln "  (let* ([the-lens (resolve-optic 'x)]")
(displayln "         [old-val ((lens-getter the-lens) p)]")
(displayln "         [new-val (let ([x old-val])")
(displayln "                    (+ x 10))])")
(displayln "    ((lens-setter the-lens) p new-val))")
(newline)

(displayln "Core forms used:")
(displayln "  • let* - sequential binding")
(displayln "  • lambda application - ((lens-getter l) t)")
(displayln "  • let - lexical scope for x")
(displayln "  • Variable reference - old-val, new-val")
(displayln "  • No set! - functional update")
(newline)

;; ============================================================================
;; RUNTIME CONTEXT: Complete Example
;; ============================================================================

(displayln "═══ RUNTIME CONTEXT: Complete Example ═══")
(newline)

(struct rect (top-left width height) #:transparent)

(define top-left-lens
  (lens
    rect-top-left
    (λ (r v) (struct-copy rect r [top-left v]))))

(define (compose-lens outer inner)
  (lens
    (λ (t) ((lens-getter inner) ((lens-getter outer) t)))
    (λ (t v) ((lens-setter outer) t
              ((lens-setter inner) ((lens-getter outer) t) v)))))

(define rect-x-lens (compose-lens top-left-lens x-lens))

(register-optic! 'top-left top-left-lens)
(register-optic! 'rect.x rect-x-lens)

(displayln "Nested struct example:")
(define r (rect (point 10 20) 50 30))
(displayln (format "  Original: ~a" r))
(newline)

(displayln "Runtime context for (modify! r [rect.x (+ x 100)]):")
(displayln "")
(displayln "  1. Resolve optic 'rect.x:")
(displayln (format "     → ~a" (resolve-optic 'rect.x)))
(newline)

(displayln "  2. Extract current value:")
(define current-x ((lens-getter rect-x-lens) r))
(displayln (format "     → x = ~a" current-x))
(newline)

(displayln "  3. Bind x in expression context:")
(define expr-ctx (make-context))
(context-bind! expr-ctx 'x current-x)
(displayln (format "     → context: ~a" (context-bindings expr-ctx)))
(newline)

(displayln "  4. Evaluate (+ x 100):")
(define new-x (+ (context-lookup expr-ctx 'x) 100))
(displayln (format "     → ~a" new-x))
(newline)

(displayln "  5. Update via lens:")
(define r2 ((lens-setter rect-x-lens) r new-x))
(displayln (format "     → ~a" r2))
(newline)

;; ============================================================================
;; VARIABLE REFERENCE vs SET!
;; ============================================================================

(displayln "═══ VARIABLE REFERENCE vs SET! ═══")
(newline)

(displayln "VARIABLE REFERENCE:")
(displayln "  • Read-only access")
(displayln "  • (+ x 10) - x is dereferenced")
(displayln "  • Compile to: (context-lookup ctx 'x)")
(newline)

(displayln "SET! (mutation):")
(displayln "  • Write access")
(displayln "  • (set! x 42) - x is assigned")
(displayln "  • Only for mutable locations")
(displayln "  • Compile to: (context-bind! ctx 'x 42)")
(newline)

(displayln "MODIFY! (functional):")
(displayln "  • Immutable update")
(displayln "  • Creates new value")
(displayln "  • No mutation of original")
(displayln "  • Compile to: lens setter application")
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "COMPILATION PIPELINE:")
(newline)

(displayln "1. SURFACE SYNTAX")
(displayln "   (modify! target [optic expr])")
(newline)

(displayln "2. ESTABLISH CONTEXT")
(displayln "   • Bind target")
(displayln "   • Resolve optic")
(displayln "   • Extract current value")
(newline)

(displayln "3. VARIABLE REFERENCE")
(displayln "   • optic bound to current value")
(displayln "   • Available in expr")
(displayln "   • Lexical scope via let")
(newline)

(displayln "4. UPDATE STRATEGY")
(displayln "   • set! - mutation (mutable)")
(displayln "   • modify! - functional (immutable)")
(displayln "   • Lens setter - always functional")
(newline)

(displayln "KEY FORMS:")
(displayln "  • Variable reference: identifier lookup")
(displayln "  • set! - assignment (mutable only)")
(displayln "  • modify! - special form for immutable updates")
(displayln "  • Runtime context - bindings environment")
(newline)

(displayln "COMPILATION:")
(displayln "  Surface → Expand → Core forms → Runtime")
(displayln "  modify! → let* + lens ops → execution")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 046: DSL Compilation")
(displayln "═══════════════════════════════════════════════════════════════")
