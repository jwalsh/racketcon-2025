#lang racket

;; Syntax-Spec Style Grammar for dupdate DSL
;; Formal grammar using nonterminal/exporting notation

(provide (all-defined-out))

;; ============================================================================
;; SYNTAX-SPEC STYLE GRAMMAR
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         SYNTAX-SPEC GRAMMAR FOR DUPDATE DSL                 ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "Grammar using syntax-spec notation:")
(displayln "(nonterminal/exporting <name> ...)")
(displayln "  #:binding - specifies binding forms")
(displayln "  #:re-export - re-exports bound names")
(newline)

;; ============================================================================
;; CORE GRAMMAR DEFINITION
;; ============================================================================

(displayln "═══ CORE GRAMMAR ═══")
(newline)

(displayln ";; Main pattern grammar")
(displayln "(nonterminal/exporting pat")
(displayln "  #:description \\\"pattern for matching and binding\\\"")
(displayln "  ")
(displayln "  ;; Wildcard: matches anything, binds nothing")
(displayln "  _")
(displayln "  ")
(displayln "  ;; Variable: binds matched value")
(displayln "  v:pattern-var")
(displayln "  #:binding (export v)")
(displayln "  ")
(displayln "  ;; Literal: exact match")
(displayln "  l:literal")
(displayln "  ")
(displayln "  ;; Cons pair")
(displayln "  (cons p1:pat p2:pat)")
(displayln "  #:binding (re-export p1 p2)")
(displayln "  ")
(displayln "  ;; List")
(displayln "  (list p:pat ...)")
(displayln "  #:binding (re-export p)")
(displayln "  ")
(displayln "  ;; Struct")
(displayln "  (struct-name:id p:pat ...)")
(displayln "  #:binding (re-export p)")
(displayln "  ")
(displayln "  ;; Predicate guard")
(displayln "  (? pred:expr p:pat)")
(displayln "  #:binding (re-export p)")
(displayln "  ")
(displayln "  ;; Conjunction: all must match")
(displayln "  (and p1:pat p2:pat)")
(displayln "  #:binding (re-export p1 p2)")
(displayln "  ")
(displayln "  ;; Disjunction: any may match")
(displayln "  (or p1:pat p2:pat)")
(displayln "  #:binding (export (union p1 p2)))")
(newline)

;; ============================================================================
;; OPTIC GRAMMAR
;; ============================================================================

(displayln "═══ OPTIC GRAMMAR ═══")
(newline)

(displayln ";; Optic (lens path) grammar")
(displayln "(nonterminal/exporting optic")
(displayln "  #:description \\\"lens or path to focus\\\"")
(displayln "  ")
(displayln "  ;; Simple field access")
(displayln "  field:id")
(displayln "  ")
(displayln "  ;; Composed path")
(displayln "  (field:id . rest:optic)")
(displayln "  ")
(displayln "  ;; List index")
(displayln "  [n:nat]")
(displayln "  ")
(displayln "  ;; Predefined lens")
(displayln "  lens:id)")
(newline)

;; ============================================================================
;; DUPDATE GRAMMAR
;; ============================================================================

(displayln "═══ DUPDATE GRAMMAR ═══")
(newline)

(displayln ";; Update expression grammar")
(displayln "(nonterminal/exporting dupdate")
(displayln "  #:description \\\"deep update expression\\\"")
(displayln "  ")
(displayln "  ;; Single update")
(displayln "  [o:optic e:expr]")
(displayln "  #:binding (scope (import o) e)")
(displayln "  ")
(displayln "  ;; Multiple updates")
(displayln "  [o:optic e:expr] ...")
(displayln "  #:binding (scope (import o) e)")
(displayln "  ")
(displayln "  ;; Conditional update")
(displayln "  #:when pred:expr")
(displayln "  [o:optic e:expr]")
(displayln "  #:binding (scope (import o) e pred))")
(newline)

;; ============================================================================
;; COMPLETE EXAMPLE
;; ============================================================================

(displayln "═══ COMPLETE EXAMPLE ═══")
(newline)

(displayln "Grammar in action:")
(displayln "")
(displayln "(dupdate target")
(displayln "  [car (* 2 a)]        ; update first element")
(displayln "  [cdr (+ b 10)])      ; update second element")
(newline)

(displayln "Expansion:")
(displayln "  1. Parse optic 'car' → identifies car-lens")
(displayln "  2. Parse expr '(* 2 a)' → function to apply")
(displayln "  3. Bind 'a' to (view car-lens target)")
(displayln "  4. Apply: (set car-lens target (* 2 a))")
(displayln "  5. Thread result to next update")
(newline)

;; ============================================================================
;; BINDING SEMANTICS
;; ============================================================================

(displayln "═══ BINDING SEMANTICS ═══")
(newline)

(displayln "#:binding forms:")
(newline)

(displayln "1. (export v)")
(displayln "   - Bind variable 'v' in pattern")
(displayln "   - Available in expression context")
(newline)

(displayln "2. (re-export p)")
(displayln "   - Re-export all bindings from sub-pattern 'p'")
(displayln "   - Propagate nested bindings up")
(newline)

(displayln "3. (scope (import o) e)")
(displayln "   - Bindings from optic 'o' available in 'e'")
(displayln "   - Create lexical scope for update expression")
(newline)

(displayln "4. (union p1 p2)")
(displayln "   - Union of bindings from p1 and p2")
(displayln "   - For 'or' patterns (either set)")
(newline)

;; ============================================================================
;; EXAMPLE: PATTERN WITH BINDINGS
;; ============================================================================

(displayln "═══ EXAMPLE: Pattern Bindings ═══")
(newline)

(displayln "Pattern:")
(displayln "  (and (cons a b)")
(displayln "       (? pair? p))")
(newline)

(displayln "Binding analysis:")
(displayln "  • (cons a b) exports: {a, b}")
(displayln "  • (? pair? p) exports: {p}")
(displayln "  • (and ...) re-exports: {a, b, p}")
(newline)

(displayln "All three names available in match body!")
(newline)

;; ============================================================================
;; EXAMPLE: OPTIC WITH BINDINGS
;; ============================================================================

(displayln "═══ EXAMPLE: Optic Bindings ═══")
(newline)

(displayln "Update expression:")
(displayln "  (dupdate rect")
(displayln "    [top-left.x (+ x 10)]")
(displayln "    [width (* w 2)])")
(newline)

(displayln "Binding analysis:")
(displayln "  • Optic 'top-left.x' focuses on x coordinate")
(displayln "  • In expr '(+ x 10)', 'x' bound to current value")
(displayln "  • Optic 'width' focuses on width field")
(displayln "  • In expr '(* w 2)', 'w' bound to current width")
(newline)

;; ============================================================================
;; REFERENCE IMPLEMENTATION
;; ============================================================================

(displayln "═══ REFERENCE IMPLEMENTATION ═══")
(newline)

(struct lens (getter setter) #:transparent)

(define-syntax-rule (simple-dupdate target [optic-id expr])
  (let* ([optic (resolve-optic 'optic-id)]
         [current-value ((lens-getter optic) target)]
         [new-value (let ([optic-id current-value]) expr)])
    ((lens-setter optic) target new-value)))

(displayln "Macro expansion:")
(displayln "  (simple-dupdate pair [car (* 2 a)])")
(displayln "  ")
(displayln "  Expands to:")
(displayln "  (let* ([optic car-lens]")
(displayln "         [current-value (car pair)]")
(displayln "         [new-value (let ([a current-value])")
(displayln "                      (* 2 a))])")
(displayln "    (cons new-value (cdr pair)))")
(newline)

;; Example: Resolve optic names
(define (resolve-optic name)
  (case name
    [(car) (lens car (λ (p v) (cons v (cdr p))))]
    [(cdr) (lens cdr (λ (p v) (cons (car p) v)))]
    [else (error 'resolve-optic "unknown optic: ~a" name)]))

(displayln "Example:")
(define test-pair '(10 . 20))

(displayln (format "  Original: ~a" test-pair))

(define updated
  (simple-dupdate test-pair [car (* 2 a)]))

(displayln (format "  After [car (* 2 a)]: ~a" updated))
(newline)

;; ============================================================================
;; NESTED OPTICS
;; ============================================================================

(displayln "═══ NESTED OPTICS ═══")
(newline)

(displayln "Grammar for dot notation:")
(displayln "  optic := id")
(displayln "         | id.optic")
(newline)

(displayln "Example: top-left.x")
(displayln "  • 'top-left' is a lens from rect → posn")
(displayln "  • 'x' is a lens from posn → number")
(displayln "  • Composition: rect → posn → number")
(newline)

(displayln "Implementation:")
(displayln "  (define rect-x-lens")
(displayln "    (compose-lens top-left-lens x-lens))")
(newline)

;; ============================================================================
;; SYNTAX-SPEC COMPLETE GRAMMAR
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║              COMPLETE SYNTAX-SPEC GRAMMAR                    ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "#lang racket")
(displayln "(require syntax-spec)")
(displayln "")
(displayln "(syntax-spec")
(displayln "  ;; Pattern grammar")
(displayln "  (nonterminal/exporting pat")
(displayln "    _")
(displayln "    v:pattern-var #:binding (export v)")
(displayln "    l:literal")
(displayln "    (cons p1:pat p2:pat) #:binding (re-export p1 p2)")
(displayln "    (list p:pat ...) #:binding (re-export p)")
(displayln "    (? pred:expr p:pat) #:binding (re-export p)")
(displayln "    (and p1:pat p2:pat) #:binding (re-export p1 p2)")
(displayln "    (or p1:pat p2:pat) #:binding (export (union p1 p2)))")
(displayln "  ")
(displayln "  ;; Optic grammar")
(displayln "  (nonterminal/exporting optic")
(displayln "    field:id")
(displayln "    (field:id . rest:optic)")
(displayln "    lens:id)")
(displayln "  ")
(displayln "  ;; Update expression")
(displayln "  (nonterminal dupdate-clause")
(displayln "    [o:optic e:expr]")
(displayln "    #:binding (scope (import o) e))")
(displayln "  ")
(displayln "  ;; Main dupdate form")
(displayln "  (host-interface/expression")
(displayln "   (dupdate target:expr clause:dupdate-clause ...)")
(displayln "   (thread-updates target clause ...)))")
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "SYNTAX-SPEC FEATURES USED:")
(newline)

(displayln "1. nonterminal/exporting")
(displayln "   - Defines grammar productions")
(displayln "   - Specifies what each form exports")
(newline)

(displayln "2. #:binding annotations")
(displayln "   - (export v) - export variable")
(displayln "   - (re-export p) - propagate bindings")
(displayln "   - (scope (import x) e) - create scope")
(displayln "   - (union p1 p2) - combine binding sets")
(newline)

(displayln "3. host-interface/expression")
(displayln "   - Connects DSL to host language")
(displayln "   - Specifies expansion function")
(newline)

(displayln "KEY CONCEPTS:")
(displayln "  • Patterns bind variables")
(displayln "  • Optics identify focus")
(displayln "  • Expressions use bound names")
(displayln "  • Bindings propagate through nesting")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 046: Syntax-Spec Grammar")
(displayln "═══════════════════════════════════════════════════════════════")
