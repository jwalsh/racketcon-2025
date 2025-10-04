#lang racket

;; Ocular Patdown: Prism Deep Dive
;; Comprehensive exploration of prisms

(require ocular-patdown)

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         OCULAR PATDOWN: PRISM DEEP DIVE                      ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

;; ============================================================================
;; WHAT IS A PRISM?
;; ============================================================================

(displayln "═══ WHAT IS A PRISM? ═══")
(newline)

(displayln "DEFINITION:")
(displayln "  A prism is an optic that:")
(displayln "  • Can have 0 or 1 focus (partial/optional)")
(displayln "  • Allows partial conversion between types")
(displayln "  • Handles subtypes or asymmetric transformations")
(newline)

(displayln "HIERARCHY:")
(displayln "  Iso ⊂ Prism ⊂ Traversal")
(displayln "  • All isomorphisms are prisms")
(displayln "  • All prisms are traversals")
(displayln "  • But NOT all prisms are isos (not bidirectional)")
(newline)

;; ============================================================================
;; CORE OPERATIONS
;; ============================================================================

(displayln "═══ CORE PRISM OPERATIONS ═══")
(newline)

(displayln "1. prism-project (extract focus)")
(displayln "   Signature: prism-project : Prism → Target → (or/c Focus prism-absent)")
(displayln "   Tries to extract focus, returns prism-absent if fails")
(newline)

(displayln "2. prism-inject (insert focus)")
(displayln "   Signature: prism-inject : Prism → Focus → Target")
(displayln "   Converts focus back to target (always succeeds)")
(newline)

(displayln "3. prism-compose (combine prisms)")
(displayln "   Signature: prism-compose : Prism → Prism → Prism")
(displayln "   Compose prisms to create new prism")
(newline)

;; ============================================================================
;; BUILT-IN PRISM: string-number-prism
;; ============================================================================

(displayln "═══ BUILT-IN: string-number-prism ═══")
(newline)

(displayln "Purpose: Convert between strings and numbers")
(displayln "")

(displayln "prism-project (string -> number):")
(displayln (format "  (prism-project string-number-prism \\\"42\\\") => ~a"
                  (prism-project string-number-prism "42")))
(displayln (format "  (prism-project string-number-prism \\\"123\\\") => ~a"
                  (prism-project string-number-prism "123")))
(newline)

(displayln "Handles failure (non-numeric string):")
(displayln (format "  (prism-project string-number-prism \\\"hello\\\") => ~a"
                  (prism-project string-number-prism "hello")))
(displayln (format "  (prism-project string-number-prism \\\"hello\\\" #f) => ~a"
                  (prism-project string-number-prism "hello" #f)))
(newline)

(displayln "prism-inject (number -> string):")
(displayln (format "  (prism-inject string-number-prism 42) => ~a"
                  (prism-inject string-number-prism 42)))
(displayln (format "  (prism-inject string-number-prism 123) => ~a"
                  (prism-inject string-number-prism 123)))
(newline)

;; ============================================================================
;; PRISM LAWS
;; ============================================================================

(displayln "═══ PRISM LAWS ═══")
(newline)

(displayln "Law 1: PROJECT-INJECT")
(displayln "  (prism-project p (prism-inject p focus)) = focus")
(displayln "  Injecting then projecting returns original focus")
(newline)

(define test-num 42)
(define injected (prism-inject string-number-prism test-num))
(define projected (prism-project string-number-prism injected))

(displayln (format "  Test: num = ~a" test-num))
(displayln (format "  Inject: ~a" injected))
(displayln (format "  Project: ~a" projected))
(displayln (format "  Equal? ~a ✓" (equal? test-num projected)))
(newline)

(displayln "Law 2: INJECT-PROJECT (when focus exists)")
(displayln "  (prism-inject p (prism-project p target)) = target")
(displayln "  If target has focus, round-trip preserves it")
(newline)

(define test-str "123")
(define num (prism-project string-number-prism test-str))
(define back-to-str (prism-inject string-number-prism num))

(displayln (format "  Test: str = ~s" test-str))
(displayln (format "  Project: ~a" num))
(displayln (format "  Inject: ~s" back-to-str))
(displayln (format "  Equal? ~a ✓" (equal? test-str back-to-str)))
(newline)

;; ============================================================================
;; GUARD PRISM
;; ============================================================================

(displayln "═══ GUARD PRISM ═══")
(newline)

(displayln "guard-prism: Create prism based on predicate")
(displayln "  Only projects when predicate is true")
(newline)

(define number-prism (guard-prism number?))

(displayln "Examples with (guard-prism number?):")
(displayln (format "  (prism-project number-prism 42) => ~a"
                  (prism-project number-prism 42)))
(displayln (format "  (prism-project number-prism \\\"hello\\\") => ~a"
                  (prism-project number-prism "hello")))
(displayln (format "  (prism-project number-prism \\\"hello\\\" #f) => ~a"
                  (prism-project number-prism "hello" #f)))
(newline)

(define even-prism (guard-prism even?))

(displayln "Examples with (guard-prism even?):")
(displayln (format "  (prism-project even-prism 10) => ~a"
                  (prism-project even-prism 10)))
(displayln (format "  (prism-project even-prism 7) => ~a"
                  (prism-project even-prism 7)))
(displayln (format "  (prism-project even-prism 7 #f) => ~a"
                  (prism-project even-prism 7 #f)))
(newline)

;; ============================================================================
;; CUSTOM PRISMS
;; ============================================================================

(displayln "═══ CUSTOM PRISMS ═══")
(newline)

(displayln "make-prism: Create custom prisms")
(displayln "  (make-prism project-fn inject-fn)")
(newline)

;; Safe division prism (denominator != 0)
(define safe-reciprocal-prism
  (make-prism
    ; project: only succeeds if not zero
    (λ (n) (if (zero? n) prism-absent (/ 1 n)))
    ; inject: reciprocal back
    (λ (r) (/ 1 r))))

(displayln "Example: safe-reciprocal-prism")
(displayln (format "  (prism-project safe-reciprocal-prism 4) => ~a"
                  (prism-project safe-reciprocal-prism 4)))
(displayln (format "  (prism-project safe-reciprocal-prism 0) => ~a"
                  (prism-project safe-reciprocal-prism 0)))
(displayln (format "  (prism-inject safe-reciprocal-prism 0.25) => ~a"
                  (prism-inject safe-reciprocal-prism 0.25)))
(newline)

;; List head prism (non-empty lists)
(define head-prism
  (make-prism
    ; project: extract first element if non-empty
    (λ (lst) (if (null? lst) prism-absent (car lst)))
    ; inject: create single-element list
    (λ (x) (list x))))

(displayln "Example: head-prism (non-empty lists)")
(displayln (format "  (prism-project head-prism '(1 2 3)) => ~a"
                  (prism-project head-prism '(1 2 3))))
(displayln (format "  (prism-project head-prism '()) => ~a"
                  (prism-project head-prism '())))
(displayln (format "  (prism-inject head-prism 42) => ~a"
                  (prism-inject head-prism 42)))
(newline)

;; ============================================================================
;; PRISM COMPOSITION
;; ============================================================================

(displayln "═══ PRISM COMPOSITION ═══")
(newline)

(displayln "Compose prisms to create complex transformations")
(displayln "")

;; Compose symbol<->string with string-number
(define symbol-number-prism
  (prism-compose symbol<->string string-number-prism))

(displayln "Example: symbol<->string ∘ string-number-prism")
(displayln "")

(displayln "prism-project (symbol -> number):")
(displayln (format "  (prism-project symbol-number-prism '|42|) => ~a"
                  (prism-project symbol-number-prism '|42|)))
(displayln (format "  (prism-project symbol-number-prism 'hello) => ~a"
                  (prism-project symbol-number-prism 'hello)))
(newline)

(displayln "prism-inject (number -> symbol):")
(displayln (format "  (prism-inject symbol-number-prism 123) => ~a"
                  (prism-inject symbol-number-prism 123)))
(newline)

;; ============================================================================
;; PRISMS vs LENSES vs TRAVERSALS
;; ============================================================================

(displayln "═══ PRISMS vs LENSES vs TRAVERSALS ═══")
(newline)

(displayln "┌──────────────┬─────────────┬──────────────┬─────────────┐")
(displayln "│ Feature      │ Lens        │ Prism        │ Traversal   │")
(displayln "├──────────────┼─────────────┼──────────────┼─────────────┤")
(displayln "│ # of foci    │ Exactly 1   │ 0 or 1       │ 0 to N      │")
(displayln "│ Can fail?    │ No          │ Yes          │ Yes (0)     │")
(displayln "│ Get op       │ lens-get    │ prism-project│ trav->list  │")
(displayln "│ Set op       │ lens-set    │ prism-inject │ trav-map    │")
(displayln "│ Use case     │ Required    │ Optional     │ Collection  │")
(displayln "│              │ field       │ field/subtype│ elements    │")
(displayln "└──────────────┴─────────────┴──────────────┴─────────────┘")
(newline)

;; ============================================================================
;; PRACTICAL EXAMPLES
;; ============================================================================

(displayln "═══ PRACTICAL EXAMPLES ═══")
(newline)

(displayln "Example 1: Safe parsing with fallback")

(define (parse-int-or str fallback)
  (define result (prism-project string-number-prism str))
  (if (equal? result prism-absent)
      fallback
      result))

(displayln (format "  (parse-int-or \\\"42\\\" 0) => ~a"
                  (parse-int-or "42" 0)))
(displayln (format "  (parse-int-or \\\"bad\\\" 0) => ~a"
                  (parse-int-or "bad" 0)))
(newline)

(displayln "Example 2: Validation pipeline")

(define positive-prism (guard-prism positive?))

(define (validate-positive-number str)
  (define num (prism-project string-number-prism str))
  (cond
    [(equal? num prism-absent) "not a number"]
    [(equal? (prism-project positive-prism num) prism-absent)
     "not positive"]
    [else num]))

(displayln (format "  (validate-positive-number \\\"42\\\") => ~a"
                  (validate-positive-number "42")))
(displayln (format "  (validate-positive-number \\\"-5\\\") => ~a"
                  (validate-positive-number "-5")))
(displayln (format "  (validate-positive-number \\\"xyz\\\") => ~a"
                  (validate-positive-number "xyz")))
(newline)

(displayln "Example 3: Optional field access")

(struct config (name port [ssl? #:mutable]) #:transparent)

;; Prism for SSL-enabled configs
(define ssl-enabled-prism
  (make-prism
    (λ (cfg) (if (config-ssl? cfg) cfg prism-absent))
    (λ (cfg) cfg)))

(define cfg1 (config "server1" 8080 #t))
(define cfg2 (config "server2" 3000 #f))

(displayln (format "  Config 1 (SSL=true): ~a" cfg1))
(displayln (format "  Config 2 (SSL=false): ~a" cfg2))
(displayln (format "  Project SSL-enabled from cfg1: ~a"
                  (prism-project ssl-enabled-prism cfg1)))
(displayln (format "  Project SSL-enabled from cfg2: ~a"
                  (prism-project ssl-enabled-prism cfg2)))
(newline)

;; ============================================================================
;; ADVANCED: PRISM AS TRAVERSAL
;; ============================================================================

(displayln "═══ PRISM AS TRAVERSAL ═══")
(newline)

(displayln "Remember: All prisms ARE traversals")
(displayln "  • 0 foci when projection fails")
(displayln "  • 1 focus when projection succeeds")
(newline)

(displayln "Using string-number-prism as traversal:")
(displayln (format "  (traversal->list string-number-prism \\\"42\\\") => ~a"
                  (traversal->list string-number-prism "42")))
(displayln (format "  (traversal->list string-number-prism \\\"bad\\\") => ~a"
                  (traversal->list string-number-prism "bad")))
(newline)

(displayln "traversal-map with prism:")
(displayln (format "  (traversal-map string-number-prism \\\"5\\\" (* 2)) => ~a"
                  (traversal-map string-number-prism "5" (λ (x) (* x 2)))))
(displayln (format "  (traversal-map string-number-prism \\\"bad\\\" (* 2)) => ~a"
                  (traversal-map string-number-prism "bad" (λ (x) (* x 2)))))
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "PRISM CORE:")
(displayln "  • Partial/optional optic (0 or 1 focus)")
(displayln "  • prism-project: try to extract focus")
(displayln "  • prism-inject: convert focus to target")
(displayln "  • prism-compose: combine prisms")
(newline)

(displayln "BUILT-IN PRISMS:")
(displayln "  • string-number-prism: string ↔ number")
(displayln "  • guard-prism: predicate-based filtering")
(newline)

(displayln "LAWS:")
(displayln "  1. project(inject(focus)) = focus")
(displayln "  2. inject(project(target)) = target (when focus exists)")
(newline)

(displayln "CUSTOM PRISMS:")
(displayln "  • make-prism with project and inject functions")
(displayln "  • Handle failure with prism-absent")
(newline)

(displayln "USE CASES:")
(displayln "  • Optional fields")
(displayln "  • Type conversions that may fail")
(displayln "  • Subtype access")
(displayln "  • Validation pipelines")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 047: Prism Deep Dive")
(displayln "═══════════════════════════════════════════════════════════════")
