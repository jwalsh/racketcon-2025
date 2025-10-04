#lang racket

;; Ocular Patdown: Lens Examples
;; Based on official documentation

(require ocular-patdown)

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         OCULAR PATDOWN: LENS EXAMPLES                        ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

;; ============================================================================
;; BASIC LENS OPERATIONS
;; ============================================================================

(displayln "═══ BASIC LENS OPERATIONS ═══")
(newline)

(struct posn (x y) #:transparent)

;; Create lenses for struct fields
(define x-lens (struct-lens posn x))
(define y-lens (struct-lens posn y))

(define p (posn 3 4))

(displayln "Struct: (posn 3 4)")
(displayln "")

(displayln "lens-get (extract value):")
(displayln (format "  (lens-get x-lens p) => ~a" (lens-get x-lens p)))
(displayln (format "  (lens-get y-lens p) => ~a" (lens-get y-lens p)))
(newline)

(displayln "lens-set (update value):")
(displayln (format "  (lens-set x-lens p 9) => ~a" (lens-set x-lens p 9)))
(displayln (format "  (lens-set y-lens p 10) => ~a" (lens-set y-lens p 10)))
(newline)

(displayln "lens-modify (transform value):")
(displayln (format "  (lens-modify x-lens p (* 2)) => ~a"
                  (lens-modify x-lens p (λ (x) (* x 2)))))
(displayln (format "  (lens-modify y-lens p add1) => ~a"
                  (lens-modify y-lens p add1)))
(newline)

;; ============================================================================
;; BUILT-IN LENSES
;; ============================================================================

(displayln "═══ BUILT-IN LENSES ═══")
(newline)

(displayln "identity-lens (focuses on entire target):")
(displayln (format "  (lens-get identity-lens 42) => ~a"
                  (lens-get identity-lens 42)))
(displayln (format "  (lens-set identity-lens 42 99) => ~a"
                  (lens-set identity-lens 42 99)))
(newline)

(displayln "car-lens and cdr-lens (for pairs):")
(define pair '(10 . 20))
(displayln (format "  Pair: ~a" pair))
(displayln (format "  (lens-get car-lens pair) => ~a"
                  (lens-get car-lens pair)))
(displayln (format "  (lens-get cdr-lens pair) => ~a"
                  (lens-get cdr-lens pair)))
(displayln (format "  (lens-set car-lens pair 99) => ~a"
                  (lens-set car-lens pair 99)))
(newline)

;; ============================================================================
;; LENS COMPOSITION
;; ============================================================================

(displayln "═══ LENS COMPOSITION ═══")
(newline)

(struct rect (top-left width height) #:transparent)

(define r (rect (posn 10 20) 50 30))

(define top-left-lens (struct-lens rect top-left))
(define width-lens (struct-lens rect width))

(displayln "Nested struct: (rect (posn 10 20) 50 30)")
(displayln "")

;; Compose lenses to access nested x coordinate
(define rect-x-lens (lens-compose top-left-lens x-lens))

(displayln "Composed lens (rect -> posn -> x):")
(displayln (format "  (lens-get rect-x-lens r) => ~a"
                  (lens-get rect-x-lens r)))
(displayln (format "  (lens-set rect-x-lens r 99) => ~a"
                  (lens-set rect-x-lens r 99)))
(displayln (format "  (lens-modify rect-x-lens r (+ 5)) => ~a"
                  (lens-modify rect-x-lens r (λ (x) (+ x 5)))))
(newline)

;; ============================================================================
;; MULTIPLE COMPOSITION
;; ============================================================================

(displayln "═══ MULTIPLE COMPOSITION ═══")
(newline)

(struct company (name ceo) #:transparent)
(struct person (name address) #:transparent)
(struct address (street city state) #:transparent)

(define tech-corp
  (company "TechCorp"
           (person "Alice"
                   (address "123 Main" "Boston" "MA"))))

(displayln "Complex structure:")
(displayln (format "  ~a" tech-corp))
(newline)

;; Build path: company -> person -> address -> city
(define ceo-lens (struct-lens company ceo))
(define address-lens (struct-lens person address))
(define city-lens (struct-lens address city))

(define company-city-lens
  (lens-compose (lens-compose ceo-lens address-lens) city-lens))

(displayln "Deep lens (company -> ceo -> address -> city):")
(displayln (format "  (lens-get company-city-lens tech-corp) => ~a"
                  (lens-get company-city-lens tech-corp)))
(displayln (format "  (lens-set company-city-lens tech-corp \\\"Cambridge\\\") => ~a"
                  (lens-set company-city-lens tech-corp "Cambridge")))
(newline)

;; ============================================================================
;; LENS LAWS
;; ============================================================================

(displayln "═══ LENS LAWS (Verification) ═══")
(newline)

(displayln "Law 1: GET-PUT")
(displayln "  (lens-set l t (lens-get l t)) = t")
(define law1-result (lens-set x-lens p (lens-get x-lens p)))
(displayln (format "  Result: ~a" law1-result))
(displayln (format "  Equal to original? ~a" (equal? law1-result p)))
(newline)

(displayln "Law 2: PUT-GET")
(displayln "  (lens-get l (lens-set l t v)) = v")
(define p-updated (lens-set x-lens p 99))
(define law2-result (lens-get x-lens p-updated))
(displayln (format "  Result: ~a" law2-result))
(displayln (format "  Equal to set value (99)? ~a" (equal? law2-result 99)))
(newline)

(displayln "Law 3: PUT-PUT")
(displayln "  (lens-set l (lens-set l t v1) v2) = (lens-set l t v2)")
(define law3-left (lens-set x-lens (lens-set x-lens p 10) 20))
(define law3-right (lens-set x-lens p 20))
(displayln (format "  Left: ~a" law3-left))
(displayln (format "  Right: ~a" law3-right))
(displayln (format "  Equal? ~a" (equal? law3-left law3-right)))
(newline)

;; ============================================================================
;; PRACTICAL EXAMPLES
;; ============================================================================

(displayln "═══ PRACTICAL EXAMPLES ═══")
(newline)

(displayln "Example 1: Config management")
(struct config (debug? port log-level) #:transparent)

(define cfg (config #t 8080 'info))
(define port-lens (struct-lens config port))

(displayln (format "  Original config: ~a" cfg))
(displayln (format "  Change port to 3000: ~a"
                  (lens-set port-lens cfg 3000)))
(newline)

(displayln "Example 2: Batch updates")
(define cfg2 (lens-modify port-lens cfg (λ (p) (+ p 1000))))
(displayln (format "  Increment port by 1000: ~a" cfg2))
(newline)

(displayln "Example 3: Chain updates")
(define debug-lens (struct-lens config debug?))
(define log-lens (struct-lens config log-level))

(define cfg3
  (lens-set log-lens
            (lens-set debug-lens cfg #f)
            'error))

(displayln (format "  Disable debug and set log-level to error: ~a" cfg3))
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "LENS OPERATIONS:")
(displayln "  • lens-get: extract focus")
(displayln "  • lens-set: update focus")
(displayln "  • lens-modify: transform focus")
(displayln "  • lens-compose: combine lenses")
(newline)

(displayln "BUILT-IN LENSES:")
(displayln "  • identity-lens: entire target")
(displayln "  • car-lens, cdr-lens: pair access")
(displayln "  • struct-lens: struct field access")
(newline)

(displayln "KEY FEATURES:")
(displayln "  • Immutable updates")
(displayln "  • Composable paths")
(displayln "  • Type-safe focusing")
(displayln "  • Follows lens laws")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 047: Ocular Patdown Lenses")
(displayln "═══════════════════════════════════════════════════════════════")
