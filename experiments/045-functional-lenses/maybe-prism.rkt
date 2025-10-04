#lang racket

;; Maybe Prism: Handling Optional Values
;; Demonstrates prisms for sum types (Maybe/Option)

(provide (all-defined-out))

;; ============================================================================
;; MAYBE TYPE (SUM TYPE)
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║              MAYBE PRISM: Optional Values                    ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "Maybe/Option type: represents optional values")
(displayln "  - Some(value): contains a value")
(displayln "  - None:        no value present")
(newline)

;; Define Maybe type as a sum type
(struct some (value) #:transparent)
(struct none () #:transparent)

(define NONE (none))

(displayln "Type definition:")
(displayln "  (struct some (value) #:transparent)")
(displayln "  (struct none () #:transparent)")
(displayln "  (define NONE (none))")
(newline)

;; ============================================================================
;; PRISM INFRASTRUCTURE
;; ============================================================================

(struct prism (match? getter setter) #:transparent)

;; Special value for failed projection
(define prism-absent 'prism-absent)

(define (prism-project p target [default prism-absent])
  "Try to extract focus from target."
  (if ((prism-match? p) target)
      ((prism-getter p) target)
      default))

(define (prism-inject p focus)
  "Convert focus back to target."
  ((prism-setter p) target focus))

;; ============================================================================
;; MAYBE PRISM
;; ============================================================================

(displayln "═══ MAYBE PRISM ═══")
(newline)

(displayln "A prism for the 'some' case of a Maybe type")
(newline)

(define maybe-prism
  (prism
    ; match?: does it have a value?
    (λ (m) (some? m))
    ; getter: extract value from Some
    (λ (m) (some-value m))
    ; setter: wrap value in Some
    (λ (m new-value) (some new-value))))

(displayln "Definition:")
(displayln "  (define maybe-prism")
(displayln "    (prism")
(displayln "      (λ (m) (some? m))           ; match?")
(displayln "      (λ (m) (some-value m))      ; getter")
(displayln "      (λ (m v) (some v))))        ; setter")
(newline)

;; ============================================================================
;; EXAMPLES
;; ============================================================================

(displayln "═══ EXAMPLES ═══")
(newline)

(define maybe-int (some 42))
(define maybe-none NONE)

(displayln "Values:")
(displayln (format "  maybe-int:  ~a" maybe-int))
(displayln (format "  maybe-none: ~a" maybe-none))
(newline)

(displayln "prism-project (extract value):")
(displayln (format "  (prism-project maybe-prism maybe-int) => ~a"
                  (prism-project maybe-prism maybe-int)))
(displayln (format "  (prism-project maybe-prism maybe-none) => ~a"
                  (prism-project maybe-prism maybe-none)))
(displayln (format "  (prism-project maybe-prism maybe-none #f) => ~a"
                  (prism-project maybe-prism maybe-none #f)))
(newline)

(displayln "Pattern matching on Maybe:")
(define (describe-maybe m)
  (match m
    [(some v) (format "Some(~a)" v)]
    [(none) "None"]))

(displayln (format "  ~a => ~a" maybe-int (describe-maybe maybe-int)))
(displayln (format "  ~a => ~a" maybe-none (describe-maybe maybe-none)))
(newline)

;; ============================================================================
;; MAYBE OPERATIONS
;; ============================================================================

(displayln "═══ MAYBE OPERATIONS ═══")
(newline)

(define (maybe-map m fn)
  "Map function over Maybe value."
  (match m
    [(some v) (some (fn v))]
    [(none) NONE]))

(define (maybe-flat-map m fn)
  "Flat-map (bind/monadic bind) for Maybe."
  (match m
    [(some v) (fn v)]
    [(none) NONE]))

(define (maybe-or-else m default)
  "Extract value or return default."
  (match m
    [(some v) v]
    [(none) default]))

(displayln "Helper functions:")
(displayln "  maybe-map: (Maybe a) -> (a -> b) -> (Maybe b)")
(displayln "  maybe-flat-map: (Maybe a) -> (a -> Maybe b) -> (Maybe b)")
(displayln "  maybe-or-else: (Maybe a) -> a -> a")
(newline)

(define m1 (some 10))

(displayln "Examples:")
(displayln (format "  (maybe-map (some 10) (* 2)) => ~a"
                  (maybe-map m1 (λ (x) (* x 2)))))
(displayln (format "  (maybe-map (none) (* 2)) => ~a"
                  (maybe-map NONE (λ (x) (* x 2)))))
(displayln (format "  (maybe-or-else (some 10) 0) => ~a"
                  (maybe-or-else m1 0)))
(displayln (format "  (maybe-or-else (none) 0) => ~a"
                  (maybe-or-else NONE 0)))
(newline)

;; ============================================================================
;; PRISM WITH MATCH
;; ============================================================================

(displayln "═══ PRISM WITH MATCH ═══")
(newline)

(displayln "Alternative prism using match:")
(newline)

(define maybe-prism-alt
  (prism
    some?
    some-value
    (λ (_ v) (some v))))

(displayln "Using prism in pipeline:")

(define (safe-div a b)
  "Division that returns Maybe."
  (if (zero? b)
      NONE
      (some (/ a b))))

(displayln (format "  (safe-div 10 2) => ~a" (safe-div 10 2)))
(displayln (format "  (safe-div 10 0) => ~a" (safe-div 10 0)))
(newline)

;; Chain operations using Maybe
(define (compute x y)
  (maybe-flat-map (safe-div x y)
                 (λ (result)
                   (maybe-map (some result)
                             (λ (r) (* r 10))))))

(displayln "Chained computation (safe-div then multiply by 10):")
(displayln (format "  (compute 20 4) => ~a" (compute 20 4)))
(displayln (format "  (compute 20 0) => ~a" (compute 20 0)))
(newline)

;; ============================================================================
;; NESTED MAYBE
;; ============================================================================

(displayln "═══ NESTED MAYBE ═══")
(newline)

(struct person (name phone) #:transparent)
(struct contact (email mobile) #:transparent)

;; Person may have contact info (Maybe contact)
(define alice (person "Alice" (some (contact "alice@example.com" "555-1234"))))
(define bob (person "Bob" NONE))

(displayln "Nested Maybe example:")
(displayln (format "  Alice: ~a" alice))
(displayln (format "  Bob:   ~a" bob))
(newline)

;; Prism to access phone -> contact
(define phone-prism
  (prism
    some?
    some-value
    (λ (_ v) (some v))))

;; Extract mobile number if it exists
(define (get-mobile p)
  (match (person-phone p)
    [(some contact-info) (some (contact-mobile contact-info))]
    [(none) NONE]))

(displayln "Extract mobile numbers:")
(displayln (format "  Alice's mobile: ~a" (get-mobile alice)))
(displayln (format "  Bob's mobile:   ~a" (get-mobile bob)))
(newline)

;; ============================================================================
;; PRISM LAWS FOR MAYBE
;; ============================================================================

(displayln "═══ PRISM LAWS FOR MAYBE ═══")
(newline)

(displayln "Law 1: Inject-Project")
(displayln "  prism-project(prism-inject(prism, focus)) = focus")

(define test-value 99)
(define injected (some test-value))
(define projected (prism-project maybe-prism injected))

(displayln (format "  Test value: ~a" test-value))
(displayln (format "  Inject: ~a" injected))
(displayln (format "  Project: ~a" projected))
(displayln (format "  Equal? ~a ✓" (equal? test-value projected)))
(newline)

(displayln "Law 2: Project-Inject (when focus exists)")
(displayln "  prism-inject(prism, prism-project(prism, target)) = target")

(define test-maybe (some 42))
(define value (prism-project maybe-prism test-maybe))
(define back-to-maybe (some value))

(displayln (format "  Test Maybe: ~a" test-maybe))
(displayln (format "  Project: ~a" value))
(displayln (format "  Inject: ~a" back-to-maybe))
(displayln (format "  Equal? ~a ✓" (equal? test-maybe back-to-maybe)))
(newline)

;; ============================================================================
;; PRACTICAL EXAMPLE: OPTION CHAINING
;; ============================================================================

(displayln "═══ PRACTICAL EXAMPLE: Option Chaining ═══")
(newline)

(struct config (database cache) #:transparent)
(struct db-config (host port) #:transparent)

;; Config may have database settings
(define cfg1 (config (some (db-config "localhost" 5432)) NONE))
(define cfg2 (config NONE NONE))

(displayln "Configurations:")
(displayln (format "  cfg1: ~a" cfg1))
(displayln (format "  cfg2: ~a" cfg2))
(newline)

(define (get-db-port cfg)
  "Extract database port if configured."
  (maybe-flat-map (config-database cfg)
                 (λ (db) (some (db-config-port db)))))

(displayln "Get database port:")
(displayln (format "  cfg1: ~a" (get-db-port cfg1)))
(displayln (format "  cfg2: ~a" (get-db-port cfg2)))
(newline)

(displayln "With default value:")
(displayln (format "  cfg1: ~a" (maybe-or-else (get-db-port cfg1) 3000)))
(displayln (format "  cfg2: ~a" (maybe-or-else (get-db-port cfg2) 3000)))
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "MAYBE TYPE:")
(displayln "  • Sum type: Some(value) | None")
(displayln "  • Represents optional values")
(displayln "  • Type-safe null handling")
(newline)

(displayln "MAYBE PRISM:")
(displayln "  • Focuses on Some case")
(displayln "  • 0 or 1 focus (partial optic)")
(displayln "  • match?: some?")
(displayln "  • getter: some-value")
(displayln "  • setter: (λ (_ v) (some v))")
(newline)

(displayln "OPERATIONS:")
(displayln "  • maybe-map: transform value if present")
(displayln "  • maybe-flat-map: chain Maybe operations")
(displayln "  • maybe-or-else: provide default")
(newline)

(displayln "USE CASES:")
(displayln "  • Optional fields in data structures")
(displayln "  • Safe computations (division, lookup)")
(displayln "  • Configuration with defaults")
(displayln "  • Chaining operations that may fail")
(newline)

(displayln "KEY INSIGHT:")
(displayln "  Prisms are perfect for sum types like Maybe")
(displayln "  They handle the 'partial' nature elegantly")
(displayln "  Match on constructor, extract/inject values")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 045: Maybe Prism")
(displayln "═══════════════════════════════════════════════════════════════")
