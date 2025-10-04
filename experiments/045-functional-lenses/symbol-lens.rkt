#lang racket

;; Symbol Lens Example: Demonstrating lenses work on any data type
;; Including how to update symbols (which are immutable like strings)

(provide symbol-upcase
         symbol-downcase
         symbol-lens
         symbol->string-lens)

;; ============================================================================
;; Symbol Operations
;; ============================================================================

(define (symbol-upcase sym)
  "Convert symbol to uppercase.
   'hello -> 'HELLO"
  (string->symbol (string-upcase (symbol->string sym))))

(define (symbol-downcase sym)
  "Convert symbol to lowercase.
   'HELLO -> 'hello"
  (string->symbol (string-downcase (symbol->string sym))))

;; ============================================================================
;; Demonstration
;; ============================================================================

(module+ main
  (displayln "╔══════════════════════════════════════════════════════════╗")
  (displayln "║              SYMBOL OPERATIONS                           ║")
  (displayln "╚══════════════════════════════════════════════════════════╝")
  (newline)

  ;; Basic symbol transformations
  (displayln "Basic Symbol Transformations:")
  (displayln "─────────────────────────────")
  (newline)

  (define sym 'hello)
  (displayln (format "Original symbol: ~a" sym))
  (displayln (format "  (symbol-upcase 'hello) => ~a" (symbol-upcase sym)))
  (displayln (format "  (symbol-downcase 'HELLO) => ~a" (symbol-downcase 'HELLO)))
  (newline)

  (define mixed 'MixedCase)
  (displayln (format "Mixed case: ~a" mixed))
  (displayln (format "  (symbol-upcase 'MixedCase) => ~a" (symbol-upcase mixed)))
  (displayln (format "  (symbol-downcase 'MixedCase) => ~a" (symbol-downcase mixed)))
  (newline)

  ;; Symbols in data structures
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "  SYMBOLS IN DATA STRUCTURES")
  (displayln "═══════════════════════════════════════════════════════════")
  (newline)

  ;; Hash with symbol values
  (define config
    (hash 'status 'active
          'level 'debug
          'mode 'production))

  (displayln "Config hash:")
  (displayln config)
  (newline)

  ;; Update symbol value
  (displayln "Upcase 'status value:")
  (define updated-config
    (hash-update config 'status symbol-upcase))
  (displayln updated-config)
  (newline)

  ;; List of symbols
  (define states '(pending active completed failed))
  (displayln (format "States: ~a" states))
  (displayln (format "Uppercase: ~a" (map symbol-upcase states)))
  (newline)

  ;; ========================================================================
  ;; Using Lenses with Symbols
  ;; ========================================================================

  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "  LENSES WITH SYMBOLS")
  (displayln "═══════════════════════════════════════════════════════════")
  (newline)

  (struct lens (getter setter) #:transparent)

  (define (view l target)
    ((lens-getter l) target))

  (define (set l target value)
    ((lens-setter l) target value))

  (define (over l target fn)
    (set l target (fn (view l target))))

  ;; Lens for hash field
  (define (hash-lens key)
    (lens
      (λ (h) (hash-ref h key))
      (λ (h v) (hash-set h key v))))

  (define status-lens (hash-lens 'status))

  (displayln "Using status-lens:")
  (displayln (format "  Original: ~a" config))
  (displayln (format "  View: ~a" (view status-lens config)))
  (displayln (format "  Over (upcase): ~a"
                    (over status-lens config symbol-upcase)))
  (newline)

  ;; Lens for car (first element)
  (define car-lens
    (lens
      car
      (λ (lst v) (cons v (cdr lst)))))

  (displayln "Using car-lens on symbol list:")
  (displayln (format "  Original: ~a" states))
  (displayln (format "  View: ~a" (view car-lens states)))
  (displayln (format "  Over (upcase): ~a"
                    (over car-lens states symbol-upcase)))
  (newline)

  ;; ========================================================================
  ;; Symbol-String Isomorphism
  ;; ========================================================================

  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "  SYMBOL ↔ STRING ISOMORPHISM")
  (displayln "═══════════════════════════════════════════════════════════")
  (newline)

  (displayln "Symbols and strings have a natural isomorphism:")
  (displayln "  symbol->string: Symbol -> String")
  (displayln "  string->symbol: String -> Symbol")
  (newline)

  (define sym-example 'hello-world)
  (define str-example (symbol->string sym-example))
  (define sym-back (string->symbol str-example))

  (displayln (format "  ~a --(symbol->string)--> ~s" sym-example str-example))
  (displayln (format "  ~s --(string->symbol)--> ~a" str-example sym-back))
  (displayln (format "  Round-trip works? ~a" (equal? sym-example sym-back)))
  (newline)

  ;; Lens that views symbol as string
  (define symbol->string-lens
    (lens
      symbol->string
      (λ (sym new-str) (string->symbol new-str))))

  (displayln "Using symbol->string-lens:")
  (define test-sym 'hello)
  (displayln (format "  Original symbol: ~a" test-sym))
  (displayln (format "  View as string: ~s" (view symbol->string-lens test-sym)))
  (displayln (format "  Over (upcase string): ~a"
                    (over symbol->string-lens test-sym string-upcase)))
  (newline)

  ;; ========================================================================
  ;; Practical Example: Config Management
  ;; ========================================================================

  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "  PRACTICAL EXAMPLE: CONFIG MANAGEMENT")
  (displayln "═══════════════════════════════════════════════════════════")
  (newline)

  (struct app-config (env log-level features) #:transparent)

  (define config-example
    (app-config 'production
                'info
                '(caching analytics monitoring)))

  (displayln "Application config:")
  (displayln config-example)
  (newline)

  ;; Lenses for config fields
  (define env-lens
    (lens
      app-config-env
      (λ (c v) (struct-copy app-config c [env v]))))

  (define log-level-lens
    (lens
      app-config-log-level
      (λ (c v) (struct-copy app-config c [log-level v]))))

  (displayln "Change environment to development:")
  (displayln (over env-lens config-example
                  (λ (_) 'development)))
  (newline)

  (displayln "Increase log level to debug:")
  (displayln (over log-level-lens config-example
                  (λ (_) 'debug)))
  (newline)

  ;; ========================================================================
  ;; Symbol Comparison: Efficient!
  ;; ========================================================================

  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "  WHY USE SYMBOLS?")
  (displayln "═══════════════════════════════════════════════════════════")
  (newline)

  (displayln "Symbols are INTERNED:")
  (displayln "  - Same symbol = same memory location")
  (displayln "  - eq? comparison is pointer comparison (very fast)")
  (displayln "  - string=? must compare every character")
  (newline)

  (define sym1 'status)
  (define sym2 'status)
  (define sym3 (string->symbol "status"))

  (displayln "Symbol identity:")
  (displayln (format "  sym1: ~a" sym1))
  (displayln (format "  sym2: ~a" sym2))
  (displayln (format "  sym3: ~a" sym3))
  (displayln (format "  (eq? sym1 sym2): ~a" (eq? sym1 sym2)))
  (displayln (format "  (eq? sym1 sym3): ~a" (eq? sym1 sym3)))
  (displayln "  All three are the SAME object in memory!")
  (newline)

  (displayln "Use symbols for:")
  (displayln "  ✓ Enums/constants (status, level, mode)")
  (displayln "  ✓ Hash keys (fast lookup)")
  (displayln "  ✓ Pattern matching")
  (displayln "  ✓ DSL keywords")
  (newline)

  ;; ========================================================================
  ;; Symbol List Manipulation
  ;; ========================================================================

  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "  SYMBOL LIST MANIPULATION")
  (displayln "═══════════════════════════════════════════════════════════")
  (newline)

  (define commands '(start stop restart status))

  (displayln "Commands list:")
  (displayln (format "  ~a" commands))
  (newline)

  (displayln "Transform all to uppercase:")
  (displayln (format "  ~a" (map symbol-upcase commands)))
  (newline)

  (displayln "Filter commands starting with 's':")
  (define s-commands
    (filter (λ (cmd)
              (string-prefix? (symbol->string cmd) "s"))
            commands))
  (displayln (format "  ~a" s-commands))
  (newline)

  ;; ========================================================================
  ;; Symbol in Lisp/Scheme Tradition
  ;; ========================================================================

  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "  SYMBOLS IN THE LISP/SCHEME TRADITION")
  (displayln "═══════════════════════════════════════════════════════════")
  (newline)

  (displayln "In Lisp/Scheme/Racket:")
  (displayln "  - Symbols represent identifiers")
  (displayln "  - Code is data (homoiconicity)")
  (displayln "  - '(+ 1 2) is a list of symbols")
  (displayln "  - eval can turn data into code")
  (newline)

  (define code-as-data '(+ 1 2))
  (displayln (format "Code as data: ~a" code-as-data))
  (displayln (format "  First: ~a (symbol)" (car code-as-data)))
  (displayln (format "  Second: ~a (number)" (cadr code-as-data)))
  (displayln (format "  Third: ~a (number)" (caddr code-as-data)))
  (newline)

  (displayln "Upcase the operator:")
  (define uppercased-code
    (cons (symbol-upcase (car code-as-data))
          (cdr code-as-data)))
  (displayln (format "  ~a" uppercased-code))
  (newline))

;; ============================================================================
;; Tests
;; ============================================================================

(module+ test
  (require rackunit)

  ;; Test symbol-upcase
  (check-equal? (symbol-upcase 'hello) 'HELLO
                "symbol-upcase works")

  (check-equal? (symbol-upcase 'mixed-Case) 'MIXED-CASE
                "symbol-upcase handles mixed case")

  ;; Test symbol-downcase
  (check-equal? (symbol-downcase 'HELLO) 'hello
                "symbol-downcase works")

  ;; Test round-trip
  (check-equal? (symbol-downcase (symbol-upcase 'test)) 'test
                "round-trip preserves lowercase")

  (check-equal? (symbol-upcase (symbol-downcase 'TEST)) 'TEST
                "round-trip preserves uppercase")

  ;; Test in data structures
  (check-equal? (map symbol-upcase '(a b c)) '(A B C)
                "map symbol-upcase works")

  (check-equal?
    (hash-update (hash 'key 'value) 'key symbol-upcase)
    (hash 'key 'VALUE)
    "hash-update with symbol-upcase works")

  (displayln "All symbol tests passed! ✓"))
