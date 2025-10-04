#lang racket

;; Polyglot module system inspired by Truffle's Polyglot API
;; Demonstrates cross-language interoperability patterns

(provide (all-defined-out))

;; ============================================================================
;; Language Context
;; ============================================================================

;; Represents a language implementation
(struct language-context
  (id name eval-proc exports)
  #:transparent
  #:mutable)

;; Global registry of language contexts
(define *language-registry* (make-hash))

;; Register a language
(define (register-language! id name eval-proc [exports (hash)])
  (hash-set! *language-registry* id
             (language-context id name eval-proc exports)))

;; Get language context
(define (get-language id)
  (hash-ref *language-registry* id
            (lambda () (error "Unknown language:" id))))

;; ============================================================================
;; Polyglot Value Protocol
;; ============================================================================

;; Wrapper for values from different languages
(struct polyglot-value
  (language-id native-value metadata)
  #:transparent)

;; Create polyglot value
(define (make-polyglot-value lang-id value [metadata (hash)])
  (polyglot-value lang-id value metadata))

;; Convert value between languages
(define (convert-value value target-lang)
  (match value
    [(polyglot-value source-lang val meta)
     (if (equal? source-lang target-lang)
         val
         (convert-between-languages val source-lang target-lang))]
    [_ value]))

;; Language-specific conversions
(define (convert-between-languages value from-lang to-lang)
  (cond
    ;; Racket → Python-like
    [(and (equal? from-lang 'racket) (equal? to-lang 'python))
     (cond
       [(list? value) (list->vector value)]
       [(hash? value) value]
       [else value])]

    ;; Python-like → Racket
    [(and (equal? from-lang 'python) (equal? to-lang 'racket))
     (cond
       [(vector? value) (vector->list value)]
       [(hash? value) value]
       [else value])]

    ;; JavaScript-like → Racket
    [(and (equal? from-lang 'javascript) (equal? to-lang 'racket))
     (cond
       [(eq? value 'undefined) (void)]
       [(eq? value 'null) #f]
       [else value])]

    ;; Default: pass through
    [else value]))

;; ============================================================================
;; Polyglot Execution Context
;; ============================================================================

;; Context for polyglot execution
(struct polyglot-context
  (bindings languages)
  #:transparent
  #:mutable)

;; Create new polyglot context
(define (make-polyglot-context)
  (polyglot-context (make-hash) (make-hash)))

;; Evaluate code in specific language
(define (eval-in-language context lang-id code)
  (define lang (get-language lang-id))
  (define result ((language-context-eval-proc lang) code context))
  (make-polyglot-value lang-id result))

;; Import value from language
(define (import-value context name lang-id)
  (define lang (get-language lang-id))
  (define exports (language-context-exports lang))
  (hash-ref exports name
            (lambda () (error (~a "No export '" name "' in " lang-id)))))

;; Export value to context
(define (export-value! context name value lang-id)
  (define lang (get-language lang-id))
  (hash-set! (language-context-exports lang) name value))

;; ============================================================================
;; Language Implementations
;; ============================================================================

;; Racket language context
(register-language!
 'racket
 "Racket"
 (lambda (code context)
   (eval code)))

;; Python-like language
(register-language!
 'python
 "Python-like"
 (lambda (code context)
   ;; Simple Python-like interpreter
   (match code
     [`(def ,name (lambda ,args ,body))
      (define func (eval `(lambda ,args ,body)))
      (export-value! context name func 'python)
      (void)]

     [`(print . ,args)
      (apply displayln args)
      (void)]

     [`(list . ,items)
      (list->vector items)]

     [_ (eval code)])))

;; JavaScript-like language
(register-language!
 'javascript
 "JavaScript-like"
 (lambda (code context)
   ;; Simple JavaScript-like interpreter
   (match code
     [`(function ,name ,args ,body)
      (define func (eval `(lambda ,args ,body)))
      (export-value! context name func 'javascript)
      (void)]

     [`(console.log . ,args)
      (apply displayln args)
      (void)]

     [`(Array . ,items)
      (list->vector items)]

     [_ (eval code)])))

;; ============================================================================
;; High-Level Polyglot API
;; ============================================================================

;; Execute polyglot code
(define-syntax-rule (with-polyglot-context body ...)
  (let ([ctx (make-polyglot-context)])
    (parameterize ([current-polyglot-context ctx])
      body ...)))

;; Current polyglot context parameter
(define current-polyglot-context (make-parameter #f))

;; Evaluate in language
(define-syntax (eval-lang stx)
  (syntax-parse stx
    [(_ lang:id expr)
     #'(eval-in-language (current-polyglot-context) 'lang 'expr)]))

;; Import from language
(define-syntax (import-from stx)
  (syntax-parse stx
    [(_ lang:id name:id)
     #'(import-value (current-polyglot-context) 'name 'lang)]))

;; ============================================================================
;; Examples
;; ============================================================================

(module+ main
  (displayln "=== Polyglot Module System ===\n")

  ;; Example 1: Cross-language function calls
  (displayln "Example 1: Calling between languages")

  (with-polyglot-context
    ;; Define function in Python-like
    (eval-lang python
               (def add (lambda (a b) (+ a b))))

    ;; Call from Racket
    (define py-add (import-from python add))
    (displayln (~a "Python add(10, 32) = " (py-add 10 32)))

    ;; Define in JavaScript-like
    (eval-lang javascript
               (function multiply (a b) (* a b)))

    ;; Call from Racket
    (define js-multiply (import-from javascript multiply))
    (displayln (~a "JavaScript multiply(6, 7) = " (js-multiply 6 7))))

  (newline)

  ;; Example 2: Data structure conversion
  (displayln "Example 2: Data structure conversion")

  (with-polyglot-context
    ;; Racket list
    (define rkt-list '(1 2 3 4 5))
    (displayln (~a "Racket list: " rkt-list))

    ;; Convert to Python-like (vector)
    (define py-list (convert-value
                     (make-polyglot-value 'racket rkt-list)
                     'python))
    (displayln (~a "As Python: " py-list))

    ;; Convert back
    (define back-to-rkt (convert-value
                         (make-polyglot-value 'python py-list)
                         'racket))
    (displayln (~a "Back to Racket: " back-to-rkt)))

  (newline)

  ;; Example 3: Shared state
  (displayln "Example 3: Shared state across languages")

  (with-polyglot-context
    ;; Python creates data
    (eval-lang python
               (def get-user (lambda ()
                              (hash 'name "Alice"
                                   'age 30
                                   'email "alice@example.com"))))

    ;; JavaScript processes it
    (eval-lang javascript
               (function format-user (user)
                        (string-append (hash-ref user 'name)
                                     " <"
                                     (hash-ref user 'email)
                                     ">")))

    ;; Racket uses both
    (define get-user (import-from python get-user))
    (define format-user (import-from javascript format-user))

    (define user (get-user))
    (displayln (~a "User: " (format-user user))))

  (newline)

  (displayln "=== Polyglot Complete ==="))

;; ============================================================================
;; Advanced: Language-Specific Type Coercion
;; ============================================================================

;; Type coercion rules
(define type-coercion-rules
  (hash
   ;; Python → Racket
   (cons 'python 'racket)
   (hash 'vector? vector->list
         'undefined (lambda (x) (void)))

   ;; JavaScript → Racket
   (cons 'javascript 'racket)
   (hash 'null (lambda (x) #f)
         'undefined (lambda (x) (void))
         'object hash)

   ;; Racket → Python
   (cons 'racket 'python)
   (hash 'list? list->vector
         'void? (lambda (x) 'None))))

;; Apply coercion
(define (coerce-value value from-lang to-lang)
  (define rules (hash-ref type-coercion-rules
                         (cons from-lang to-lang)
                         (hash)))

  (for/or ([(pred coerce) (in-hash rules)])
    (and (if (procedure? pred)
             (pred value)
             (equal? (type-of value) pred))
         (coerce value)))

  value)

;; ============================================================================
;; FFI-Style Bindings
;; ============================================================================

;; Declare foreign function
(define-syntax (define-foreign stx)
  (syntax-parse stx
    [(_ name:id lang:id foreign-name:id)
     #'(define name
         (lambda args
           (define func (import-value (current-polyglot-context)
                                     'foreign-name
                                     'lang))
           (apply func args)))]))

;; Example usage
(module+ foreign-example
  (with-polyglot-context
    ;; Define in Python
    (eval-lang python
               (def greet (lambda (name)
                           (string-append "Hello, " name "!"))))

    ;; Use in Racket
    (define-foreign racket-greet python greet)
    (displayln (racket-greet "World"))))

;; ============================================================================
;; Tests
;; ============================================================================

(module+ test
  (require rackunit)

  (test-case "Language registration"
    (check-true (hash-has-key? *language-registry* 'racket))
    (check-true (hash-has-key? *language-registry* 'python))
    (check-true (hash-has-key? *language-registry* 'javascript)))

  (test-case "Value conversion"
    (define rkt-val (make-polyglot-value 'racket '(1 2 3)))
    (define py-val (convert-value rkt-val 'python))
    (check-true (vector? py-val))
    (check-equal? py-val #(1 2 3)))

  (test-case "Cross-language execution"
    (with-polyglot-context
      (eval-lang python (def test-add (lambda (a b) (+ a b))))
      (define add-fn (import-from python test-add))
      (check-equal? (add-fn 5 7) 12))))
