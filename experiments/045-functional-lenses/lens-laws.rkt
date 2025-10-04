#lang racket

;; Lens Laws: Formal verification and examples
;; Demonstrates the three laws that well-behaved lenses must satisfy

(require rackunit)

(provide lens
         view
         set
         over
         car-lens
         cdr-lens
         verify-lens-laws)

;; ============================================================================
;; Core Lens Structure
;; ============================================================================

(struct lens (getter setter) #:transparent)

(define (view l target)
  "Extract focus from target."
  ((lens-getter l) target))

(define (set l target value)
  "Replace focus in target."
  ((lens-setter l) target value))

(define (over l target fn)
  "Modify focus in target."
  (set l target (fn (view l target))))

;; ============================================================================
;; THE THREE LENS LAWS
;; ============================================================================

(displayln "╔══════════════════════════════════════════════════════════╗")
(displayln "║             THE THREE LENS LAWS                          ║")
(displayln "╚══════════════════════════════════════════════════════════╝")
(newline)

(displayln "LAW 1: GetPut (You get what you put)")
(displayln "  If you SET what you just GOT, nothing changes")
(displayln "")
(displayln "  (set lens target (view lens target)) ≡ target")
(displayln "")
(displayln "  In other words: putting back what you got is identity")
(newline)

(displayln "LAW 2: PutGet (You put what you get)")
(displayln "  If you GET after you SET, you get what you SET")
(displayln "")
(displayln "  (view lens (set lens target value)) ≡ value")
(displayln "")
(displayln "  In other words: getting after setting returns the set value")
(newline)

(displayln "LAW 3: PutPut (Setting twice is same as setting once)")
(displayln "  If you SET twice, only the last SET matters")
(displayln "")
(displayln "  (set lens (set lens target v1) v2) ≡ (set lens target v2)")
(displayln "")
(displayln "  In other words: second set overwrites first")
(newline)

;; ============================================================================
;; Lens Law Verification Functions
;; ============================================================================

(define (verify-get-put lens target)
  "Law 1: Setting what you got does nothing.

   Mathematical: set l s (view l s) = s

   Intuition: If you read a value and write it back unchanged,
              the structure should be unchanged."
  (equal? (set lens target (view lens target))
          target))

(define (verify-put-get lens target value)
  "Law 2: Getting what you set returns that value.

   Mathematical: view l (set l s v) = v

   Intuition: If you write a value and immediately read it back,
              you should get exactly what you wrote."
  (equal? (view lens (set lens target value))
          value))

(define (verify-put-put lens target value1 value2)
  "Law 3: Setting twice is same as setting once (with second value).

   Mathematical: set l (set l s v1) v2 = set l s v2

   Intuition: Writing twice in a row is the same as just writing
              the final value once."
  (equal? (set lens (set lens target value1) value2)
          (set lens target value2)))

(define (verify-lens-laws lens target value1 value2)
  "Verify all three lens laws hold."
  (and (verify-get-put lens target)
       (verify-put-get lens target value1)
       (verify-put-put lens target value1 value2)))

;; ============================================================================
;; List Lenses
;; ============================================================================

(define car-lens
  (lens
    ; Getter: first element
    car
    ; Setter: replace first, keep rest
    (λ (lst new-first)
      (cons new-first (cdr lst)))))

(define cdr-lens
  (lens
    ; Getter: rest of list
    cdr
    ; Setter: keep first, replace rest
    (λ (lst new-rest)
      (cons (car lst) new-rest))))

;; ============================================================================
;; Demonstrations
;; ============================================================================

(module+ main
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "  DEMONSTRATION: car-lens on '(1 2 3)")
  (displayln "═══════════════════════════════════════════════════════════")
  (newline)

  (define test-list '(1 2 3))
  (displayln (format "Original list: ~a" test-list))
  (newline)

  ;; LAW 1: GetPut
  (displayln "LAW 1: GetPut - Set what you got")
  (displayln "─────────────────────────────────────")
  (define got-value (view car-lens test-list))
  (displayln (format "  view car-lens '(1 2 3) = ~a" got-value))
  (displayln "")
  (define put-back (set car-lens test-list got-value))
  (displayln (format "  set car-lens '(1 2 3) ~a = ~a" got-value put-back))
  (displayln "")
  (displayln (format "  Original:  ~a" test-list))
  (displayln (format "  After:     ~a" put-back))
  (displayln (format "  Equal?     ~a  ✓" (equal? test-list put-back)))
  (newline)

  ;; LAW 2: PutGet
  (displayln "LAW 2: PutGet - Get what you put")
  (displayln "─────────────────────────────────────")
  (define new-value 10)
  (define after-set (set car-lens test-list new-value))
  (displayln (format "  set car-lens '(1 2 3) ~a = ~a" new-value after-set))
  (displayln "")
  (define got-after-set (view car-lens after-set))
  (displayln (format "  view car-lens ~a = ~a" after-set got-after-set))
  (displayln "")
  (displayln (format "  Set value: ~a" new-value))
  (displayln (format "  Got value: ~a" got-after-set))
  (displayln (format "  Equal?     ~a  ✓" (equal? new-value got-after-set)))
  (newline)

  ;; LAW 3: PutPut
  (displayln "LAW 3: PutPut - Last set wins")
  (displayln "─────────────────────────────────────")
  (define set-first (set car-lens test-list 10))
  (displayln (format "  set car-lens '(1 2 3) 10 = ~a" set-first))
  (displayln "")
  (define set-second (set car-lens set-first 20))
  (displayln (format "  set car-lens ~a 20 = ~a" set-first set-second))
  (displayln "")
  (define set-once (set car-lens test-list 20))
  (displayln (format "  set car-lens '(1 2 3) 20 = ~a" set-once))
  (displayln "")
  (displayln (format "  Set twice: ~a" set-second))
  (displayln (format "  Set once:  ~a" set-once))
  (displayln (format "  Equal?     ~a  ✓" (equal? set-second set-once)))
  (newline)

  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "  ALL LAWS VERIFIED FOR car-lens")
  (displayln "═══════════════════════════════════════════════════════════")
  (newline)

  ;; cdr-lens demonstration
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "  DEMONSTRATION: cdr-lens on '(1 2 3)")
  (displayln "═══════════════════════════════════════════════════════════")
  (newline)

  (displayln "LAW 1: GetPut")
  (define cdr-got (view cdr-lens test-list))
  (displayln (format "  view cdr-lens '(1 2 3) = ~a" cdr-got))
  (define cdr-put-back (set cdr-lens test-list cdr-got))
  (displayln (format "  set car-lens '(1 2 3) '(2 3) = ~a" cdr-put-back))
  (displayln (format "  Equal? ~a  ✓" (equal? test-list cdr-put-back)))
  (newline)

  (displayln "LAW 2: PutGet")
  (define new-cdr '(20 30))
  (define cdr-after-set (set cdr-lens test-list new-cdr))
  (displayln (format "  set cdr-lens '(1 2 3) '(20 30) = ~a" cdr-after-set))
  (define cdr-got-after (view cdr-lens cdr-after-set))
  (displayln (format "  view cdr-lens ~a = ~a" cdr-after-set cdr-got-after))
  (displayln (format "  Equal? ~a  ✓" (equal? new-cdr cdr-got-after)))
  (newline)

  (displayln "LAW 3: PutPut")
  (define cdr-set-twice
    (set cdr-lens (set cdr-lens test-list '(10 10)) '(20 30)))
  (define cdr-set-once
    (set cdr-lens test-list '(20 30)))
  (displayln (format "  Set twice: ~a" cdr-set-twice))
  (displayln (format "  Set once:  ~a" cdr-set-once))
  (displayln (format "  Equal? ~a  ✓" (equal? cdr-set-twice cdr-set-once)))
  (newline))

;; ============================================================================
;; Tests
;; ============================================================================

(module+ test
  (displayln "Running lens law tests...")
  (newline)

  ;; car-lens tests
  (displayln "Testing car-lens laws...")

  (check-true
    (verify-get-put car-lens '(1 2 3))
    "car-lens: Law 1 (GetPut)")

  (check-true
    (verify-put-get car-lens '(1 2 3) 10)
    "car-lens: Law 2 (PutGet)")

  (check-true
    (verify-put-put car-lens '(1 2 3) 10 20)
    "car-lens: Law 3 (PutPut)")

  (check-true
    (verify-lens-laws car-lens '(1 2 3) 10 20)
    "car-lens: All laws")

  ;; cdr-lens tests
  (displayln "Testing cdr-lens laws...")

  (check-true
    (verify-get-put cdr-lens '(1 2 3))
    "cdr-lens: Law 1 (GetPut)")

  (check-true
    (verify-put-get cdr-lens '(1 2 3) '(20 30))
    "cdr-lens: Law 2 (PutGet)")

  (check-true
    (verify-put-put cdr-lens '(1 2 3) '(10 10) '(20 30))
    "cdr-lens: Law 3 (PutPut)")

  (check-true
    (verify-lens-laws cdr-lens '(1 2 3) '(10 10) '(20 30))
    "cdr-lens: All laws")

  ;; Edge cases
  (displayln "Testing edge cases...")

  (check-true
    (verify-lens-laws car-lens '(x) 'y 'z)
    "car-lens: Single element list")

  (check-true
    (verify-lens-laws cdr-lens '(1 2) '(3) '(4 5))
    "cdr-lens: Various rest values")

  (displayln "")
  (displayln "✓ All tests passed!"))
