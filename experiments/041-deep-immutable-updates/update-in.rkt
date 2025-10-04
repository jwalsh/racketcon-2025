#lang racket

;; Deep Immutable Updates - update-in implementation

(provide update-in
         set-in
         get-in
         update-many
         update-when)

;; ============================================================================
;; Core update-in Implementation
;; ============================================================================

(define (update-in data path fn)
  "Update value at path by applying fn. Works with hashes, lists, vectors."
  (cond
    ;; Base case: empty path, apply function
    [(null? path)
     (fn data)]

    ;; Hash
    [(hash? data)
     (define key (car path))
     (define rest-path (cdr path))
     (hash-set data key
       (update-in (hash-ref data key) rest-path fn))]

    ;; List
    [(list? data)
     (define idx (car path))
     (define rest-path (cdr path))
     (list-set data idx
       (update-in (list-ref data idx) rest-path fn))]

    ;; Vector
    [(vector? data)
     (define idx (car path))
     (define rest-path (cdr path))
     (define result (vector-copy data))
     (vector-set! result idx
       (update-in (vector-ref data idx) rest-path fn))
     result]

    ;; Struct - would need struct-set! and struct-ref
    [else
     (error 'update-in "Cannot update type: ~a" data)]))

;; ============================================================================
;; Convenience Functions
;; ============================================================================

(define (set-in data path value)
  "Set value at path (shorthand for update-in with constant function)."
  (update-in data path (λ (_) value)))

(define (get-in data path [default #f])
  "Get value at path, returning default if not found."
  (cond
    [(null? path) data]
    [(hash? data)
     (define key (car path))
     (if (hash-has-key? data key)
         (get-in (hash-ref data key) (cdr path) default)
         default)]
    [(list? data)
     (define idx (car path))
     (if (< idx (length data))
         (get-in (list-ref data idx) (cdr path) default)
         default)]
    [(vector? data)
     (define idx (car path))
     (if (< idx (vector-length data))
         (get-in (vector-ref data idx) (cdr path) default)
         default)]
    [else default]))

;; ============================================================================
;; Batch Updates
;; ============================================================================

(define (update-many data updates)
  "Apply multiple updates to data. updates is list of (path fn) pairs."
  (for/fold ([result data])
            ([update updates])
    (match update
      [(list path fn)
       (update-in result path fn)]
      [_
       (error 'update-many "Invalid update format: ~a" update)])))

;; ============================================================================
;; Conditional Updates
;; ============================================================================

(define (update-when data path pred? fn)
  "Update only if predicate holds for current value."
  (if (pred? (get-in data path))
      (update-in data path fn)
      data))

;; ============================================================================
;; Examples
;; ============================================================================

(module+ test
  (require rackunit)

  ;; Test data
  (define person
    (hash 'name "Alice"
          'age 30
          'address (hash 'street "123 Main St"
                         'city "Seattle"
                         'state "WA")))

  ;; Test update-in with hash
  (check-equal?
    (get-in (update-in person '(age) add1) '(age))
    31
    "update-in increments age")

  (check-equal?
    (get-in (update-in person '(address city) (λ (_) "Portland")) '(address city))
    "Portland"
    "update-in changes nested city")

  ;; Test set-in
  (check-equal?
    (get-in (set-in person '(phone) "555-1234") '(phone))
    "555-1234"
    "set-in adds new key")

  ;; Test get-in
  (check-equal?
    (get-in person '(address city))
    "Seattle"
    "get-in retrieves nested value")

  (check-equal?
    (get-in person '(missing key) 'default)
    'default
    "get-in returns default for missing key")

  ;; Test with lists
  (define team
    (list
      (hash 'name "Alice" 'score 100)
      (hash 'name "Bob" 'score 95)))

  (check-equal?
    (get-in (update-in team '(0 score) add1) '(0 score))
    101
    "update-in works with list index")

  ;; Test update-many
  (define updated-person
    (update-many person
      '([(age) ,add1]
        [(address city) ,(λ (_) "Portland")])))

  (check-equal? (get-in updated-person '(age)) 31)
  (check-equal? (get-in updated-person '(address city)) "Portland")

  ;; Test update-when
  (define maybe-updated
    (update-when person '(age)
      (λ (age) (< age 40))
      add1))

  (check-equal? (get-in maybe-updated '(age)) 31 "age updated when < 40")

  (define not-updated
    (update-when person '(age)
      (λ (age) (> age 40))
      add1))

  (check-equal? (get-in not-updated '(age)) 30 "age not updated when condition false")

  (displayln "All tests passed!"))

;; ============================================================================
;; Demo
;; ============================================================================

(module+ main
  (define person
    (hash 'name "Alice"
          'age 30
          'address (hash 'street "123 Main St"
                         'city "Seattle"
                         'state "WA")))

  (displayln "Original person:")
  (displayln person)
  (newline)

  (displayln "Update age:")
  (displayln (update-in person '(age) add1))
  (newline)

  (displayln "Update nested city:")
  (displayln (update-in person '(address city) (λ (_) "Portland")))
  (newline)

  (displayln "Multiple updates:")
  (displayln
    (update-many person
      `([(age) ,add1]
        [(address city) ,(λ (_) "Portland")]
        [(phone) ,(λ (_) "555-1234")])))
  (newline)

  ;; Complex example
  (define company
    (hash 'name "Tech Corp"
          'departments
            (list
              (hash 'name "Engineering"
                    'employees
                      (list
                        (hash 'id 1 'name "Alice" 'salary 100000)
                        (hash 'id 2 'name "Bob" 'salary 95000)))
              (hash 'name "Sales"
                    'employees
                      (list
                        (hash 'id 3 'name "Charlie" 'salary 80000))))))

  (displayln "Original company:")
  (displayln company)
  (newline)

  (displayln "Update Alice's salary:")
  (displayln
    (update-in company '(departments 0 employees 0 salary)
      (λ (s) (* s 1.1))))
  (newline)

  (displayln "Bulk salary update for Engineering:")
  (define updated-company
    (update-in company '(departments 0 employees)
      (λ (employees)
        (for/list ([emp employees])
          (hash-update emp 'salary (λ (s) (* s 1.1)))))))

  (displayln updated-company))
