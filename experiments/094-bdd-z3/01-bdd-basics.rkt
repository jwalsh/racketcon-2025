#lang racket

;; BDD Basics: Binary Decision Diagram Implementation
;; Canonical representation of boolean functions

(provide (all-defined-out))

;; ============================================================================
;; BDD Node Structure
;; ============================================================================

;; A BDD node represents a decision point
(struct bdd-node (var low high) #:transparent)

;; Terminal nodes
(define bdd-true #t)
(define bdd-false #f)

;; ============================================================================
;; Node Cache for Sharing
;; ============================================================================

;; Cache for sharing identical nodes
(define node-cache (make-hash))

(define (reset-cache!)
  (set! node-cache (make-hash)))

;; ============================================================================
;; BDD Construction with Sharing
;; ============================================================================

;; Create BDD node with redundancy elimination and sharing
(define (make-bdd var low high)
  (cond
    ;; Redundancy: if both branches point to same node, skip this level
    [(equal? low high) low]

    ;; Check cache for existing identical node
    [else
     (define key (list var low high))
     (hash-ref node-cache key
               (lambda ()
                 (define node (bdd-node var low high))
                 (hash-set! node-cache key node)
                 node))]))

;; ============================================================================
;; Variable Creation
;; ============================================================================

;; Create a BDD variable
(define (bdd-var name)
  (make-bdd name bdd-false bdd-true))

;; ============================================================================
;; BDD Operations
;; ============================================================================

;; NOT operation
(define (bdd-not bdd)
  (cond
    [(boolean? bdd) (not bdd)]
    [(bdd-node? bdd)
     (make-bdd (bdd-node-var bdd)
               (bdd-not (bdd-node-low bdd))
               (bdd-not (bdd-node-high bdd)))]))

;; Apply operation cache
(define apply-cache (make-hash))

(define (reset-apply-cache!)
  (set! apply-cache (make-hash)))

;; Generic binary operation on BDDs
(define (bdd-apply op bdd1 bdd2)
  ;; Check cache first
  (define cache-key (list op bdd1 bdd2))
  (hash-ref apply-cache cache-key
            (lambda ()
              (define result
                (cond
                  ;; Both terminal
                  [(and (boolean? bdd1) (boolean? bdd2))
                   (op bdd1 bdd2)]

                  ;; One terminal, one node
                  [(boolean? bdd1)
                   (bdd-apply op (if bdd1 bdd-true bdd-false) bdd2)]
                  [(boolean? bdd2)
                   (bdd-apply op bdd1 (if bdd2 bdd-true bdd-false))]

                  ;; Both nodes - choose variable with lower order
                  [else
                   (define var1 (bdd-node-var bdd1))
                   (define var2 (bdd-node-var bdd2))

                   (cond
                     ;; Same variable
                     [(equal? var1 var2)
                      (make-bdd var1
                                (bdd-apply op (bdd-node-low bdd1) (bdd-node-low bdd2))
                                (bdd-apply op (bdd-node-high bdd1) (bdd-node-high bdd2)))]

                     ;; var1 < var2 (assuming lexicographic order)
                     [(string<? (symbol->string var1) (symbol->string var2))
                      (make-bdd var1
                                (bdd-apply op (bdd-node-low bdd1) bdd2)
                                (bdd-apply op (bdd-node-high bdd1) bdd2))]

                     ;; var2 < var1
                     [else
                      (make-bdd var2
                                (bdd-apply op bdd1 (bdd-node-low bdd2))
                                (bdd-apply op bdd1 (bdd-node-high bdd2)))])]))

              (hash-set! apply-cache cache-key result)
              result)))

;; AND operation
(define (bdd-and bdd1 bdd2)
  (bdd-apply (lambda (a b) (and a b)) bdd1 bdd2))

;; OR operation
(define (bdd-or bdd1 bdd2)
  (bdd-apply (lambda (a b) (or a b)) bdd1 bdd2))

;; XOR operation
(define (bdd-xor bdd1 bdd2)
  (bdd-apply (lambda (a b) (not (equal? a b))) bdd1 bdd2))

;; ============================================================================
;; Restrict: Assign value to variable
;; ============================================================================

(define (bdd-restrict bdd var value)
  (cond
    [(boolean? bdd) bdd]
    [(bdd-node? bdd)
     (cond
       [(equal? (bdd-node-var bdd) var)
        (if value
            (bdd-node-high bdd)
            (bdd-node-low bdd))]
       [else
        (make-bdd (bdd-node-var bdd)
                  (bdd-restrict (bdd-node-low bdd) var value)
                  (bdd-restrict (bdd-node-high bdd) var value))])]))

;; ============================================================================
;; Satisfy: Find satisfying assignment
;; ============================================================================

(define (bdd-satisfy bdd)
  (cond
    [(equal? bdd bdd-false) #f]
    [(equal? bdd bdd-true) '()]
    [(bdd-node? bdd)
     ;; Try high branch first
     (define high-result (bdd-satisfy (bdd-node-high bdd)))
     (if high-result
         (cons (cons (bdd-node-var bdd) #t) high-result)
         (define low-result (bdd-satisfy (bdd-node-low bdd)))
         (if low-result
             (cons (cons (bdd-node-var bdd) #f) low-result)
             #f))]))

;; ============================================================================
;; Count satisfying assignments
;; ============================================================================

(define (bdd-satcount bdd num-vars)
  (define memo (make-hash))

  (define (count-aux b depth)
    (cond
      [(equal? b bdd-false) 0]
      [(equal? b bdd-true) (expt 2 (- num-vars depth))]
      [else
       (hash-ref memo (cons b depth)
                 (lambda ()
                   (define result
                     (+ (count-aux (bdd-node-low b) (+ depth 1))
                        (count-aux (bdd-node-high b) (+ depth 1))))
                   (hash-set! memo (cons b depth) result)
                   result))]))

  (count-aux bdd 0))

;; ============================================================================
;; BDD Size (node count)
;; ============================================================================

(define (bdd-size bdd)
  (define visited (make-hash))

  (define (count b)
    (cond
      [(boolean? b) 0]
      [(hash-has-key? visited b) 0]
      [else
       (hash-set! visited b #t)
       (+ 1
          (count (bdd-node-low b))
          (count (bdd-node-high b)))]))

  (count bdd))

;; ============================================================================
;; Examples
;; ============================================================================

(module+ main
  (displayln "=== BDD Basics ===\n")

  ;; Create variables
  (define a (bdd-var 'a))
  (define b (bdd-var 'b))
  (define c (bdd-var 'c))

  (displayln "Variables created: a, b, c\n")

  ;; Example 1: (a ∧ b) ∨ (¬a ∧ c)
  (displayln "Example 1: (a ∧ b) ∨ (¬a ∧ c)")
  (define expr1
    (bdd-or (bdd-and a b)
            (bdd-and (bdd-not a) c)))

  (displayln (~a "BDD size: " (bdd-size expr1) " nodes"))
  (displayln (~a "Satisfying assignment: " (bdd-satisfy expr1)))
  (displayln (~a "Number of solutions (3 vars): " (bdd-satcount expr1 3)))
  (newline)

  ;; Example 2: XOR chain
  (displayln "Example 2: a ⊕ b ⊕ c")
  (define expr2
    (bdd-xor (bdd-xor a b) c))

  (displayln (~a "BDD size: " (bdd-size expr2) " nodes"))
  (displayln (~a "Satisfying assignment: " (bdd-satisfy expr2)))
  (displayln (~a "Number of solutions (3 vars): " (bdd-satcount expr2 3)))
  (newline)

  ;; Example 3: Contradiction
  (displayln "Example 3: a ∧ ¬a")
  (define expr3
    (bdd-and a (bdd-not a)))

  (displayln (~a "Result: " expr3 " (should be #f)"))
  (displayln (~a "Satisfying assignment: " (bdd-satisfy expr3)))
  (newline)

  ;; Example 4: Tautology
  (displayln "Example 4: a ∨ ¬a")
  (define expr4
    (bdd-or a (bdd-not a)))

  (displayln (~a "Result: " expr4 " (should be #t)"))
  (displayln (~a "All assignments satisfy: " (= (bdd-satcount expr4 1) 2)))
  (newline)

  ;; Example 5: Restrict operation
  (displayln "Example 5: Restrict (a ∧ b) ∨ c with a=true")
  (define expr5 (bdd-or (bdd-and a b) c))
  (define restricted (bdd-restrict expr5 'a #t))

  (displayln (~a "Original: " expr5))
  (displayln (~a "After a=true: " restricted))
  (displayln (~a "Simplified to: b ∨ c"))
  (newline)

  ;; Cache statistics
  (displayln "Cache Statistics:")
  (displayln (~a "Node cache size: " (hash-count node-cache)))
  (displayln (~a "Apply cache size: " (hash-count apply-cache)))

  (displayln "\n=== BDD Basics Complete ==="))
