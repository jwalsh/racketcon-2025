#lang racket

;; Basic Universe Forking with Continuations
;; Exploring all possible execution paths

(provide (all-defined-out))

;; ============================================================================
;; Core: Universe Forking with call/cc
;; ============================================================================

;; Global state to track universes
(define universes '())
(define universe-results '())

;; Fork execution into multiple universes
(define (fork . choices)
  (call/cc
   (lambda (k)
     ;; Save continuations for all but first choice
     (for ([choice (cdr choices)])
       (set! universes (cons (cons k choice) universes)))
     ;; Return first choice immediately
     (car choices))))

;; Explore next universe
(define (explore-next)
  (if (null? universes)
      'all-universes-explored
      (let ([next (car universes)])
        (set! universes (cdr universes))
        ((car next) (cdr next)))))

;; Collect result from current universe
(define (collect-result result)
  (set! universe-results (cons result universe-results))
  (explore-next))

;; ============================================================================
;; Example 1: Simple Fork
;; ============================================================================

(module+ main
  (displayln "=== Universe Forking Examples ===\n")

  ;; Reset state
  (set! universes '())
  (set! universe-results '())

  (displayln "Example 1: Simple two-way fork")

  (call/cc
   (lambda (exit)
     (define x (fork 'red 'blue))
     (displayln (~a "Universe: x = " x))

     ;; Collect and continue
     (collect-result x)

     ;; When all done, exit with results
     (exit (reverse universe-results))))

  (displayln (~a "All universes explored: " universe-results))
  (newline))

;; ============================================================================
;; Example 2: Multiple Forks
;; ============================================================================

(module+ multiple
  (displayln "=== Multiple Fork Points ===\n")

  (set! universes '())
  (set! universe-results '())

  (displayln "Example 2: Two fork points (4 universes)")

  (define results
    (call/cc
     (lambda (exit)
       (define color (fork 'red 'blue))
       (define shape (fork 'circle 'square))

       (define result (list color shape))
       (displayln (~a "Universe: " result))

       (collect-result result)
       (exit (reverse universe-results)))))

  (displayln (~a "\nAll 4 universes: " results))
  (newline))

;; ============================================================================
;; Example 3: Three-Way Fork
;; ============================================================================

(module+ three-way
  (displayln "=== Three-Way Fork ===\n")

  (set! universes '())
  (set! universe-results '())

  (displayln "Example 3: Three-way choice (3 universes)")

  (define results
    (call/cc
     (lambda (exit)
       (define choice (fork 'a 'b 'c))
       (displayln (~a "Universe: choice = " choice))

       (collect-result choice)
       (exit (reverse universe-results)))))

  (displayln (~a "\nAll choices explored: " results))
  (newline))

;; ============================================================================
;; Example 4: Conditional Forking
;; ============================================================================

(module+ conditional
  (displayln "=== Conditional Forking ===\n")

  (set! universes '())
  (set! universe-results '())

  (displayln "Example 4: Fork based on previous choice")

  (define results
    (call/cc
     (lambda (exit)
       (define size (fork 'small 'large))
       (define color
         (if (eq? size 'small)
             (fork 'red 'blue)      ; Small items: red or blue
             (fork 'green 'yellow))) ; Large items: green or yellow

       (define result (list size color))
       (displayln (~a "Universe: " result))

       (collect-result result)
       (exit (reverse universe-results)))))

  (displayln (~a "\nAll combinations: " results))
  (newline))

;; ============================================================================
;; Example 5: Accumulating Computation
;; ============================================================================

(module+ accumulate
  (displayln "=== Accumulating Results ===\n")

  (set! universes '())
  (set! universe-results '())

  (displayln "Example 5: Compute in each universe")

  (define results
    (call/cc
     (lambda (exit)
       (define x (fork 1 2 3))
       (define y (fork 10 20))

       (define sum (+ x y))
       (displayln (~a "Universe: " x " + " y " = " sum))

       (collect-result sum)
       (exit (reverse universe-results)))))

  (displayln (~a "\nAll sums: " results))
  (displayln (~a "Total of all universes: " (apply + results)))
  (newline))

;; ============================================================================
;; Example 6: Universe Counting
;; ============================================================================

(module+ counting
  (displayln "=== Universe Counting ===\n")

  (set! universes '())
  (set! universe-results '())

  (displayln "Example 6: Count universes created")

  (define universe-id 0)

  (define results
    (call/cc
     (lambda (exit)
       (set! universe-id (add1 universe-id))
       (define id universe-id)

       (define a (fork 1 2))
       (define b (fork 'x 'y 'z))

       (define result (list id a b))
       (displayln (~a "Universe " id ": " result))

       (collect-result result)
       (exit (reverse universe-results)))))

  (displayln (~a "\nTotal universes created: " (length results)))
  (displayln (~a "Universe details: " results))
  (newline))

;; ============================================================================
;; Example 7: Deep Forking
;; ============================================================================

(module+ deep
  (displayln "=== Deep Universe Forking ===\n")

  (set! universes '())
  (set! universe-results '())

  (displayln "Example 7: Multiple nested forks")

  (define results
    (call/cc
     (lambda (exit)
       (define a (fork 1 2))
       (define b (fork 'x 'y))
       (define c (fork #t #f))
       (define d (fork 'α 'β))

       (define result (list a b c d))
       (displayln (~a "Universe: " result))

       (collect-result result)
       (exit (reverse universe-results)))))

  (displayln (~a "\nTotal universes: " (length results)))
  (displayln (~a "First few: " (take results (min 5 (length results)))))
  (displayln (~a "Last few: " (take-right results (min 5 (length results)))))
  (newline))

;; ============================================================================
;; Example 8: Selective Universe Exploration
;; ============================================================================

(module+ selective
  (displayln "=== Selective Exploration ===\n")

  (set! universes '())
  (set! universe-results '())

  (displayln "Example 8: Only explore valid universes")

  (define (valid-universe? x y)
    (> x y))  ; Only explore where x > y

  (define results
    (call/cc
     (lambda (exit)
       (define x (fork 1 2 3 4))
       (define y (fork 1 2 3 4))

       ;; Skip invalid universes
       (if (valid-universe? x y)
           (let ([result (list x y)])
             (displayln (~a "Valid universe: " result))
             (collect-result result))
           (explore-next))

       (exit (reverse universe-results)))))

  (displayln (~a "\nValid universes (x > y): " results))
  (newline))

;; ============================================================================
;; Example 9: Universe Statistics
;; ============================================================================

(module+ statistics
  (displayln "=== Universe Statistics ===\n")

  (set! universes '())
  (set! universe-results '())

  (displayln "Example 9: Gather statistics across universes")

  (define results
    (call/cc
     (lambda (exit)
       (define dice1 (fork 1 2 3 4 5 6))
       (define dice2 (fork 1 2 3 4 5 6))

       (define sum (+ dice1 dice2))
       (collect-result sum)

       (exit (reverse universe-results)))))

  (displayln (~a "All dice sums: " (length results) " universes"))

  ;; Count occurrences
  (define (count-occurrences lst)
    (for/hash ([val (remove-duplicates lst)])
      (values val (length (filter (lambda (x) (= x val)) lst)))))

  (define counts (count-occurrences results))

  (displayln "\nDistribution:")
  (for ([sum (sort (hash-keys counts) <)])
    (define count (hash-ref counts sum))
    (define bar (make-string count #\█))
    (displayln (~a (~a sum #:width 3) " " (~a count #:width 3) " " bar)))

  (newline))

;; ============================================================================
;; Example 10: All Modules
;; ============================================================================

(module+ all
  (require (submod ".." main))
  (require (submod ".." multiple))
  (require (submod ".." three-way))
  (require (submod ".." conditional))
  (require (submod ".." accumulate))
  (require (submod ".." counting))
  (require (submod ".." deep))
  (require (submod ".." selective))
  (require (submod ".." statistics))

  (displayln "=== All Basic Forking Examples Complete ==="))
