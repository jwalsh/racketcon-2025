#lang racket

;; Experiment 049: Ocular-Patdown Optics Guide
;; Part 3: Traversals - Multiple Foci Optics
;;
;; Based on: https://docs.racket-lang.org/ocular-patdown/optics-guide.html

(require ocular-patdown)

;; ============================================================================
;; What are Traversals?
;; ============================================================================

;; A traversal focuses on ZERO OR MORE subparts of a data structure
;; Unlike lenses (exactly 1), traversals can have 0, 1, 2, ... N foci

(displayln "=== What are Traversals? ===")
(displayln "A traversal is an optic that focuses on 0 or more subparts")
(displayln "Examples: all list elements, all matching values, all leaves")
(newline)

;; ============================================================================
;; List Traversal
;; ============================================================================

(define numbers '(1 2 3 4 5))

(displayln "=== List Traversal ===")
(displayln (format "Original list: ~a" numbers))

;; Transform all elements
(define doubled (lens-transform list-traversal (λ (x) (* x 2)) numbers))
(displayln (format "Double all: ~a" doubled))

;; Set all elements to same value
(define all-zeros (lens-set list-traversal numbers 0))
(displayln (format "Set all to 0: ~a" all-zeros))
(newline)

;; ============================================================================
;; Vector Traversal
;; ============================================================================

(define vec (vector 10 20 30 40))

(displayln "=== Vector Traversal ===")
(displayln (format "Original vector: ~a" vec))

(define incremented (lens-transform vector-traversal add1 vec))
(displayln (format "Increment all: ~a" incremented))

(define reset (lens-set vector-traversal vec 0))
(displayln (format "Reset all: ~a" reset))
(newline)

;; ============================================================================
;; Hash Values Traversal
;; ============================================================================

(define scores (hash 'alice 85 'bob 92 'carol 78))

(displayln "=== Hash Values Traversal ===")
(displayln (format "Original scores: ~a" scores))

;; Add 5 points to everyone's score
(define curved (lens-transform hash-values-traversal (λ (x) (+ x 5)) scores))
(displayln (format "Add 5 to all: ~a" curved))
(newline)

;; ============================================================================
;; Filtered Traversals
;; ============================================================================

(displayln "=== Filtered Traversals ===")

;; Focus only on elements matching a predicate
(define mixed-list '(1 2 3 4 5 6 7 8 9 10))
(displayln (format "Original: ~a" mixed-list))

;; Transform only even numbers
(define even-doubled
  (lens-transform (filtered-traversal even?) (λ (x) (* x 2)) mixed-list))
(displayln (format "Double evens: ~a" even-doubled))

;; Set only odd numbers to 0
(define odds-zeroed
  (lens-set (filtered-traversal odd?) mixed-list 0))
(displayln (format "Zero odds: ~a" odds-zeroed))
(newline)

;; ============================================================================
;; Composing Traversals with Lenses
;; ============================================================================

(struct person (name age) #:transparent)

(define people
  (list (person "Alice" 30)
        (person "Bob" 25)
        (person "Carol" 35)))

;; Create lens for age field
(define person-age-lens
  (lens person-age (λ (p v) (struct-copy person p [age v]))))

;; Compose: traverse list, then access age field
(define all-ages-traversal
  (lens-compose list-traversal person-age-lens))

(displayln "=== Composing Traversals with Lenses ===")
(displayln (format "Original people: ~a" people))

;; Add 1 year to everyone's age
(define birthday (lens-transform all-ages-traversal add1 people))
(displayln (format "Everyone ages 1 year: ~a" birthday))

;; Set everyone's age to 25
(define same-age (lens-set all-ages-traversal people 25))
(displayln (format "Set all ages to 25: ~a" same-age))
(newline)

;; ============================================================================
;; Nested Traversals
;; ============================================================================

(displayln "=== Nested Traversals ===")

(define matrix '((1 2 3) (4 5 6) (7 8 9)))
(displayln (format "Matrix: ~a" matrix))

;; Traverse outer list, then traverse inner lists
(define nested-traversal
  (lens-compose list-traversal list-traversal))

;; Transform all elements in 2D structure
(define squared (lens-transform nested-traversal (λ (x) (* x x)) matrix))
(displayln (format "Square all: ~a" squared))
(newline)

;; ============================================================================
;; Traversal vs Lens: Number of Foci
;; ============================================================================

(displayln "=== Traversal vs Lens: Number of Foci ===")
(displayln "Lens:      exactly 1 focus")
(displayln "Prism:     0 or 1 focus")
(displayln "Traversal: 0, 1, 2, ... N foci")
(newline)

;; Empty list: 0 foci
(define empty-list '())
(displayln (format "Empty list transform: ~a"
                   (lens-transform list-traversal add1 empty-list)))

;; Single element: 1 focus
(define single '(42))
(displayln (format "Single element transform: ~a"
                   (lens-transform list-traversal add1 single)))

;; Multiple elements: N foci
(define many '(1 2 3 4 5))
(displayln (format "Many elements transform: ~a"
                   (lens-transform list-traversal add1 many)))
(newline)

;; ============================================================================
;; Practical Example: Updating Nested Configurations
;; ============================================================================

(struct server (name host port) #:transparent)
(struct cluster (name servers) #:transparent)

(define production-cluster
  (cluster "production"
           (list (server "web1" "10.0.1.1" 8080)
                 (server "web2" "10.0.1.2" 8080)
                 (server "web3" "10.0.1.3" 8080))))

;; Lenses for struct fields
(define cluster-servers-lens
  (lens cluster-servers (λ (c v) (struct-copy cluster c [servers v]))))

(define server-port-lens
  (lens server-port (λ (s v) (struct-copy server s [port v]))))

;; Compose: access servers list, traverse it, access port
(define all-ports-traversal
  (lens-compose cluster-servers-lens
                list-traversal
                server-port-lens))

(displayln "=== Practical Example: Cluster Configuration ===")
(displayln (format "Original cluster: ~a" production-cluster))

;; Update all server ports to 9000
(define updated-cluster
  (lens-set all-ports-traversal production-cluster 9000))
(displayln (format "Update all ports to 9000: ~a" updated-cluster))
(newline)

;; ============================================================================
;; Summary
;; ============================================================================

(displayln "=== Summary: Traversals ===")
(displayln "1. Traversals focus on 0 or more subparts")
(displayln "2. Built-in: list-traversal, vector-traversal, hash-values-traversal")
(displayln "3. Filtered: filtered-traversal with predicate")
(displayln "4. Compose with lenses to reach nested fields")
(displayln "5. Compose with traversals for nested structures")
(displayln "")
(displayln "Next: 04-isomorphisms.rkt - Bidirectional conversions")
