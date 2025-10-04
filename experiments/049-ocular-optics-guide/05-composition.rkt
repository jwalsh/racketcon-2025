#lang racket

;; Experiment 049: Ocular-Patdown Optics Guide
;; Part 5: Composition - Combining Optics
;;
;; Based on: https://docs.racket-lang.org/ocular-patdown/optics-guide.html

(require racket/struct)
(require ocular-patdown)

;; ============================================================================
;; Why Composition?
;; ============================================================================

(displayln "=== Why Composition? ===")
(displayln "Optics compose to build complex paths from simple pieces")
(displayln "Benefits:")
(displayln "  1. Modularity: Define small, reusable optics")
(displayln "  2. Clarity: Express nested access declaratively")
(displayln "  3. Type-safety: Compiler checks compositions")
(newline)

;; ============================================================================
;; Basic Lens Composition
;; ============================================================================

(struct address (street city state zip) #:transparent)
(struct person (name address) #:transparent)
(struct company (name employees) #:transparent)

;; Simple lenses
(define person-address-lens
  (lens person-address (λ (p v) (struct-copy person p [address v]))))

(define address-city-lens
  (lens address-city (λ (a v) (struct-copy address a [city v]))))

;; Compose: person -> address -> city
(define person-city-lens
  (lens-compose person-address-lens address-city-lens))

(define alice
  (person "Alice"
          (address "123 Main St" "Boston" "MA" "02101")))

(displayln "=== Basic Lens Composition ===")
(displayln (format "Person: ~a" alice))
(displayln (format "City: ~a" (lens-view person-city-lens alice)))
(displayln (format "Move to Cambridge: ~a"
                   (lens-set person-city-lens alice "Cambridge")))
(newline)

;; ============================================================================
;; Composing Lenses with Traversals
;; ============================================================================

(define company-employees-lens
  (lens company-employees (λ (c v) (struct-copy company c [employees v]))))

(define acme
  (company "Acme Corp"
           (list (person "Alice" (address "123 Main" "Boston" "MA" "02101"))
                 (person "Bob" (address "456 Oak" "Cambridge" "MA" "02138"))
                 (person "Carol" (address "789 Elm" "Somerville" "MA" "02144")))))

;; Compose: company -> employees list -> each person -> city
(define all-employee-cities-traversal
  (lens-compose company-employees-lens
                list-traversal
                person-city-lens))

(displayln "=== Lens + Traversal Composition ===")
(displayln (format "Company: ~a" (company-name acme)))
(displayln "Update all employees to work from Boston office:")
(displayln (lens-set all-employee-cities-traversal acme "Boston"))
(newline)

;; ============================================================================
;; Multi-Level Composition
;; ============================================================================

(struct project (name owner collaborators) #:transparent)

(define proj
  (project "Widget"
           (person "Alice" (address "123 Main" "Boston" "MA" "02101"))
           (list (person "Bob" (address "456 Oak" "Cambridge" "MA" "02138"))
                 (person "Carol" (address "789 Elm" "Somerville" "MA" "02144")))))

;; Lenses for project
(define project-owner-lens
  (lens project-owner (λ (p v) (struct-copy project p [owner v]))))

(define project-collaborators-lens
  (lens project-collaborators (λ (p v) (struct-copy project p [collaborators v]))))

;; Access owner's city
(define owner-city-lens
  (lens-compose project-owner-lens person-city-lens))

;; Access all collaborator cities
(define collaborator-cities-traversal
  (lens-compose project-collaborators-lens
                list-traversal
                person-city-lens))

(displayln "=== Multi-Level Composition ===")
(displayln (format "Project: ~a" (project-name proj)))
(displayln (format "Owner city: ~a" (lens-view owner-city-lens proj)))
(displayln "Move all collaborators to Austin:")
(displayln (lens-set collaborator-cities-traversal proj "Austin"))
(newline)

;; ============================================================================
;; Composition with Isomorphisms
;; ============================================================================

;; Compose iso with lens to transform during access

(define string-chars-iso
  (iso string->list list->string))

(define person-name-lens
  (lens person-name (λ (p v) (struct-copy person p [name v]))))

;; Access name as list of characters
(define person-name-chars-optic
  (lens-compose person-name-lens string-chars-iso))

(displayln "=== Composition with Isomorphisms ===")
(displayln (format "Person: ~a" (person-name alice)))
(displayln (format "Name as chars: ~a"
                   (lens-view person-name-chars-optic alice)))

;; Reverse the name
(define reversed-name
  (lens-transform person-name-chars-optic reverse alice))
(displayln (format "Reversed name: ~a" (person-name reversed-name)))
(newline)

;; ============================================================================
;; Thrush: Pipeline-Style Composition
;; ============================================================================

(displayln "=== Thrush: Pipeline Composition ===")

;; lens-thrush allows pipeline-style composition
;; Syntax: (lens-thrush data optic1 optic2 ... operation)

(displayln "Traditional composition:")
(displayln (lens-view (lens-compose person-address-lens address-city-lens) alice))

(displayln "Thrush style:")
(displayln (lens-thrush alice person-address-lens address-city-lens lens-view))
(newline)

;; ============================================================================
;; Composition Laws
;; ============================================================================

(displayln "=== Composition Laws ===")
(displayln "Optic composition is associative:")
(displayln "  (compose a (compose b c)) = (compose (compose a b) c)")
(displayln "")
(displayln "Identity optic exists:")
(displayln "  (compose identity-optic o) = o")
(displayln "  (compose o identity-optic) = o")
(newline)

;; ============================================================================
;; Practical Pattern: Building Path DSL
;; ============================================================================

(displayln "=== Pattern: Building Path DSL ===")

;; Define vocabulary of optics
(define addr person-address-lens)
(define city address-city-lens)
(define name person-name-lens)
(define chars string-chars-iso)

;; Compose fluently
(define city-path (lens-compose addr city))
(define city-chars-path (lens-compose addr city chars))

(displayln "Using path DSL:")
(displayln (format "City: ~a" (lens-view city-path alice)))
(displayln (format "City chars: ~a" (lens-view city-chars-path alice)))
(newline)

;; ============================================================================
;; Complex Example: Nested JSON-like Structure
;; ============================================================================

(displayln "=== Complex Example: Nested Structure ===")

(define api-response
  (hash 'status "success"
        'data (hash 'users (list (hash 'id 1 'name "Alice" 'active #t)
                                 (hash 'id 2 'name "Bob" 'active #f)
                                 (hash 'id 3 'name "Carol" 'active #t)))))

;; Build path: data -> users -> each user -> name
(define all-user-names-path
  (lens-compose (hash-ref-lens 'data)
                (hash-ref-lens 'users)
                list-traversal
                (hash-ref-lens 'name)))

(displayln (format "API response has ~a users"
                   (length (hash-ref (hash-ref api-response 'data) 'users))))

;; Transform all names to uppercase
(define uppercased-response
  (lens-transform all-user-names-path string-upcase api-response))

(displayln "All names uppercased:")
(displayln uppercased-response)
(newline)

;; ============================================================================
;; Filtered Composition
;; ============================================================================

(displayln "=== Filtered Composition ===")

;; Build path: users -> active users only -> name
(define active-user-names-path
  (lens-compose (hash-ref-lens 'data)
                (hash-ref-lens 'users)
                (filtered-traversal (λ (user) (hash-ref user 'active)))
                (hash-ref-lens 'name)))

;; Transform only active user names
(define active-uppercased
  (lens-transform active-user-names-path string-upcase api-response))

(displayln "Active users uppercased:")
(displayln active-uppercased)
(newline)

;; ============================================================================
;; Summary
;; ============================================================================

(displayln "=== Summary: Composition ===")
(displayln "1. Optics compose with lens-compose")
(displayln "2. Lens + Lens = deeper lens")
(displayln "3. Lens + Traversal = traversal to nested values")
(displayln "4. Lens + Iso = lens with representation change")
(displayln "5. Use lens-thrush for pipeline style")
(displayln "6. Build domain-specific path vocabularies")
(displayln "")
(displayln "Next: 06-practical-patterns.rkt - Real-world usage")
