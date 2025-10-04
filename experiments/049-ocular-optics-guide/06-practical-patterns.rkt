#lang racket

;; Experiment 049: Ocular-Patdown Optics Guide
;; Part 6: Practical Patterns - Real-World Usage
;;
;; Based on: https://docs.racket-lang.org/ocular-patdown/optics-guide.html

(require racket/struct)
(require racket/match)
(require ocular-patdown)

;; ============================================================================
;; Pattern 1: Configuration Management
;; ============================================================================

(displayln "=== Pattern 1: Configuration Management ===")

(struct db-config (host port user password) #:transparent)
(struct cache-config (enabled ttl max-size) #:transparent)
(struct app-config (name version db cache) #:transparent)

;; Create lenses for nested config
(define app-db-lens
  (lens app-config-db (λ (c v) (struct-copy app-config c [db v]))))

(define db-host-lens
  (lens db-config-host (λ (c v) (struct-copy db-config c [host v]))))

(define db-port-lens
  (lens db-config-port (λ (c v) (struct-copy db-config c [port v]))))

(define app-cache-lens
  (lens app-config-cache (λ (c v) (struct-copy app-config c [cache v]))))

(define cache-ttl-lens
  (lens cache-config-ttl (λ (c v) (struct-copy cache-config c [ttl v]))))

;; Composed paths
(define db-host-path (lens-compose app-db-lens db-host-lens))
(define db-port-path (lens-compose app-db-lens db-port-lens))
(define cache-ttl-path (lens-compose app-cache-lens cache-ttl-lens))

;; Configuration transformations
(define dev-config
  (app-config "myapp" "1.0.0"
              (db-config "localhost" 5432 "dev" "dev123")
              (cache-config #t 3600 1000)))

(displayln (format "Dev config: ~a" dev-config))

;; Derive production config
(define prod-config
  (lens-set cache-ttl-path
            (lens-set db-host-path dev-config "prod.db.example.com")
            7200))

(displayln (format "Prod config: ~a" prod-config))
(newline)

;; ============================================================================
;; Pattern 2: State Updates in MVC/Redux Style
;; ============================================================================

(displayln "=== Pattern 2: State Updates (MVC/Redux) ===")

(struct todo (id text completed) #:transparent)
(struct app-state (todos filter) #:transparent)

;; Lenses
(define state-todos-lens
  (lens app-state-todos (λ (s v) (struct-copy app-state s [todos v]))))

(define todo-completed-lens
  (lens todo-completed (λ (t v) (struct-copy todo t [completed v]))))

;; Find todo by id
(define (find-todo-lens id)
  (filtered-traversal (λ (t) (= (todo-id t) id))))

;; Compose: state -> todos -> specific todo -> completed
(define (toggle-todo-path id)
  (lens-compose state-todos-lens
                (find-todo-lens id)
                todo-completed-lens))

;; State
(define state
  (app-state (list (todo 1 "Buy milk" #f)
                   (todo 2 "Write code" #t)
                   (todo 3 "Sleep" #f))
             'all))

(displayln (format "Initial state: ~a" state))

;; Action: Toggle todo 1
(define toggled
  (lens-transform (toggle-todo-path 1) not state))

(displayln (format "After toggle: ~a" toggled))
(newline)

;; ============================================================================
;; Pattern 3: Form Field Updates
;; ============================================================================

(displayln "=== Pattern 3: Form Field Updates ===")

(struct form-field (name value errors) #:transparent)
(struct form (fields submitted) #:transparent)

;; Helper: find field by name
(define (find-field-lens name)
  (filtered-traversal (λ (f) (string=? (form-field-name f) name))))

(define form-fields-lens
  (lens form-fields (λ (f v) (struct-copy form f [fields v]))))

(define field-value-lens
  (lens form-field-value (λ (f v) (struct-copy form-field f [value v]))))

;; Path to specific field value
(define (field-value-path name)
  (lens-compose form-fields-lens
                (find-field-lens name)
                field-value-lens))

;; Form
(define login-form
  (form (list (form-field "username" "" '())
              (form-field "password" "" '())
              (form-field "remember" #f '()))
        #f))

(displayln (format "Initial form: ~a" login-form))

;; Update username field
(define updated-form
  (lens-set (field-value-path "username") login-form "alice"))

(displayln (format "After update: ~a" updated-form))
(newline)

;; ============================================================================
;; Pattern 4: Tree Traversal and Updates
;; ============================================================================

(displayln "=== Pattern 4: Tree Traversal ===")

(struct node (value children) #:transparent)

;; Lens for node children
(define node-children-lens
  (lens node-children (λ (n v) (struct-copy node n [children v]))))

;; Lens for node value
(define node-value-lens
  (lens node-value (λ (n v) (struct-copy node n [value v]))))

;; All values in tree (recursive traversal)
(define all-tree-values-traversal
  (lens-compose list-traversal
                node-value-lens))

;; Tree
(define tree
  (list (node 1 (list (node 2 '())
                      (node 3 '())))
        (node 4 (list (node 5 '())))))

(displayln (format "Tree: ~a" tree))

;; Double all values
(define doubled-tree
  (lens-transform all-tree-values-traversal (λ (x) (* x 2)) tree))

(displayln (format "Doubled: ~a" doubled-tree))
(newline)

;; ============================================================================
;; Pattern 5: Data Validation with Optics
;; ============================================================================

(displayln "=== Pattern 5: Data Validation ===")

(struct validated (value errors) #:transparent)

;; Lens for validated value
(define validated-value-lens
  (lens validated-value (λ (v val) (struct-copy validated v [value val]))))

;; Validate and transform
(define (validate-email v)
  (if (string-contains? (validated-value v) "@")
      v
      (struct-copy validated v [errors (cons "Invalid email" (validated-errors v))])))

(define email-field (validated "alice" '()))

(displayln (format "Before validation: ~a" email-field))
(displayln (format "After validation: ~a" (validate-email email-field)))

;; Fix with optics
(define fixed (lens-set validated-value-lens email-field "alice@example.com"))
(displayln (format "Fixed: ~a" fixed))
(displayln (format "Validated: ~a" (validate-email fixed)))
(newline)

;; ============================================================================
;; Pattern 6: API Response Processing
;; ============================================================================

(displayln "=== Pattern 6: API Response Processing ===")

;; Simulate API response
(define api-data
  (hash 'meta (hash 'status 200 'timestamp 1234567890)
        'data (list (hash 'id 1 'name "Alice" 'email "alice@example.com")
                    (hash 'id 2 'name "Bob" 'email "bob@example.com")
                    (hash 'id 3 'name "Carol" 'email "carol@example.com"))))

;; Extract all emails
(define all-emails-path
  (lens-compose (hash-ref-lens 'data)
                list-traversal
                (hash-ref-lens 'email)))

(displayln "All emails:")
(displayln (lens-view all-emails-path api-data))

;; Anonymize emails
(define anonymized
  (lens-transform all-emails-path
                  (λ (email) "***@***")
                  api-data))

(displayln "Anonymized:")
(displayln anonymized)
(newline)

;; ============================================================================
;; Pattern 7: Lens-Based Getters/Setters
;; ============================================================================

(displayln "=== Pattern 7: Lens-Based Getters/Setters ===")

;; Create getter/setter functions from lenses
(define (make-getter lens)
  (λ (target) (lens-view lens target)))

(define (make-setter lens)
  (λ (target value) (lens-set lens target value)))

;; Usage
(define get-db-host (make-getter db-host-path))
(define set-db-host (make-setter db-host-path))

(displayln (format "Get host: ~a" (get-db-host dev-config)))
(displayln (format "Set host: ~a" (set-db-host dev-config "new-host")))
(newline)

;; ============================================================================
;; Pattern 8: Optics with Pattern Matching
;; ============================================================================

(displayln "=== Pattern 8: Optics with Pattern Matching ===")

(define (update-config config)
  (match config
    [(app-config name version db cache)
     ;; Use optics for targeted updates
     (lens-transform cache-ttl-path (λ (ttl) (* ttl 2)) config)]))

(displayln (format "Original: ~a" dev-config))
(displayln (format "Updated: ~a" (update-config dev-config)))
(newline)

;; ============================================================================
;; Pattern 9: Conditional Updates
;; ============================================================================

(displayln "=== Pattern 9: Conditional Updates ===")

;; Update only if predicate holds
(define (conditional-transform lens pred f target)
  (if (pred (lens-view lens target))
      (lens-transform lens f target)
      target))

;; Double port only if it's < 10000
(define maybe-doubled
  (conditional-transform db-port-path
                        (λ (p) (< p 10000))
                        (λ (p) (* p 2))
                        dev-config))

(displayln (format "Conditional double: ~a" maybe-doubled))
(newline)

;; ============================================================================
;; Summary
;; ============================================================================

(displayln "=== Summary: Practical Patterns ===")
(displayln "1. Configuration: Build paths to nested settings")
(displayln "2. State management: Redux-style updates")
(displayln "3. Forms: Field-level updates")
(displayln "4. Trees: Recursive traversals")
(displayln "5. Validation: Compose optics with validators")
(displayln "6. APIs: Extract and transform response data")
(displayln "7. Getters/Setters: Create from lenses")
(displayln "8. Pattern matching: Combine with match")
(displayln "9. Conditional: Update based on predicates")
(displayln "")
(displayln "=== Complete! ===")
(displayln "You've completed the ocular-patdown optics guide walkthrough.")
(displayln "See README.org for related experiments and resources.")
