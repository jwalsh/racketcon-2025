#lang racket

;; Example of dupdate - Mike Delmonaco's match-like update DSL
;; This is a conceptual demo of how the syntax might work

;; ============================================================================
;; Simple List Example
;; ============================================================================

(displayln "=== Simple List Update Example ===\n")

(define lst '(1 2 3))

(displayln "Original list:")
(displayln lst)
(newline)

;; Conceptual dupdate syntax (if it existed):
;; (dupdate lst
;;   [(list a b c)
;;    (list a b 10)])  ; Replace third element with 10

;; Without dupdate - manual approach:
(define lst-updated-manual
  (match lst
    [(list a b c)
     (list a b 10)]))

(displayln "Updated with manual match:")
(displayln lst-updated-manual)
(newline)

;; Using list-set (mutable style)
(define lst-updated-set
  (list-set lst 2 10))

(displayln "Updated with list-set:")
(displayln lst-updated-set)
(newline)

;; ============================================================================
;; Nested Structure Example
;; ============================================================================

(displayln "=== Nested Structure Update ===\n")

(define data
  (hash 'user (hash 'name "Alice"
                    'scores (list 85 90 92))))

(displayln "Original data:")
(displayln data)
(newline)

;; Conceptual dupdate for nested update:
;; (dupdate data
;;   [(hash 'user (hash 'scores (list a b c)))
;;    ; Pattern vars a, b, c bind to scores
;;    ; Update last score
;;    (hash 'user (hash 'scores (list a b 100)))])

;; Manual approach:
(define data-updated-manual
  (hash-update data 'user
    (λ (user)
      (hash-update user 'scores
        (λ (scores)
          (match scores
            [(list a b c)
             (list a b 100)]))))))

(displayln "Updated manually:")
(displayln data-updated-manual)
(newline)

;; ============================================================================
;; What dupdate Syntax Might Look Like
;; ============================================================================

(displayln "=== Hypothetical dupdate Syntax ===\n")

(displayln "Example 1: Update list element")
(displayln "(dupdate '(1 2 3)")
(displayln "  [(list a b _)")
(displayln "   (list a b 10)])")
(displayln "=> '(1 2 10)")
(newline)

(displayln "Example 2: Increment list element using pattern var")
(displayln "(dupdate '(1 2 3)")
(displayln "  [(list a b c)")
(displayln "   (list a b (+ c 1))])")
(displayln "=> '(1 2 4)")
(newline)

(displayln "Example 3: Update nested hash")
(displayln "(dupdate (hash 'x 1 'y (hash 'z 2))")
(displayln "  [(hash 'y (hash 'z val))")
(displayln "   (hash 'y (hash 'z (* val 2)))])")
(displayln "=> (hash 'x 1 'y (hash 'z 4))")
(newline)

(displayln "Example 4: Multiple updates")
(displayln "(dupdate person")
(displayln "  [(hash 'age a 'name n)")
(displayln "   (hash 'age (+ a 1)")
(displayln "         'name (string-upcase n))])")
(newline)

;; ============================================================================
;; Simulating dupdate with Macro
;; ============================================================================

(displayln "=== Simulated dupdate Implementation ===\n")

(define-syntax-rule (simple-dupdate data
                       [pattern template])
  (match data
    [pattern template]))

;; Usage
(define result1
  (simple-dupdate '(1 2 3)
    [(list a b c)
     (list a b 10)]))

(displayln "Using simple-dupdate macro:")
(displayln result1)
(newline)

;; ============================================================================
;; More Sophisticated Example
;; ============================================================================

(displayln "=== Complex Nested Update ===\n")

(define company
  (hash 'name "Tech Corp"
        'employees
          (list
            (hash 'id 1 'name "Alice" 'salary 100000)
            (hash 'id 2 'name "Bob" 'salary 95000)
            (hash 'id 3 'name "Charlie" 'salary 90000))))

(displayln "Original company:")
(displayln company)
(newline)

;; Hypothetical dupdate to give Alice a raise:
;; (dupdate company
;;   [(hash 'employees (list (hash 'name "Alice" 'salary s) rest ...))
;;    (hash 'employees (list (hash 'name "Alice" 'salary (* s 1.1)) rest ...))])

;; Manual version:
(define company-updated
  (hash-update company 'employees
    (λ (emps)
      (for/list ([emp emps])
        (if (equal? (hash-ref emp 'name) "Alice")
            (hash-update emp 'salary (λ (s) (* s 1.1)))
            emp)))))

(displayln "After giving Alice a 10% raise:")
(displayln company-updated)
(newline)

;; ============================================================================
;; Comparison: dupdate vs update-in vs manual
;; ============================================================================

(displayln "=== Syntax Comparison ===\n")

(define person
  (hash 'name "Alice"
        'age 30
        'address (hash 'street "123 Main St"
                       'city "Seattle")))

(displayln "Task: Change city to Portland\n")

(displayln "1. Hypothetical dupdate:")
(displayln "(dupdate person")
(displayln "  [(hash 'address (hash 'city _))")
(displayln "   (hash 'address (hash 'city \"Portland\"))])")
(newline)

(displayln "2. Using update-in:")
(displayln "(update-in person '(address city) (λ (_) \"Portland\"))")
(newline)

(displayln "3. Manual hash-update:")
(displayln "(hash-update person 'address")
(displayln "  (λ (addr) (hash-set addr 'city \"Portland\")))")
(newline)

;; ============================================================================
;; Key Benefits of dupdate
;; ============================================================================

(displayln "=== Benefits of dupdate DSL ===\n")

(displayln "1. DECLARATIVE:")
(displayln "   - Shows what you're matching and what changes")
(displayln "   - Pattern reveals structure")
(newline)

(displayln "2. CONCISE:")
(displayln "   - No nested lambdas or explicit updates")
(displayln "   - Pattern variables bind values for reuse")
(newline)

(displayln "3. TYPE-SAFE:")
(displayln "   - Pattern match ensures structure exists")
(displayln "   - Compiler can check pattern coverage")
(newline)

(displayln "4. COMPOSITIONAL:")
(displayln "   - Can be combined with other patterns")
(displayln "   - Works with arbitrary nesting")
(newline)

(displayln "5. FAMILIAR:")
(displayln "   - Looks like match (which everyone knows)")
(displayln "   - Easy to read and understand")
(newline)

;; ============================================================================
;; Behind the Scenes: Optics/Lenses
;; ============================================================================

(displayln "=== How dupdate Works: Optics ===\n")

(displayln "dupdate is powered by OPTICS (lenses, prisms, traversals)")
(displayln "")
(displayln "An optic is a composable getter/setter pair")
(displayln "")
(displayln "For each pattern variable in dupdate:")
(displayln "1. Create a lens focusing on that location")
(displayln "2. Compose lenses to navigate nested structure")
(displayln "3. Apply update using the lens")
(displayln "")
(displayln "Example:")
(displayln "  Pattern: (hash 'address (hash 'city c))")
(displayln "  Lens: compose-lens address-lens city-lens")
(displayln "  Update: set-lens composed-lens data \"Portland\"")
(newline)

;; ============================================================================
;; Real Implementation Sketch
;; ============================================================================

(displayln "=== Implementation Sketch ===\n")

(displayln "A real dupdate implementation would:")
(displayln "")
(displayln "1. Parse pattern to identify update locations")
(displayln "   - Pattern vars that appear in template = update points")
(displayln "")
(displayln "2. Build lenses for each update location")
(displayln "   - Hash field -> hash-lens")
(displayln "   - List index -> list-lens")
(displayln "   - Nested -> compose-lens")
(displayln "")
(displayln "3. Extract values via pattern matching")
(displayln "   - Regular match clause for getting values")
(displayln "")
(displayln "4. Apply updates via lenses")
(displayln "   - Use lens setters to rebuild structure")
(displayln "")
(displayln "5. Optimize for sharing")
(displayln "   - Only rebuild changed portions")
(displayln "   - Structural sharing for unchanged data")
