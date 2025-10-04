#lang racket

;; Experiment 049: Ocular-Patdown Optics Guide
;; Part 2: Lenses - Single Focus Optics
;;
;; Based on: https://docs.racket-lang.org/ocular-patdown/optics-guide.html

(require racket/struct)
(require ocular-patdown)

;; ============================================================================
;; What are Lenses?
;; ============================================================================

;; A lens focuses on exactly ONE subpart of a data structure
;; Operations: view, set, transform

(displayln "=== What are Lenses? ===")
(displayln "A lens is an optic that focuses on exactly one subpart")
(displayln "It guarantees: always exactly 1 focus point")
(newline)

;; ============================================================================
;; Creating Lenses
;; ============================================================================

(struct point (x y) #:transparent)

;; Method 1: Using lens constructor
(define point-x-lens
  (lens point-x                           ; getter
        (λ (p v) (struct-copy point p [x v]))))  ; setter

(define point-y-lens
  (lens point-y
        (λ (p v) (struct-copy point p [y v]))))

(define origin (point 0 0))
(define p1 (point 3 4))

(displayln "=== Creating Lenses ===")
(displayln (format "Origin: ~a" origin))
(displayln (format "Point: ~a" p1))
(newline)

;; ============================================================================
;; Lens Operations
;; ============================================================================

(displayln "=== Lens Operations ===")

;; View: Extract the focused value
(displayln (format "View x: ~a" (lens-view point-x-lens p1)))
(displayln (format "View y: ~a" (lens-view point-y-lens p1)))

;; Set: Replace the focused value
(define p2 (lens-set point-x-lens p1 10))
(displayln (format "Set x=10: ~a" p2))

;; Transform: Apply function to focused value
(define p3 (lens-transform point-y-lens (λ (y) (* y 2)) p1))
(displayln (format "Transform y*2: ~a" p3))
(newline)

;; ============================================================================
;; Lens Laws
;; ============================================================================

(displayln "=== Lens Laws ===")

;; Law 1: GetPut - get(set(target, value)) = value
(define test-val 42)
(define after-set (lens-set point-x-lens p1 test-val))
(define got-val (lens-view point-x-lens after-set))
(displayln (format "GetPut: ~a = ~a? ~a" got-val test-val (= got-val test-val)))

;; Law 2: PutGet - set(target, get(target)) = target
(define original-x (lens-view point-x-lens p1))
(define put-back (lens-set point-x-lens p1 original-x))
(displayln (format "PutGet: ~a = ~a? ~a" p1 put-back (equal? p1 put-back)))

;; Law 3: PutPut - set(set(target, v1), v2) = set(target, v2)
(define set-twice (lens-set point-x-lens (lens-set point-x-lens p1 5) 7))
(define set-once (lens-set point-x-lens p1 7))
(displayln (format "PutPut: ~a = ~a? ~a" set-twice set-once (equal? set-twice set-once)))
(newline)

;; ============================================================================
;; Composing Lenses
;; ============================================================================

(struct rect (top-left bottom-right) #:transparent)

(define rect-tl-lens
  (lens rect-top-left (λ (r v) (struct-copy rect r [top-left v]))))

(define rect-br-lens
  (lens rect-bottom-right (λ (r v) (struct-copy rect r [bottom-right v]))))

;; Compose to reach nested fields
(define rect-tl-x-lens (lens-compose rect-tl-lens point-x-lens))
(define rect-tl-y-lens (lens-compose rect-tl-lens point-y-lens))

(define r1 (rect (point 0 0) (point 10 10)))

(displayln "=== Composing Lenses ===")
(displayln (format "Rectangle: ~a" r1))
(displayln (format "View top-left x: ~a" (lens-view rect-tl-x-lens r1)))
(displayln (format "Set top-left x=5: ~a" (lens-set rect-tl-x-lens r1 5)))
(newline)

;; ============================================================================
;; Lenses for Collections
;; ============================================================================

(displayln "=== Lenses for Collections ===")

;; List index lens
(define lst '(10 20 30 40))
(displayln (format "List: ~a" lst))
(displayln (format "View index 2: ~a" (lens-view (list-ref-lens 2) lst)))
(displayln (format "Set index 1=99: ~a" (lens-set (list-ref-lens 1) lst 99)))

;; Vector index lens
(define vec (vector 'a 'b 'c 'd))
(displayln (format "Vector: ~a" vec))
(displayln (format "View index 0: ~a" (lens-view (vector-ref-lens 0) vec)))
(displayln (format "Set index 2='z: ~a" (lens-set (vector-ref-lens 2) vec 'z)))

;; Hash key lens
(define ht (hash 'name "Alice" 'age 30))
(displayln (format "Hash: ~a" ht))
(displayln (format "View 'name: ~a" (lens-view (hash-ref-lens 'name) ht)))
(displayln (format "Set 'age=31: ~a" (lens-set (hash-ref-lens 'age) ht 31)))
(newline)

;; ============================================================================
;; Practical Example: Configuration Updates
;; ============================================================================

(struct config (server database cache) #:transparent)
(struct server-config (host port) #:transparent)
(struct db-config (connection-string pool-size) #:transparent)
(struct cache-config (enabled ttl) #:transparent)

(define app-config
  (config
   (server-config "localhost" 8080)
   (db-config "postgres://..." 10)
   (cache-config #t 3600)))

;; Define lenses
(define config-server-lens
  (lens config-server (λ (c v) (struct-copy config c [server v]))))

(define server-port-lens
  (lens server-config-port (λ (s v) (struct-copy server-config s [port v]))))

;; Compose to reach nested port
(define config-port-lens (lens-compose config-server-lens server-port-lens))

(displayln "=== Practical Example: Configuration ===")
(displayln (format "Original config: ~a" app-config))
(displayln (format "Current port: ~a" (lens-view config-port-lens app-config)))
(displayln (format "Update port to 9000: ~a"
                   (lens-set config-port-lens app-config 9000)))
(newline)

;; ============================================================================
;; Summary
;; ============================================================================

(displayln "=== Summary: Lenses ===")
(displayln "1. Lenses focus on exactly ONE subpart")
(displayln "2. Operations: view, set, transform")
(displayln "3. Laws: GetPut, PutGet, PutPut")
(displayln "4. Compose with lens-compose")
(displayln "5. Work with structs, lists, vectors, hashes")
(displayln "")
(displayln "Next: 03-traversals.rkt - Multiple foci optics")
