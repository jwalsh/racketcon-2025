#lang racket

;; Experiment 049: Ocular-Patdown Optics Guide
;; Part 4: Isomorphisms - Bidirectional Conversions
;;
;; Based on: https://docs.racket-lang.org/ocular-patdown/optics-guide.html

(require ocular-patdown)
(require racket/string)

;; ============================================================================
;; What are Isomorphisms?
;; ============================================================================

;; An isomorphism is a bidirectional conversion between two equivalent
;; representations. It always succeeds in both directions.

(displayln "=== What are Isomorphisms? ===")
(displayln "An isomorphism (iso) converts between equivalent representations")
(displayln "Requirements:")
(displayln "  - Bidirectional: A <-> B")
(displayln "  - Lossless: from(to(x)) = x and to(from(y)) = y")
(displayln "  - Always succeeds")
(newline)

;; ============================================================================
;; Creating Isomorphisms
;; ============================================================================

;; String <-> List of characters
(define string-chars-iso
  (iso string->list list->string))

(displayln "=== String <-> List Isomorphism ===")
(define text "hello")
(displayln (format "Original: ~a" text))

;; View through the iso: string -> list
(define as-chars (lens-view string-chars-iso text))
(displayln (format "As chars: ~a" as-chars))

;; Transform through the iso
(define uppercased
  (lens-transform string-chars-iso
                  (λ (chars) (map char-upcase chars))
                  text))
(displayln (format "Uppercased: ~a" uppercased))
(newline)

;; ============================================================================
;; Numeric Isomorphisms
;; ============================================================================

;; Celsius <-> Fahrenheit
(define (celsius->fahrenheit c)
  (+ (* c 9/5) 32))

(define (fahrenheit->celsius f)
  (* (- f 32) 5/9))

(define celsius-fahrenheit-iso
  (iso celsius->fahrenheit fahrenheit->celsius))

(displayln "=== Temperature Isomorphism ===")
(define temp-c 20)
(displayln (format "~a°C = ~a°F"
                   temp-c
                   (lens-view celsius-fahrenheit-iso temp-c)))

;; Transform in Fahrenheit space
(define warmer-c
  (lens-transform celsius-fahrenheit-iso
                  (λ (f) (+ f 10))  ; Add 10°F
                  temp-c))
(displayln (format "Add 10°F: ~a°C -> ~a°C" temp-c warmer-c))
(newline)

;; ============================================================================
;; Pair Isomorphisms
;; ============================================================================

;; Swap components of a pair
(define (swap p) (cons (cdr p) (car p)))

(define pair-swap-iso
  (iso swap swap))  ; Swap is its own inverse

(displayln "=== Pair Swap Isomorphism ===")
(define coordinate (cons 3 4))
(displayln (format "Original: ~a" coordinate))
(displayln (format "Swapped: ~a" (lens-view pair-swap-iso coordinate)))
(newline)

;; ============================================================================
;; List Isomorphisms
;; ============================================================================

;; Reverse a list
(define reverse-iso
  (iso reverse reverse))  ; Reverse is its own inverse

(displayln "=== List Reverse Isomorphism ===")
(define lst '(1 2 3 4 5))
(displayln (format "Original: ~a" lst))
(displayln (format "Reversed: ~a" (lens-view reverse-iso lst)))

;; Transform in reversed space
(define modified
  (lens-transform reverse-iso
                  (λ (rev) (cons 0 rev))  ; Add 0 at "beginning" of reversed list
                  lst))
(displayln (format "Add 0 at end: ~a" modified))
(newline)

;; ============================================================================
;; Struct <-> List Isomorphism
;; ============================================================================

(struct point (x y) #:transparent)

(define point-list-iso
  (iso (λ (p) (list (point-x p) (point-y p)))
       (λ (lst) (point (first lst) (second lst)))))

(displayln "=== Point <-> List Isomorphism ===")
(define pt (point 3 4))
(displayln (format "Point: ~a" pt))
(displayln (format "As list: ~a" (lens-view point-list-iso pt)))

;; Transform via list representation
(define scaled
  (lens-transform point-list-iso
                  (λ (lst) (map (λ (x) (* x 2)) lst))
                  pt))
(displayln (format "Scaled 2x: ~a" scaled))
(newline)

;; ============================================================================
;; Composing Isomorphisms
;; ============================================================================

;; Symbol <-> String <-> List of chars
(define symbol-string-iso
  (iso symbol->string string->symbol))

;; Compose: symbol -> string -> list of chars
(define symbol-chars-iso
  (lens-compose symbol-string-iso string-chars-iso))

(displayln "=== Composed Isomorphisms ===")
(define sym 'hello)
(displayln (format "Symbol: ~a" sym))
(displayln (format "As chars: ~a" (lens-view symbol-chars-iso sym)))

;; Transform through multiple representations
(define reversed-sym
  (lens-transform symbol-chars-iso reverse sym))
(displayln (format "Reversed: ~a" reversed-sym))
(newline)

;; ============================================================================
;; Isomorphisms in Data Transformations
;; ============================================================================

;; Rectangle: two representations
;; 1. Top-left corner + bottom-right corner
;; 2. Top-left corner + width + height

(struct rect-corners (tl br) #:transparent)
(struct rect-dims (tl width height) #:transparent)

(define (corners->dims rc)
  (let ([x1 (car (rect-corners-tl rc))]
        [y1 (cdr (rect-corners-tl rc))]
        [x2 (car (rect-corners-br rc))]
        [y2 (cdr (rect-corners-br rc))])
    (rect-dims (rect-corners-tl rc)
               (- x2 x1)
               (- y2 y1))))

(define (dims->corners rd)
  (let ([x (car (rect-dims-tl rd))]
        [y (cdr (rect-dims-tl rd))]
        [w (rect-dims-width rd)]
        [h (rect-dims-height rd)])
    (rect-corners (rect-dims-tl rd)
                  (cons (+ x w) (+ y h)))))

(define rect-representation-iso
  (iso corners->dims dims->corners))

(displayln "=== Rectangle Representation Isomorphism ===")
(define r (rect-corners (cons 0 0) (cons 10 10)))
(displayln (format "Corners: ~a" r))
(displayln (format "Dims: ~a" (lens-view rect-representation-iso r)))

;; Work in dimensions space
(define scaled-rect
  (lens-transform rect-representation-iso
                  (λ (rd) (struct-copy rect-dims rd
                                       [width (* (rect-dims-width rd) 2)]
                                       [height (* (rect-dims-height rd) 2)]))
                  r))
(displayln (format "Scaled 2x: ~a" scaled-rect))
(newline)

;; ============================================================================
;; Isomorphism Laws
;; ============================================================================

(displayln "=== Isomorphism Laws ===")
(displayln "Law 1: from(to(x)) = x (round-trip preserves value)")
(displayln "Law 2: to(from(y)) = y (reverse round-trip preserves value)")
(newline)

;; Verify for string-chars-iso
(define original-str "test")
(define round-trip
  (list->string (string->list original-str)))
(displayln (format "Round-trip test: \"~a\" = \"~a\"? ~a"
                   original-str round-trip (equal? original-str round-trip)))
(newline)

;; ============================================================================
;; Isomorphisms vs Lenses
;; ============================================================================

(displayln "=== Isomorphisms vs Lenses ===")
(displayln "Lens:        Focuses on a PART of a structure")
(displayln "Isomorphism: Converts BETWEEN equivalent representations")
(displayln "")
(displayln "Both are optics, both compose, but:")
(displayln "- Lens: whole -> part -> whole")
(displayln "- Iso:  repr-A <-> repr-B")
(newline)

;; ============================================================================
;; Summary
;; ============================================================================

(displayln "=== Summary: Isomorphisms ===")
(displayln "1. Bidirectional conversions between equivalent types")
(displayln "2. Always succeed (lossless)")
(displayln "3. Laws: from(to(x)) = x and to(from(y)) = y")
(displayln "4. Use cases: representation changes, unit conversions")
(displayln "5. Compose with other optics")
(displayln "")
(displayln "Next: 05-composition.rkt - Combining optics")
