#lang racket

;; Rectangle Isomorphisms: Different Representations, Same Idea
;; Demonstrates how width/height and position are equivalent views

(provide (all-defined-out))

;; ============================================================================
;; TWO RECTANGLE REPRESENTATIONS
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║         RECTANGLE REPRESENTATIONS: ISOMORPHISMS              ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

;; Representation 1: Top-left corner + width + height
(struct rect-wh (x y width height) #:transparent)

;; Representation 2: Top-left corner + bottom-right corner
(struct rect-corners (x1 y1 x2 y2) #:transparent)

(displayln "Two ways to represent a rectangle:")
(newline)

(displayln "Representation 1: Top-left + Width/Height")
(displayln "  (rect-wh x y width height)")
(displayln "  Example: (rect-wh 10 20 50 30)")
(displayln "  → Rectangle at (10, 20) with width=50, height=30")
(newline)

(displayln "Representation 2: Top-left + Bottom-right")
(displayln "  (rect-corners x1 y1 x2 y2)")
(displayln "  Example: (rect-corners 10 20 60 50)")
(displayln "  → Rectangle from (10, 20) to (60, 50)")
(newline)

(displayln "These represent THE SAME rectangle!")
(newline)

;; ============================================================================
;; ISOMORPHISM: Bidirectional Conversion
;; ============================================================================

(displayln "═══ ISOMORPHISM: wh ↔ corners ═══")
(newline)

(define (wh->corners r)
  "Convert width/height to corners representation."
  (rect-corners
    (rect-wh-x r)
    (rect-wh-y r)
    (+ (rect-wh-x r) (rect-wh-width r))
    (+ (rect-wh-y r) (rect-wh-height r))))

(define (corners->wh r)
  "Convert corners to width/height representation."
  (rect-wh
    (rect-corners-x1 r)
    (rect-corners-y1 r)
    (- (rect-corners-x2 r) (rect-corners-x1 r))
    (- (rect-corners-y2 r) (rect-corners-y1 r))))

(displayln "Conversion functions:")
(displayln "  wh->corners: rect-wh → rect-corners")
(displayln "  corners->wh: rect-corners → rect-wh")
(newline)

;; Example
(define r1 (rect-wh 10 20 50 30))
(define r2 (wh->corners r1))
(define r3 (corners->wh r2))

(displayln "Example conversion:")
(displayln (format "  Start:     ~a" r1))
(displayln (format "  To corners: ~a" r2))
(displayln (format "  Back to wh: ~a" r3))
(displayln (format "  Round-trip works? ~a" (equal? r1 r3)))
(newline)

;; ============================================================================
;; ISO STRUCT: Formal Isomorphism
;; ============================================================================

(displayln "═══ ISO: Formal Isomorphism ═══")
(newline)

(struct iso (to from) #:transparent)

(define rect-iso
  (iso wh->corners corners->wh))

(displayln "An ISO packages both directions:")
(displayln "  (struct iso (to from) #:transparent)")
(displayln "")
(displayln "  rect-iso = iso wh->corners corners->wh")
(newline)

(displayln "Properties of isomorphisms:")
(displayln "  • to ∘ from = identity")
(displayln "  • from ∘ to = identity")
(displayln "  • Lossless in both directions")
(newline)

;; Verification
(define test-wh (rect-wh 5 10 20 15))
(define test-corners ((iso-to rect-iso) test-wh))
(define back-to-wh ((iso-from rect-iso) test-corners))

(displayln "Verification:")
(displayln (format "  Original:   ~a" test-wh))
(displayln (format "  Converted:  ~a" test-corners))
(displayln (format "  Back again: ~a" back-to-wh))
(displayln (format "  Equal? ~a ✓" (equal? test-wh back-to-wh)))
(newline)

;; ============================================================================
;; MOVING RECTANGLES: Using Different Representations
;; ============================================================================

(displayln "═══ MOVING RECTANGLES ═══")
(newline)

(displayln "Same operation, different representations:")
(newline)

;; Move in width/height representation
(define (move-wh rect dx dy)
  "Move rectangle by (dx, dy) in wh representation."
  (rect-wh
    (+ (rect-wh-x rect) dx)
    (+ (rect-wh-y rect) dy)
    (rect-wh-width rect)
    (rect-wh-height rect)))

;; Move in corners representation
(define (move-corners rect dx dy)
  "Move rectangle by (dx, dy) in corners representation."
  (rect-corners
    (+ (rect-corners-x1 rect) dx)
    (+ (rect-corners-y1 rect) dy)
    (+ (rect-corners-x2 rect) dx)
    (+ (rect-corners-y2 rect) dy)))

(define original-wh (rect-wh 10 20 50 30))

(displayln "Method 1: Move in wh representation")
(displayln (format "  Before: ~a" original-wh))
(define moved-wh (move-wh original-wh 5 -3))
(displayln (format "  After:  ~a" moved-wh))
(newline)

(displayln "Method 2: Convert to corners, move, convert back")
(displayln (format "  Before: ~a" original-wh))
(define as-corners (wh->corners original-wh))
(displayln (format "  As corners: ~a" as-corners))
(define moved-corners (move-corners as-corners 5 -3))
(displayln (format "  Moved corners: ~a" moved-corners))
(define back-to-wh-moved (corners->wh moved-corners))
(displayln (format "  Back to wh: ~a" back-to-wh-moved))
(newline)

(displayln "Are they equal?")
(displayln (format "  ~a ✓" (equal? moved-wh back-to-wh-moved)))
(newline)

;; ============================================================================
;; LENSES FOR EACH REPRESENTATION
;; ============================================================================

(displayln "═══ LENSES FOR EACH REPRESENTATION ═══")
(newline)

(struct lens (getter setter) #:transparent)

(define (view l target)
  ((lens-getter l) target))

(define (set l target value)
  ((lens-setter l) target value))

;; Lenses for width/height representation
(define x-wh-lens
  (lens
    rect-wh-x
    (λ (r v) (struct-copy rect-wh r [x v]))))

(define y-wh-lens
  (lens
    rect-wh-y
    (λ (r v) (struct-copy rect-wh r [y v]))))

;; Lenses for corners representation
(define x1-corners-lens
  (lens
    rect-corners-x1
    (λ (r v) (struct-copy rect-corners r [x1 v]))))

(define y1-corners-lens
  (lens
    rect-corners-y1
    (λ (r v) (struct-copy rect-corners r [y1 v]))))

(displayln "Lenses for wh representation:")
(displayln "  x-wh-lens, y-wh-lens")
(newline)

(displayln "Lenses for corners representation:")
(displayln "  x1-corners-lens, y1-corners-lens")
(newline)

(define demo-rect (rect-wh 100 200 50 30))

(displayln "Example: Move x by 10 using lens")
(displayln (format "  Before: ~a" demo-rect))
(displayln (format "  After:  ~a" (set x-wh-lens demo-rect 110)))
(newline)

;; ============================================================================
;; ISO-LENS: Viewing Through an Isomorphism
;; ============================================================================

(displayln "═══ ISO-LENS: View Through Isomorphism ═══")
(newline)

(define (iso-lens iso lens)
  "Create a lens that views through an isomorphism.
   This lets you use a lens for one representation on another."
  (struct-copy lens lens
    [getter (λ (target)
              (view lens ((iso-to iso) target)))]
    [setter (λ (target value)
              ((iso-from iso)
               (set lens ((iso-to iso) target) value)))]))

(displayln "iso-lens: Use a corners-lens on a wh-rectangle!")
(newline)

;; Create a lens that accesses x2 (bottom-right x) from wh representation
(define x2-via-iso-lens
  (iso-lens
    rect-iso
    (lens
      rect-corners-x2
      (λ (r v) (struct-copy rect-corners r [x2 v])))))

(define wh-rect (rect-wh 10 20 50 30))

(displayln "View x2 (bottom-right x) from wh representation:")
(displayln (format "  Rectangle: ~a" wh-rect))
(displayln (format "  x2 value:  ~a" (view x2-via-iso-lens wh-rect)))
(displayln "  (x2 = x + width = 10 + 50 = 60)")
(newline)

(displayln "Set x2 to 70 (changes width!):")
(define updated-rect (set x2-via-iso-lens wh-rect 70))
(displayln (format "  Result: ~a" updated-rect))
(displayln "  (new width = x2 - x = 70 - 10 = 60)")
(newline)

;; ============================================================================
;; PRACTICAL EXAMPLE: Area and Perimeter
;; ============================================================================

(displayln "═══ PRACTICAL EXAMPLE: Same Operations, Any Representation ═══")
(newline)

(define (area-wh r)
  (* (rect-wh-width r) (rect-wh-height r)))

(define (area-corners r)
  (* (- (rect-corners-x2 r) (rect-corners-x1 r))
     (- (rect-corners-y2 r) (rect-corners-y1 r))))

(define (perimeter-wh r)
  (* 2 (+ (rect-wh-width r) (rect-wh-height r))))

(define (perimeter-corners r)
  (* 2 (+ (- (rect-corners-x2 r) (rect-corners-x1 r))
          (- (rect-corners-y2 r) (rect-corners-y1 r)))))

(define rect-a (rect-wh 10 20 50 30))
(define rect-b (wh->corners rect-a))

(displayln "Same rectangle, two representations:")
(displayln (format "  WH:      ~a" rect-a))
(displayln (format "  Corners: ~a" rect-b))
(newline)

(displayln "Area (should be identical):")
(displayln (format "  area-wh:      ~a" (area-wh rect-a)))
(displayln (format "  area-corners: ~a" (area-corners rect-b)))
(newline)

(displayln "Perimeter (should be identical):")
(displayln (format "  perimeter-wh:      ~a" (perimeter-wh rect-a)))
(displayln (format "  perimeter-corners: ~a" (perimeter-corners rect-b)))
(newline)

;; ============================================================================
;; SUMMARY
;; ============================================================================

(displayln "╔═══════════════════════════════════════════════════════════════╗")
(displayln "║                          SUMMARY                             ║")
(displayln "╚═══════════════════════════════════════════════════════════════╝")
(newline)

(displayln "KEY INSIGHTS:")
(newline)

(displayln "1. ISOMORPHIC REPRESENTATIONS")
(displayln "   • rect-wh and rect-corners represent the SAME idea")
(displayln "   • Can convert between them losslessly")
(displayln "   • Both have their advantages")
(newline)

(displayln "2. ISOMORPHISM STRUCTURE")
(displayln "   • (struct iso (to from))")
(displayln "   • Laws: to ∘ from = id, from ∘ to = id")
(displayln "   • Bidirectional, lossless conversion")
(newline)

(displayln "3. ISO-LENS")
(displayln "   • View one representation through another's lens")
(displayln "   • Compose isomorphisms with lenses")
(displayln "   • Update width by changing x2!")
(newline)

(displayln "4. REPRESENTATION CHOICE")
(displayln "   • rect-wh: Good for size operations")
(displayln "   • rect-corners: Good for bounds checking")
(displayln "   • Convert as needed with iso")
(newline)

(displayln "ANALOGY:")
(displayln "  Celsius ↔ Fahrenheit  (same temperature, different scale)")
(displayln "  WH ↔ Corners          (same rectangle, different coords)")
(newline)

(displayln "═══════════════════════════════════════════════════════════════")
(displayln (format "Racket version: ~a" (version)))
(displayln "RacketCon 2025 - Experiment 045: Rectangle Isomorphisms")
(displayln "═══════════════════════════════════════════════════════════════")
