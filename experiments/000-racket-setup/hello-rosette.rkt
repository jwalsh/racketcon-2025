#lang rosette

(require rosette/lib/synthax)

; Simple verification example
(define-symbolic x integer?)
(define-symbolic y integer?)

; Verify that x + y = y + x (commutativity)
(verify
 (assert (= (+ x y) (+ y x))))

(displayln "Commutativity verified!")
