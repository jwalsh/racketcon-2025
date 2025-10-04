#lang rosette

(require rosette/lib/synthax)

;; Synthesis examples

;; 1. Synthesize a function that doubles its input
(displayln "Synthesizing a doubling function:")

(define-symbolic x integer?)

(define (double-sketch x)
  (choose* (+ x x)
           (* x 2)
           (* 2 x)))

(define double-spec
  (synthesize
    #:forall (list x)
    #:guarantee (assert (= (double-sketch x) (* 2 x)))))

(displayln double-spec)

;; 2. Synthesize constant value
(displayln "\nSynthesizing a constant that makes x + c = 10:")

(define-symbolic c integer?)
(define-symbolic input integer?)

(define const-spec
  (synthesize
    #:forall (list input)
    #:guarantee
      (assert (=> (= input 5)
                  (= (+ input c) 10)))))

(displayln const-spec)

;; 3. Synthesize comparison function
(displayln "\nSynthesizing a max function:")

(define (max-sketch a b)
  (if (choose* (> a b) (< a b) (>= a b) (<= a b))
      (choose a b)
      (choose a b)))

(define-symbolic a b integer?)

(define max-spec
  (synthesize
    #:forall (list a b)
    #:guarantee
      (assert
        (and (>= (max-sketch a b) a)
             (>= (max-sketch a b) b)
             (or (= (max-sketch a b) a)
                 (= (max-sketch a b) b))))))

(displayln max-spec)
