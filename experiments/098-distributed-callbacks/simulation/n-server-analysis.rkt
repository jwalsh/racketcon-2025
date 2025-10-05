#lang roulette/example/disrupt

;; Generalized analysis for N servers
;; Shows how failure rate scales with number of servers

(define (analyze-n-servers n)
  (displayln (format "═══ ~a Server(s) ═══" n))
  (displayln (format "  Success probability: 1/~a = ~a%" 
                     n 
                     (real->decimal-string (exact->inexact (/ 100 n)) 2)))
  (displayln (format "  Failure probability: ~a/~a = ~a%" 
                     (- n 1) 
                     n 
                     (real->decimal-string (exact->inexact (* 100 (/ (- n 1) n))) 2)))
  (displayln ""))

(displayln "╔═══════════════════════════════════════════════════════════╗")
(displayln "║  N-Server Failure Rate Analysis                          ║")
(displayln "╚═══════════════════════════════════════════════════════════╝")
(displayln "")

(displayln "As you add more servers, the callback problem gets WORSE:")
(displayln "")

(analyze-n-servers 2)    ;; Blue/Green or A/B (50/50)
(analyze-n-servers 3)
(analyze-n-servers 5)
(analyze-n-servers 10)   ;; Typical load balancer
(analyze-n-servers 20)
(analyze-n-servers 50)
(analyze-n-servers 100)  ;; Large cluster

(displayln "═══════════════════════════════════════════════════════════")
(displayln "Key Insight: Horizontal scaling INCREASES failure rate!")
(displayln "")
(displayln "Formula: Failure Rate = (N-1)/N")
(displayln "         Success Rate = 1/N")
(displayln "")
(displayln "As N → ∞, Failure Rate → 100%")
(displayln "═══════════════════════════════════════════════════════════")

;; Now let's simulate a few specific cases to verify
(displayln "")
(displayln "╔═══════════════════════════════════════════════════════════╗")
(displayln "║  Verification with Roulette Simulations                  ║")
(displayln "╚═══════════════════════════════════════════════════════════╝")
(displayln "")

;; 2-server simulation
(displayln "2 Servers (50/50):")
(define s2-initial (flip 0.5))
(define s2-callback (flip 0.5))
(define s2-match (equal? s2-initial s2-callback))
s2-match
(displayln "")

;; 3-server simulation
(displayln "3 Servers:")
(define s3-initial (categorical '(0 1 2)))
(define s3-callback (categorical '(0 1 2)))
(define s3-match (equal? s3-initial s3-callback))
s3-match
(displayln "")

;; 5-server simulation
(displayln "5 Servers:")
(define s5-initial (categorical '(0 1 2 3 4)))
(define s5-callback (categorical '(0 1 2 3 4)))
(define s5-match (equal? s5-initial s5-callback))
s5-match
(displayln "")
