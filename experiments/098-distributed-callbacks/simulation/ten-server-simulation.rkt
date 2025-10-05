#lang roulette/example/disrupt

;; 10 Server Load Balancer Simulation
;; Each server gets 10% of traffic
;; Demonstrates how failure rate scales with number of servers

;; Use categorical distribution for 10 servers
;; Returns a number 0-9 representing which server
(define (pick-server)
  (categorical '(0 1 2 3 4 5 6 7 8 9)))

;; Initial request picks a server
(define initial-server (pick-server))

;; Callback independently picks a server
(define callback-server (pick-server))

;; Success only when same server is picked
(define same-server (equal? initial-server callback-server))

;; Failure when different servers
(define different-server (not same-server))

(displayln "╔═══════════════════════════════════════════════════════════╗")
(displayln "║  10 Server Load Balancer Simulation                      ║")
(displayln "╚═══════════════════════════════════════════════════════════╝")
(displayln "")

(displayln "Initial request routed to server:")
initial-server
(displayln "")

(displayln "Callback routed to server:")
callback-server
(displayln "")

(displayln "─────────────────────────────────────────────────────────")
(displayln "Routes to SAME server (SUCCESS):")
same-server
(displayln "")

(displayln "Routes to DIFFERENT server (FAILURE):")
different-server
(displayln "")

(displayln "═══════════════════════════════════════════════════════════")
(displayln "Expected Results:")
(displayln "  Success = 10% (1/10)")
(displayln "  Failure = 90% (9/10)")
(displayln "")
(displayln "Why: With 10 servers, there's only a 1 in 10 chance")
(displayln "     the callback randomly hits the same server.")
(displayln "═══════════════════════════════════════════════════════════")
