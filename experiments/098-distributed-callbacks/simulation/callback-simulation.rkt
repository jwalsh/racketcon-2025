#lang roulette/example/disrupt

;; Distributed Callback Problem Simulation
;; Models the probability of successful callback routing in a load-balanced system

;; Simulate initial server routing (50/50 split)
;; #t = routes to Server A, #f = routes to Server B
(define routes-to-server-a (flip 0.5))

;; Simulate callback routing (independent 50/50 split)
;; #t = callback routes to Server A, #f = callback routes to Server B
(define callback-routes-to-server-a (flip 0.5))

;; Success only when both route to same server
;; Case 1: Both route to Server A
(define both-route-to-a (and routes-to-server-a callback-routes-to-server-a))

;; Case 2: Both route to Server B (both are #f)
(define both-route-to-b (and (not routes-to-server-a) 
                               (not callback-routes-to-server-a)))

;; Overall success is when either case succeeds
(define callback-succeeds (or both-route-to-a both-route-to-b))

;; The failure case - callback hits wrong server
(define callback-fails (not callback-succeeds))

;; Display results
(displayln "=== Distributed Callback Simulation ===")
(displayln "")
(displayln "Initial routing to Server A:")
routes-to-server-a
(displayln "")
(displayln "Callback routing to Server A:")
callback-routes-to-server-a
(displayln "")
(displayln "Both route to Server A (success case 1):")
both-route-to-a
(displayln "")
(displayln "Both route to Server B (success case 2):")
both-route-to-b
(displayln "")
(displayln "Callback succeeds (overall):")
callback-succeeds
(displayln "")
(displayln "Callback fails (wrong server):")
callback-fails
