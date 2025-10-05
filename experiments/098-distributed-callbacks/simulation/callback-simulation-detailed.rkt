#lang roulette/example/disrupt

;; Enhanced Distributed Callback Problem Simulation
;; Shows all four possible routing scenarios explicitly

;; Two independent coin flips
(define initial-route (flip 0.5))  ; #t = Server A, #f = Server B
(define callback-route (flip 0.5)) ; #t = Server A, #f = Server B

;; All four possible scenarios
(define scenario-a-to-a (and initial-route callback-route))       ; SUCCESS
(define scenario-a-to-b (and initial-route (not callback-route))) ; FAILURE
(define scenario-b-to-a (and (not initial-route) callback-route)) ; FAILURE  
(define scenario-b-to-b (and (not initial-route) (not callback-route))) ; SUCCESS

;; Success and failure outcomes
(define success (or scenario-a-to-a scenario-b-to-b))
(define failure (or scenario-a-to-b scenario-b-to-a))

;; Display all scenarios
(displayln "╔═══════════════════════════════════════════════════════════╗")
(displayln "║  Distributed Callback Routing Scenarios                  ║")
(displayln "╚═══════════════════════════════════════════════════════════╝")
(displayln "")

(displayln "Initial Route (Server A = #t, Server B = #f):")
initial-route
(displayln "")

(displayln "Callback Route (Server A = #t, Server B = #f):")
callback-route
(displayln "")

(displayln "─────────────────────────────────────────────────────────")
(displayln "SCENARIO 1: Initial→A, Callback→A [SUCCESS]")
scenario-a-to-a
(displayln "")

(displayln "SCENARIO 2: Initial→A, Callback→B [FAILURE]")
scenario-a-to-b
(displayln "")

(displayln "SCENARIO 3: Initial→B, Callback→A [FAILURE]")
scenario-b-to-a
(displayln "")

(displayln "SCENARIO 4: Initial→B, Callback→B [SUCCESS]")
scenario-b-to-b
(displayln "")

(displayln "─────────────────────────────────────────────────────────")
(displayln "OVERALL SUCCESS (routes match):")
success
(displayln "")

(displayln "OVERALL FAILURE (routes don't match):")
failure
(displayln "")

(displayln "═══════════════════════════════════════════════════════════")
(displayln "Expected: Success = 50%, Failure = 50%")
(displayln "═══════════════════════════════════════════════════════════")
