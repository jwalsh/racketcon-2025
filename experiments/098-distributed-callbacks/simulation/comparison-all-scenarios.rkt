#lang roulette/example/disrupt

;; Comprehensive Comparison of All Deployment Scenarios
;; Shows side-by-side probability distributions

(displayln "╔═══════════════════════════════════════════════════════════╗")
(displayln "║  Distributed Callback Problem - All Scenarios            ║")
(displayln "╚═══════════════════════════════════════════════════════════╝")
(displayln "")

;; ═══════════════════════════════════════════════════════════
;; Scenario 1: Blue/Green (50/50)
;; ═══════════════════════════════════════════════════════════
(displayln "┌─────────────────────────────────────────────────────────┐")
(displayln "│ SCENARIO 1: Blue/Green Deployment (50/50)              │")
(displayln "└─────────────────────────────────────────────────────────┘")

(define bg-initial (flip 0.5))
(define bg-callback (flip 0.5))
(define bg-success (equal? bg-initial bg-callback))

(displayln "Initial route (Blue=#t, Green=#f):")
bg-initial
(displayln "")
(displayln "Callback route (Blue=#t, Green=#f):")
bg-callback
(displayln "")
(displayln "SUCCESS (same server):")
bg-success
(displayln "")
(displayln "Expected: 50% success, 50% failure")
(displayln "")

;; ═══════════════════════════════════════════════════════════
;; Scenario 2: Stable/Canary (90/10)
;; ═══════════════════════════════════════════════════════════
(displayln "┌─────────────────────────────────────────────────────────┐")
(displayln "│ SCENARIO 2: Stable/Canary Deployment (90/10)           │")
(displayln "└─────────────────────────────────────────────────────────┘")

(define sc-initial (flip 0.9))
(define sc-callback (flip 0.9))
(define sc-success (equal? sc-initial sc-callback))

(displayln "Initial route (Stable=#t, Canary=#f):")
sc-initial
(displayln "")
(displayln "Callback route (Stable=#t, Canary=#f):")
sc-callback
(displayln "")
(displayln "SUCCESS (same environment):")
sc-success
(displayln "")
(displayln "Expected: 82% success, 18% failure")
(displayln "  - Both Stable: 81%")
(displayln "  - Both Canary: 1%")
(displayln "")

;; ═══════════════════════════════════════════════════════════
;; Scenario 3: Load Balancer (10 servers)
;; ═══════════════════════════════════════════════════════════
(displayln "┌─────────────────────────────────────────────────────────┐")
(displayln "│ SCENARIO 3: Load Balancer (10 Servers)                 │")
(displayln "└─────────────────────────────────────────────────────────┘")

(define lb-initial (categorical '(0 1 2 3 4 5 6 7 8 9)))
(define lb-callback (categorical '(0 1 2 3 4 5 6 7 8 9)))
(define lb-success (equal? lb-initial lb-callback))

(displayln "Initial server (0-9):")
lb-initial
(displayln "")
(displayln "Callback server (0-9):")
lb-callback
(displayln "")
(displayln "SUCCESS (same server):")
lb-success
(displayln "")
(displayln "Expected: 10% success, 90% failure")
(displayln "")

;; ═══════════════════════════════════════════════════════════
;; Summary Table
;; ═══════════════════════════════════════════════════════════
(displayln "═══════════════════════════════════════════════════════════")
(displayln "SUMMARY")
(displayln "═══════════════════════════════════════════════════════════")
(displayln "")
(displayln "Deployment     | Servers | Split    | Success | Failure")
(displayln "───────────────┼─────────┼──────────┼─────────┼────────")
(displayln "Blue/Green     |    2    | 50/50    |   50%   |   50%")
(displayln "Stable/Canary  |    2    | 90/10    |   82%   |   18%")
(displayln "Load Balancer  |   10    | 10% each |   10%   |   90%")
(displayln "Large Cluster  |  100    |  1% each |    1%   |   99%")
(displayln "")
(displayln "═══════════════════════════════════════════════════════════")
(displayln "KEY INSIGHT: More servers = MORE failures!")
(displayln "═══════════════════════════════════════════════════════════")
