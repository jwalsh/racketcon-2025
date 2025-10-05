#lang roulette/example/disrupt

;; Stable/Canary Deployment (90/10 split)
;; Models callback routing with unbalanced traffic distribution

;; 90% probability routes to stable, 10% to canary
(define initial-routes-to-stable (flip 0.9))
(define callback-routes-to-stable (flip 0.9))

;; Success when both route to same environment
(define both-stable (and initial-routes-to-stable callback-routes-to-stable))
(define both-canary (and (not initial-routes-to-stable) 
                          (not callback-routes-to-stable)))

(define success (or both-stable both-canary))
(define failure (not success))

;; Individual failure scenarios
(define stable-to-canary (and initial-routes-to-stable 
                               (not callback-routes-to-stable)))
(define canary-to-stable (and (not initial-routes-to-stable)
                               callback-routes-to-stable))

(displayln "╔═══════════════════════════════════════════════════════════╗")
(displayln "║  Stable/Canary Deployment (90/10 Split)                  ║")
(displayln "╚═══════════════════════════════════════════════════════════╝")
(displayln "")

(displayln "Initial routes to Stable (90%):")
initial-routes-to-stable
(displayln "")

(displayln "Callback routes to Stable (90%):")
callback-routes-to-stable
(displayln "")

(displayln "─────────────────────────────────────────────────────────")
(displayln "Both route to Stable (SUCCESS):")
both-stable
(displayln "")

(displayln "Both route to Canary (SUCCESS):")
both-canary
(displayln "")

(displayln "Stable → Canary (FAILURE):")
stable-to-canary
(displayln "")

(displayln "Canary → Stable (FAILURE):")
canary-to-stable
(displayln "")

(displayln "─────────────────────────────────────────────────────────")
(displayln "Overall SUCCESS (routes match):")
success
(displayln "")

(displayln "Overall FAILURE (routes don't match):")
failure
(displayln "")

(displayln "═══════════════════════════════════════════════════════════")
(displayln "Expected: Success = 82%, Failure = 18%")
(displayln "Breakdown:")
(displayln "  Both Stable:  90% × 90% = 81%")
(displayln "  Both Canary:  10% × 10% =  1%")
(displayln "  Stable→Canary: 90% × 10% = 9%")
(displayln "  Canary→Stable: 10% × 90% = 9%")
(displayln "═══════════════════════════════════════════════════════════")
