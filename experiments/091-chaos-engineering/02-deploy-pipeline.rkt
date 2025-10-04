#lang roulette

;; Deployment Pipeline Failure Models
;; Blue/Green, Canary, Rolling Updates with probabilistic analysis

(require racket/format)
(provide (all-defined-out))

;; ============================================================================
;; Blue/Green Deployment
;; ============================================================================

(module+ main
  (displayln "=== Deployment Pipeline Failure Analysis ===\n")

  ;; Example 1: Blue/Green deployment
  (displayln "Example 1: Blue/Green deployment")

  (define (blue-green-deploy)
    ;; Deploy to green environment
    (define green-deploy (flip 0.95))

    ;; Health checks on green
    (define green-healthy
      (if green-deploy
          (flip 0.98)
          #f))

    ;; Traffic cutover
    (define cutover-success
      (if green-healthy
          (flip 0.99)
          #f))

    ;; Rollback capability
    (define can-rollback
      (if (not cutover-success)
          (flip 0.999)  ; Can always rollback
          #t))

    ;; Success if cutover works OR rollback available
    (or cutover-success can-rollback))

  (displayln (~a "P(blue/green succeeds or rollback) = "
                (~r (probability (blue-green-deploy)) #:precision 4)))
  (newline))

;; ============================================================================
;; Canary Deployment
;; ============================================================================

(module+ canary
  (displayln "=== Canary Deployment ===\n")

  ;; Example 2: Canary release strategy
  (displayln "Example 2: Canary deployment stages")

  (define (canary-deploy)
    ;; Stage 1: 5% traffic
    (define canary-5pct (flip 0.90))

    ;; Stage 2: 25% traffic (if 5% ok)
    (define canary-25pct
      (if canary-5pct
          (flip 0.92)
          #f))

    ;; Stage 3: 50% traffic (if 25% ok)
    (define canary-50pct
      (if canary-25pct
          (flip 0.94)
          #f))

    ;; Stage 4: 100% traffic (if 50% ok)
    (define full-deploy
      (if canary-50pct
          (flip 0.96)
          #f))

    full-deploy)

  (displayln (~a "P(canary completes to 100%) = "
                (~r (probability (canary-deploy)) #:precision 4)))

  ;; Early detection
  (define (canary-early-detection)
    (define stage1 (flip 0.90))

    ;; Detect issues early at 5%
    (if (not stage1)
        'failed-at-5pct
        (let ([stage2 (flip 0.92)])
          (if (not stage2)
              'failed-at-25pct
              (let ([stage3 (flip 0.94)])
                (if (not stage3)
                    'failed-at-50pct
                    (let ([stage4 (flip 0.96)])
                      (if stage4
                          'success
                          'failed-at-100pct))))))))

  (define canary-dist
    (distribution-of (canary-early-detection)
                    '(failed-at-5pct failed-at-25pct failed-at-50pct
                      failed-at-100pct success)))

  (displayln "\nCanary failure distribution:")
  (for ([(stage prob) (in-hash canary-dist)])
    (when (> prob 0)
      (displayln (~a "  " (~a stage #:width 20) " → "
                    (~r prob #:precision 4)))))
  (newline))

;; ============================================================================
;; Rolling Update
;; ============================================================================

(module+ rolling
  (displayln "=== Rolling Update ===\n")

  ;; Example 3: Rolling update across instances
  (displayln "Example 3: Rolling update (3 instances)")

  (define (rolling-update replicas)
    ;; Update instances one by one
    (define (update-instance n)
      (if (= n 0)
          #t
          (and (flip 0.95)  ; Instance update succeeds
               (update-instance (- n 1)))))

    ;; All must succeed
    (update-instance replicas))

  (displayln (~a "P(rolling update succeeds) = "
                (~r (probability (rolling-update 3)) #:precision 4)))

  ;; With max unavailable
  (define (rolling-update-resilient replicas max-unavailable)
    (define failures
      (for/sum ([i replicas])
        (if (flip 0.95) 0 1)))

    ;; Can tolerate failures up to max-unavailable
    (<= failures max-unavailable))

  (displayln (~a "P(update succeeds with max-unavailable=1) = "
                (~r (probability (rolling-update-resilient 5 1)) #:precision 4)))
  (newline))

;; ============================================================================
;; Health Checks
;; ============================================================================

(module+ health
  (displayln "=== Health Check Scenarios ===\n")

  ;; Example 4: Health check configurations
  (displayln "Example 4: Liveness and readiness probes")

  (define (health-checks)
    ;; Liveness probe
    (define liveness
      (and (flip 0.99)   ; Process running
           (flip 0.98))) ; Not deadlocked

    ;; Readiness probe
    (define readiness
      (if liveness
          (and (flip 0.95)  ; Dependencies available
               (flip 0.97)) ; Can handle traffic
          #f))

    ;; Startup probe
    (define startup
      (if liveness
          (flip 0.90)  ; Initialization complete
          #f))

    (and liveness readiness startup))

  (displayln (~a "P(all health checks pass) = "
                (~r (probability (health-checks)) #:precision 4)))

  ;; With retries
  (define (health-with-retries)
    (define (probe-with-retry attempts)
      (if (= attempts 0)
          #f
          (or (flip 0.95)
              (probe-with-retry (- attempts 1)))))

    (probe-with-retry 3))

  (displayln (~a "P(health check passes with 3 retries) = "
                (~r (probability (health-with-retries)) #:precision 4)))
  (newline))

;; ============================================================================
;; Database Migration
;; ============================================================================

(module+ migration
  (displayln "=== Database Migration ===\n")

  ;; Example 5: Schema migration during deployment
  (displayln "Example 5: Database schema migration")

  (define (db-migration)
    ;; Backup before migration
    (define backup-success (flip 0.99))

    ;; Run migration
    (define migration-success
      (if backup-success
          (flip 0.95)
          #f))

    ;; Verify migration
    (define verification
      (if migration-success
          (flip 0.98)
          #f))

    ;; Rollback if verification fails
    (define final-state
      (if verification
          #t
          (if backup-success
              (flip 0.999)  ; Rollback from backup
              #f)))

    final-state)

  (displayln (~a "P(migration succeeds or rollback) = "
                (~r (probability (db-migration)) #:precision 4)))
  (newline))

;; ============================================================================
;; Traffic Shifting
;; ============================================================================

(module+ traffic
  (displayln "=== Traffic Shifting ===\n")

  ;; Example 6: Gradual traffic shift
  (displayln "Example 6: Progressive traffic shifting")

  (define (traffic-shift)
    ;; Shift traffic gradually
    (define (shift-stage pct)
      (disrupt
       [(pct) 'new-version]
       [(- 1 pct) 'old-version]))

    ;; Start with 10% on new version
    (define stage1 (shift-stage 0.1))

    ;; Monitor error rate
    (define stage1-ok
      (case stage1
        [(new-version) (flip 0.95)]  ; New version might have issues
        [(old-version) (flip 0.99)])) ; Old version stable

    ;; Continue if ok
    (if stage1-ok
        (let ([stage2 (shift-stage 0.5)])
          (case stage2
            [(new-version) (flip 0.97)]  ; Better after initial testing
            [(old-version) (flip 0.99)]))
        #f))

  (displayln (~a "P(traffic shift succeeds) = "
                (~r (probability (traffic-shift)) #:precision 4)))
  (newline))

;; ============================================================================
;; Configuration Rollout
;; ============================================================================

(module+ config
  (displayln "=== Configuration Rollout ===\n")

  ;; Example 7: Feature flag deployment
  (displayln "Example 7: Feature flag rollout")

  (define (feature-flag-rollout)
    ;; Roll out to internal users first
    (define internal-ok (flip 0.90))

    ;; Then beta users
    (define beta-ok
      (if internal-ok
          (flip 0.92)
          #f))

    ;; Finally all users
    (define general-ok
      (if beta-ok
          (flip 0.95)
          #f))

    ;; Can disable flag anytime
    (or general-ok (flip 0.999)))  ; Kill switch

  (displayln (~a "P(feature rollout succeeds) = "
                (~r (probability (feature-flag-rollout)) #:precision 4)))
  (newline))

;; ============================================================================
;; Multi-Region Deployment
;; ============================================================================

(module+ multi-region
  (displayln "=== Multi-Region Deployment ===\n")

  ;; Example 8: Deploy across regions
  (displayln "Example 8: Multi-region rollout")

  (define (multi-region-deploy regions)
    (define (deploy-to-region region)
      (case region
        [(us-east)   (flip 0.98)]
        [(us-west)   (flip 0.97)]
        [(eu-west)   (flip 0.96)]
        [(ap-south)  (flip 0.94)]))

    ;; All regions must succeed
    (andmap deploy-to-region regions))

  (displayln (~a "P(all regions succeed) = "
                (~r (probability (multi-region-deploy
                                 '(us-east us-west eu-west ap-south)))
                    #:precision 4)))

  ;; Progressive region rollout
  (define (progressive-regions)
    ;; Start with us-east
    (define us-east (flip 0.98))

    ;; Then us-west if us-east ok
    (define us-west
      (if us-east (flip 0.97) #f))

    ;; Then EU if US ok
    (define eu-west
      (if (and us-east us-west)
          (flip 0.96)
          #f))

    ;; Finally APAC
    (define ap-south
      (if (and us-east us-west eu-west)
          (flip 0.94)
          #f))

    (and us-east us-west eu-west ap-south))

  (displayln (~a "P(progressive rollout succeeds) = "
                (~r (probability (progressive-regions)) #:precision 4)))
  (newline))

;; ============================================================================
;; Rollback Strategies
;; ============================================================================

(module+ rollback
  (displayln "=== Rollback Strategies ===\n")

  ;; Example 9: Automated rollback
  (displayln "Example 9: Automated rollback")

  (define (deploy-with-auto-rollback)
    ;; Deploy new version
    (define deploy-ok (flip 0.90))

    ;; Monitor for issues
    (define monitoring-ok
      (if deploy-ok
          (flip 0.95)
          #f))

    ;; Auto-rollback if issues detected
    (if (not monitoring-ok)
        (flip 0.99)  ; Rollback success
        deploy-ok))

  (displayln (~a "P(deploy succeeds or rollback) = "
                (~r (probability (deploy-with-auto-rollback)) #:precision 4)))

  ;; Manual rollback
  (define (manual-rollback-available)
    (define deploy (flip 0.85))

    ;; If failed, can manually rollback
    (if deploy
        #t
        (flip 0.95)))  ; Manual intervention

  (displayln (~a "P(success with manual rollback option) = "
                (~r (probability (manual-rollback-available)) #:precision 4)))
  (newline))

;; ============================================================================
;; Complete Deployment Pipeline
;; ============================================================================

(module+ complete
  (displayln "=== Complete Deployment Pipeline ===\n")

  ;; Example 10: End-to-end deployment
  (displayln "Example 10: Full deployment pipeline")

  (define (complete-deployment)
    ;; 1. Build artifact
    (define artifact (flip 0.98))

    ;; 2. Pre-deployment checks
    (define pre-checks
      (if artifact
          (and (flip 0.99)  ; Version compatibility
               (flip 0.98)  ; Resource availability
               (flip 0.99)) ; Access permissions
          #f))

    ;; 3. Deploy to staging
    (define staging
      (if pre-checks
          (flip 0.95)
          #f))

    ;; 4. Run smoke tests
    (define smoke-tests
      (if staging
          (flip 0.92)
          #f))

    ;; 5. Deploy to production (canary)
    (define canary
      (if smoke-tests
          (flip 0.94)
          #f))

    ;; 6. Monitor and complete rollout
    (define full-rollout
      (if canary
          (flip 0.96)
          #f))

    ;; 7. Post-deployment verification
    (define verification
      (if full-rollout
          (flip 0.98)
          #f))

    verification)

  (displayln (~a "P(complete deployment succeeds) = "
                (~r (probability (complete-deployment)) #:precision 4)))

  (displayln "\n=== Deployment Pipeline Analysis Complete ==="))

;; ============================================================================
;; Helper Functions
;; ============================================================================

(define (distribution-of expr possible-values)
  (for/hash ([val possible-values])
    (values val (probability (equal? expr val)))))
