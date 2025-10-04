#lang roulette

;; Build Pipeline Failure Models
;; Probabilistic analysis of CI/CD build failures

(require racket/format)
(provide (all-defined-out))

;; ============================================================================
;; Build Stage Models
;; ============================================================================

(module+ main
  (displayln "=== Build Pipeline Failure Analysis ===\n")

  ;; Example 1: Sequential build stages
  (displayln "Example 1: Sequential build pipeline")

  (define (sequential-build)
    ;; Each stage must succeed
    (and (flip 0.99)  ; Checkout: 99%
         (flip 0.98)  ; Compile: 98%
         (flip 0.95)  ; Unit tests: 95%
         (flip 0.90)  ; Integration tests: 90%
         (flip 0.98)  ; Package: 98%
         (flip 0.99))) ; Upload: 99%

  (displayln (~a "P(build succeeds) = "
                (~r (probability (sequential-build)) #:precision 4)))

  ;; Individual stage failure probabilities
  (displayln "\nStage failure rates:")
  (displayln "  Checkout:     1%")
  (displayln "  Compile:      2%")
  (displayln "  Unit tests:   5%")
  (displayln "  Integration:  10%")
  (displayln "  Package:      2%")
  (displayln "  Upload:       1%")
  (newline))

;; ============================================================================
;; Parallel Build Stages
;; ============================================================================

(module+ parallel
  (displayln "=== Parallel Build Stages ===\n")

  ;; Example 2: Parallel test execution
  (displayln "Example 2: Parallel test suites")

  (define (parallel-tests)
    ;; All parallel suites must pass
    (and (flip 0.95)  ; Unit tests
         (flip 0.90)  ; Integration tests
         (flip 0.92)  ; E2E tests
         (flip 0.88))) ; Performance tests

  (displayln (~a "P(all tests pass) = "
                (~r (probability (parallel-tests)) #:precision 4)))

  ;; Compare to sequential (same total time if parallel)
  (displayln "\nParallel vs Sequential:")
  (displayln "  Parallel: Fails if ANY suite fails")
  (displayln "  Sequential: Fails at first failure (faster feedback)")
  (newline))

;; ============================================================================
;; Flaky Tests
;; ============================================================================

(module+ flaky
  (displayln "=== Flaky Test Analysis ===\n")

  ;; Example 3: Test flakiness
  (displayln "Example 3: Flaky test scenarios")

  (define (flaky-test-suite)
    ;; 5% of tests are flaky (fail randomly)
    (define stable-tests (flip 0.95))  ; 95% stable
    (define flaky-tests
      (or (flip 0.90)  ; Pass 90% when run
          (flip 0.90)  ; Retry once
          (flip 0.90))) ; Retry twice

    (and stable-tests flaky-tests))

  (displayln (~a "P(suite passes with retries) = "
                (~r (probability (flaky-test-suite)) #:precision 4)))

  ;; Without retries
  (define (no-retry-flaky)
    (and (flip 0.95)  ; Stable
         (flip 0.90))) ; Flaky (no retry)

  (displayln (~a "P(suite passes without retries) = "
                (~r (probability (no-retry-flaky)) #:precision 4)))
  (newline))

;; ============================================================================
;; Dependency Resolution
;; ============================================================================

(module+ dependencies
  (displayln "=== Dependency Resolution ===\n")

  ;; Example 4: Package dependency failures
  (displayln "Example 4: Dependency resolution")

  (define (dependency-resolution)
    ;; Direct dependencies
    (define deps-available (flip 0.99))

    ;; Transitive dependencies (if direct deps work)
    (define transitive-ok
      (if deps-available
          (flip 0.95)  ; Might have conflicts
          #f))

    ;; Version compatibility
    (define version-compatible
      (if (and deps-available transitive-ok)
          (flip 0.98)
          #f))

    (and deps-available transitive-ok version-compatible))

  (displayln (~a "P(dependencies resolve) = "
                (~r (probability (dependency-resolution)) #:precision 4)))

  ;; With lockfile
  (define (with-lockfile)
    (flip 0.999))  ; Lockfile ensures reproducibility

  (displayln (~a "P(resolution with lockfile) = "
                (~r (probability (with-lockfile)) #:precision 4)))
  (newline))

;; ============================================================================
;; Resource Exhaustion
;; ============================================================================

(module+ resources
  (displayln "=== Resource Exhaustion ===\n")

  ;; Example 5: Build resource limits
  (displayln "Example 5: Resource availability")

  (define (resource-constrained-build)
    ;; CPU availability
    (define cpu-ok (flip 0.95))

    ;; Memory availability (depends on CPU load)
    (define memory-ok
      (if cpu-ok
          (flip 0.90)  ; Less memory if CPU busy
          (flip 0.70))) ; Even less if CPU starved

    ;; Disk space
    (define disk-ok (flip 0.98))

    ;; Network (for downloads)
    (define network-ok (flip 0.97))

    (and cpu-ok memory-ok disk-ok network-ok))

  (displayln (~a "P(build completes with resources) = "
                (~r (probability (resource-constrained-build)) #:precision 4)))
  (newline))

;; ============================================================================
;; Build Matrix (Multi-platform)
;; ============================================================================

(module+ matrix
  (displayln "=== Build Matrix ===\n")

  ;; Example 6: Multi-platform builds
  (displayln "Example 6: Cross-platform build matrix")

  (define (platform-build platform)
    (case platform
      [(linux)   (flip 0.98)]
      [(macos)   (flip 0.95)]
      [(windows) (flip 0.90)]
      [(docker)  (flip 0.93)]))

  (define (build-matrix)
    ;; All platforms must succeed
    (and (platform-build 'linux)
         (platform-build 'macos)
         (platform-build 'windows)
         (platform-build 'docker)))

  (displayln (~a "P(all platforms succeed) = "
                (~r (probability (build-matrix)) #:precision 4)))

  ;; Best effort (at least one succeeds)
  (define (best-effort-matrix)
    (or (platform-build 'linux)
        (platform-build 'macos)
        (platform-build 'windows)
        (platform-build 'docker)))

  (displayln (~a "P(at least one platform succeeds) = "
                (~r (probability (best-effort-matrix)) #:precision 4)))
  (newline))

;; ============================================================================
;; Cache Effectiveness
;; ============================================================================

(module+ caching
  (displayln "=== Build Cache Analysis ===\n")

  ;; Example 7: Cache hit/miss scenarios
  (displayln "Example 7: Cache effectiveness")

  (define (build-with-cache)
    (define cache-hit (flip 0.80))  ; 80% cache hit rate

    (if cache-hit
        (flip 0.99)  ; Fast build with cache
        (flip 0.95))) ; Slower build without cache

  (displayln (~a "P(build succeeds with cache) = "
                (~r (probability (build-with-cache)) #:precision 4)))

  ;; Cache invalidation issues
  (define (cache-invalidation-bug)
    (define stale-cache (flip 0.05))  ; 5% stale

    (if stale-cache
        (flip 0.50)  ; Build might fail with stale cache
        (flip 0.98))) ; Normal build

  (displayln (~a "P(build succeeds despite cache issues) = "
                (~r (probability (cache-invalidation-bug)) #:precision 4)))
  (newline))

;; ============================================================================
;; Build Time Variation
;; ============================================================================

(module+ timing
  (displayln "=== Build Time Variation ===\n")

  ;; Example 8: Build timeout scenarios
  (displayln "Example 8: Build timeout analysis")

  (define (build-timeout max-time)
    ;; Build time varies
    (define build-time
      (disrupt
       [(0.6) 5]   ; Fast: 5 min
       [(0.3) 15]  ; Normal: 15 min
       [(0.1) 30])) ; Slow: 30 min

    ;; Succeeds if within timeout
    (<= build-time max-time))

  (displayln (~a "P(build completes in 10 min) = "
                (~r (probability (build-timeout 10)) #:precision 4)))
  (displayln (~a "P(build completes in 20 min) = "
                (~r (probability (build-timeout 20)) #:precision 4)))
  (displayln (~a "P(build completes in 35 min) = "
                (~r (probability (build-timeout 35)) #:precision 4)))
  (newline))

;; ============================================================================
;; Security Scanning
;; ============================================================================

(module+ security
  (displayln "=== Security Scanning ===\n")

  ;; Example 9: Security vulnerability detection
  (displayln "Example 9: Security scan failures")

  (define (security-scan)
    ;; Dependency vulnerabilities
    (define vuln-free (flip 0.85))

    ;; Code quality (if vuln-free)
    (define quality-ok
      (if vuln-free
          (flip 0.90)
          (flip 0.70)))  ; More issues if vulns present

    ;; License compliance
    (define license-ok (flip 0.95))

    (and vuln-free quality-ok license-ok))

  (displayln (~a "P(security scan passes) = "
                (~r (probability (security-scan)) #:precision 4)))

  ;; With auto-updates
  (define (auto-patched)
    (define has-vuln (flip 0.15))

    (if has-vuln
        (flip 0.80)  ; 80% auto-patch success
        #t))

  (displayln (~a "P(scan passes with auto-patch) = "
                (~r (probability (auto-patched)) #:precision 4)))
  (newline))

;; ============================================================================
;; Complete Build Pipeline Model
;; ============================================================================

(module+ complete
  (displayln "=== Complete Build Pipeline ===\n")

  ;; Example 10: Full CI/CD pipeline
  (displayln "Example 10: End-to-end build pipeline")

  (define (complete-pipeline)
    ;; Stage 1: Checkout
    (define checkout (flip 0.99))

    ;; Stage 2: Dependencies (if checkout ok)
    (define deps
      (if checkout (flip 0.98) #f))

    ;; Stage 3: Compile (if deps ok)
    (define compile
      (if deps (flip 0.97) #f))

    ;; Stage 4: Tests (if compile ok)
    (define tests
      (if compile
          (and (flip 0.95)  ; Unit
               (flip 0.90)  ; Integration
               (flip 0.85)) ; E2E
          #f))

    ;; Stage 5: Security (if tests ok)
    (define security
      (if tests (flip 0.88) #f))

    ;; Stage 6: Package (if security ok)
    (define package
      (if security (flip 0.98) #f))

    ;; Stage 7: Upload (if package ok)
    (define upload
      (if package (flip 0.99) #f))

    upload)

  (displayln (~a "P(complete pipeline succeeds) = "
                (~r (probability (complete-pipeline)) #:precision 4)))

  ;; Failure point analysis
  (define (failure-point)
    (define checkout (flip 0.99))
    (cond
      [(not checkout) 'checkout]
      [else
       (define deps (flip 0.98))
       (cond
         [(not deps) 'dependencies]
         [else
          (define compile (flip 0.97))
          (cond
            [(not compile) 'compile]
            [else
             (define tests (and (flip 0.95) (flip 0.90) (flip 0.85)))
             (cond
               [(not tests) 'tests]
               [else
                (define security (flip 0.88))
                (cond
                  [(not security) 'security]
                  [else
                   (define package (flip 0.98))
                   (cond
                     [(not package) 'package]
                     [else
                      (define upload (flip 0.99))
                      (if upload 'success 'upload)])])])])])])

  (define failure-dist
    (distribution-of (failure-point)
                    '(checkout dependencies compile tests
                      security package upload success)))

  (displayln "\nFailure point distribution:")
  (for ([(point prob) (in-hash failure-dist)])
    (when (> prob 0)
      (displayln (~a "  " (~a point #:width 15) " → "
                    (~r prob #:precision 4)))))

  (displayln "\n=== Build Pipeline Analysis Complete ==="))

;; ============================================================================
;; Helper: Distribution utilities
;; ============================================================================

(define (distribution-of expr possible-values)
  (for/hash ([val possible-values])
    (values val (probability (equal? expr val)))))
