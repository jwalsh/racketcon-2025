#lang rosette

;; Advanced verification examples for DNS policies
;; Demonstrates verification techniques used in topaz-lang

(require rosette/lib/synthax)

;; ============================================================================
;; Domain Types
;; ============================================================================

(struct IPv4 (octets) #:transparent)
(struct IPv6 (segments) #:transparent)

(struct DNSQuery
  (name type client-ip client-country client-region)
  #:transparent)

;; DNS types
(define A 1)
(define AAAA 28)
(define CNAME 5)

;; ============================================================================
;; Symbolic DNS Queries
;; ============================================================================

(define (make-symbolic-query)
  (define-symbolic* qname string?)
  (define-symbolic* qtype integer?)
  (define-symbolic* client-ip string?)
  (define-symbolic* client-country string?)
  (define-symbolic* client-region string?)
  (DNSQuery qname qtype client-ip client-country client-region))

;; ============================================================================
;; Verification Property 1: Mutual Exclusion
;; ============================================================================

;; Verify that two policies never both match the same query
(define (verify-mutual-exclusion match-fn-1 match-fn-2)
  (displayln "\n=== Mutual Exclusion Check ===")

  (define q (make-symbolic-query))

  (define result
    (verify
     (assert
      (not (and (match-fn-1 q)
                (match-fn-2 q))))))

  (if (unsat? result)
      (displayln "✓ Policies are mutually exclusive")
      (begin
        (displayln "✗ Policies overlap!")
        (displayln (format "  Counterexample: ~a" result))))

  result)

;; ============================================================================
;; Verification Property 2: Complete Coverage
;; ============================================================================

;; Verify that at least one policy matches every valid query
(define (verify-complete-coverage match-fns)
  (displayln "\n=== Complete Coverage Check ===")

  (define q (make-symbolic-query))

  ;; Add constraints for valid queries
  (define valid-query?
    (and (or (= (DNSQuery-type q) A)
             (= (DNSQuery-type q) AAAA))
         ; Add more validity constraints as needed
         ))

  (define at-least-one-matches?
    (apply || (map (lambda (fn) (fn q)) match-fns)))

  (define result
    (verify
     (assert
      (=> valid-query?
          at-least-one-matches?))))

  (if (unsat? result)
      (displayln "✓ All valid queries are covered")
      (begin
        (displayln "✗ Some queries not covered!")
        (displayln (format "  Counterexample: ~a" result))))

  result)

;; ============================================================================
;; Verification Property 3: Response Validity
;; ============================================================================

;; Verify that response function always returns valid IPs
(define (verify-response-validity response-fn)
  (displayln "\n=== Response Validity Check ===")

  (define ips (response-fn))

  ;; Check each IP is valid
  (define all-valid?
    (andmap
     (lambda (ip)
       (and (IPv4? ip)
            (let ([octets (IPv4-octets ip)])
              (and (>= (length octets) 4)
                   (andmap (lambda (o)
                            (and (>= o 0) (<= o 255)))
                          octets)))))
     ips))

  (if all-valid?
      (displayln "✓ All responses are valid")
      (displayln "✗ Invalid response detected!"))

  all-valid?)

;; ============================================================================
;; Verification Property 4: Policy Ordering
;; ============================================================================

;; Verify that policy order matters (first-match semantics)
(define (verify-policy-ordering policies)
  (displayln "\n=== Policy Ordering Check ===")

  (define q (make-symbolic-query))

  ;; For each policy, check if it's shadowed by earlier ones
  (for ([i (in-range 1 (length policies))])
    (define current-policy (list-ref policies i))
    (define earlier-policies (take policies i))

    (define current-matches?
      ((car current-policy) q))

    (define earlier-match?
      (apply || (map (lambda (p) ((car p) q)) earlier-policies)))

    (define result
      (solve
       (assert
        (and current-matches?
             (not earlier-match?)))))

    (displayln (format "\nPolicy ~a:" i))
    (if (sat? result)
        (displayln "  ✓ Reachable despite earlier policies")
        (displayln "  ⚠ Never reached - always shadowed!"))))

;; ============================================================================
;; Example Policies for Testing
;; ============================================================================

;; Policy 1: Exact match
(define (exact-match-policy q)
  (and (equal? (DNSQuery-name q) "api.example.com")
       (= (DNSQuery-type q) A)))

;; Policy 2: Wildcard (overlaps with Policy 1!)
(define (wildcard-policy q)
  (and (or (equal? (DNSQuery-name q) "api.example.com")
           (equal? (DNSQuery-name q) "www.example.com"))
       (= (DNSQuery-type q) A)))

;; Policy 3: Different name
(define (other-policy q)
  (and (equal? (DNSQuery-name q) "other.example.com")
       (= (DNSQuery-type q) A)))

;; Policy 4: IPv6
(define (ipv6-policy q)
  (= (DNSQuery-type q) AAAA))

;; Response functions
(define (basic-response)
  (list (IPv4 '(192 0 2 1))))

(define (multi-response)
  (list (IPv4 '(192 0 2 1))
        (IPv4 '(192 0 2 2))))

;; ============================================================================
;; Run Verifications
;; ============================================================================

(displayln "====== DNS Policy Verification Suite ======")

;; Test 1: Mutual exclusion
(verify-mutual-exclusion exact-match-policy other-policy)
(verify-mutual-exclusion exact-match-policy wildcard-policy)

;; Test 2: Coverage
(verify-complete-coverage
 (list exact-match-policy wildcard-policy other-policy ipv6-policy))

;; Test 3: Response validity
(verify-response-validity basic-response)
(verify-response-validity multi-response)

;; Test 4: Ordering
(verify-policy-ordering
 (list (cons exact-match-policy basic-response)
       (cons wildcard-policy multi-response)
       (cons other-policy basic-response)))

(displayln "\n====== Verification Complete ======")
