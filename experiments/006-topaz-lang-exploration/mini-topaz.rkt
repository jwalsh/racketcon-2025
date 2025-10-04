#lang rosette

;; Mini topaz-lang: Simplified DNS policy DSL with verification
;; Inspired by Cloudflare's topaz-lang

(require rosette/lib/synthax)

;; ============================================================================
;; DNS Query Structure
;; ============================================================================

(struct DNS-Query
  (name type client-ip)
  #:transparent)

;; DNS Query Types
(define A 1)
(define AAAA 28)
(define CNAME 5)
(define MX 15)

;; ============================================================================
;; Policy Structure
;; ============================================================================

(struct Policy
  (name match-fn response-fn config)
  #:transparent)

;; ============================================================================
;; Helper Functions
;; ============================================================================

(define (string-suffix? str suffix)
  ; Simplified: just check equality for now
  ; In real topaz-lang, would check if str ends with suffix
  (equal? str suffix))

(define (in-cidr? ip cidr)
  ; Simplified: check if IP matches
  ; In real implementation, would do proper CIDR matching
  #t)

;; ============================================================================
;; Example Policy 1: Simple A Record
;; ============================================================================

(define simple-policy
  (Policy
   "simple-a-record"
   ; Match function: exact name match
   (lambda (query)
     (and (equal? (DNS-Query-name query) "api.example.com")
          (= (DNS-Query-type query) A)))
   ; Response function
   (lambda ()
     '("192.0.2.1"))
   ; Config
   '((ttl 3600))))

;; ============================================================================
;; Example Policy 2: Wildcard Match
;; ============================================================================

(define wildcard-policy
  (Policy
   "wildcard-match"
   ; Match function: suffix match
   (lambda (query)
     (and (string-suffix? (DNS-Query-name query) ".example.com")
          (= (DNS-Query-type query) A)))
   ; Response function
   (lambda ()
     '("192.0.2.10"))
   ; Config
   '((ttl 300))))

;; ============================================================================
;; Example Policy 3: Geo-based Routing
;; ============================================================================

(define geo-policy
  (Policy
   "geo-routing"
   ; Match function: geo-aware
   (lambda (query)
     (and (equal? (DNS-Query-name query) "www.example.com")
          (= (DNS-Query-type query) A)
          (in-cidr? (DNS-Query-client-ip query) "192.0.2.0/24")))
   ; Response function
   (lambda ()
     '("192.0.2.100" "192.0.2.101"))
   ; Config
   '((region "US-WEST") (ttl 60))))

;; ============================================================================
;; Verification Functions
;; ============================================================================

;; Check 1: Satisfiability
;; Can this policy ever match any query?
(define (check-satisfiability policy)
  (define-symbolic* qname string?)
  (define-symbolic* qtype integer?)
  (define-symbolic* client-ip string?)

  (define query (DNS-Query qname qtype client-ip))
  (define match-fn (Policy-match-fn policy))

  (define result
    (solve
     (assert (match-fn query))))

  (displayln (format "Satisfiability check for '~a':" (Policy-name policy)))
  (if (sat? result)
      (begin
        (displayln "  ✓ SAT - Policy can match queries")
        (displayln (format "  Example: name=~a type=~a"
                          (evaluate qname result)
                          (evaluate qtype result))))
      (displayln "  ✗ UNSAT - Policy can never match!"))
  (newline)
  result)

;; Check 2: Reachability
;; Can new-policy be reached given existing-policies?
(define (check-reachability existing-policies new-policy)
  (define-symbolic* qname string?)
  (define-symbolic* qtype integer?)
  (define-symbolic* client-ip string?)

  (define query (DNS-Query qname qtype client-ip))

  (define (any-existing-matches?)
    (ormap (lambda (p) ((Policy-match-fn p) query))
           existing-policies))

  (define new-matches?
    ((Policy-match-fn new-policy) query))

  (define result
    (solve
     (assert
      (and (not (any-existing-matches?))
           new-matches?))))

  (displayln (format "Reachability check for '~a':" (Policy-name new-policy)))
  (if (sat? result)
      (begin
        (displayln "  ✓ REACHABLE - Policy can execute")
        (displayln (format "  Example: name=~a type=~a"
                          (evaluate qname result)
                          (evaluate qtype result))))
      (displayln "  ✗ UNREACHABLE - Shadowed by earlier policies!"))
  (newline)
  result)

;; Check 3: Conflicts
;; Do two "exclusive" policies ever match the same query?
(define (check-conflicts policy-a policy-b)
  (define-symbolic* qname string?)
  (define-symbolic* qtype integer?)
  (define-symbolic* client-ip string?)

  (define query (DNS-Query qname qtype client-ip))

  (define a-matches? ((Policy-match-fn policy-a) query))
  (define b-matches? ((Policy-match-fn policy-b) query))

  (define result
    (solve
     (assert (and a-matches? b-matches?))))

  (displayln (format "Conflict check between '~a' and '~a':"
                    (Policy-name policy-a)
                    (Policy-name policy-b)))
  (if (sat? result)
      (begin
        (displayln "  ✗ CONFLICT - Policies overlap!")
        (displayln (format "  Example: name=~a type=~a"
                          (evaluate qname result)
                          (evaluate qtype result))))
      (displayln "  ✓ NO CONFLICT - Policies are disjoint"))
  (newline)
  result)

;; ============================================================================
;; Run Verification
;; ============================================================================

(displayln "===== Mini topaz-lang Verification =====")
(newline)

;; Test satisfiability
(check-satisfiability simple-policy)
(check-satisfiability wildcard-policy)
(check-satisfiability geo-policy)

;; Test reachability
;; wildcard-policy should be reachable even with simple-policy first
(check-reachability (list simple-policy) wildcard-policy)

;; Test conflicts
;; simple-policy and wildcard-policy should conflict on "api.example.com"
(check-conflicts simple-policy wildcard-policy)

;; simple-policy and geo-policy should not conflict (different names)
(check-conflicts simple-policy geo-policy)

(displayln "===== Verification Complete =====")
