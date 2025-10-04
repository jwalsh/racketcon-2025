#lang racket

;; Example DNS policies in topaz-lang style
;; These are illustrative - not executable without the full topaz runtime

;; ============================================================================
;; Policy 1: Basic A Record
;; ============================================================================

(define basic-a-record
  '(policy "basic-a"
    (match
      (lambda (query)
        (= (query-name query) "api.example.com")))

    (response
      (lambda ()
        (list (ipv4 "192.0.2.1"))))

    (config
      ((ttl 3600)))))

;; ============================================================================
;; Policy 2: Multi-IP Round Robin
;; ============================================================================

(define round-robin
  '(policy "round-robin"
    (match
      (lambda (query)
        (= (query-name query) "app.example.com")))

    (response
      (lambda ()
        (list (ipv4 "192.0.2.1")
              (ipv4 "192.0.2.2")
              (ipv4 "192.0.2.3"))))

    (config
      ((ttl 60)
       (strategy "round-robin")))))

;; ============================================================================
;; Policy 3: Geo-based Routing
;; ============================================================================

(define geo-routing
  '(policy "geo-us-west"
    (match
      (lambda (query)
        (and (= (query-name query) "www.example.com")
             (in-region? (client-country query) "US")
             (in-region? (client-region query) "CA"))))

    (response
      (lambda ()
        (list (ipv4 "192.0.2.10")
              (ipv4 "192.0.2.11"))))

    (config
      ((region "US-WEST")
       (ttl 60)))))

;; ============================================================================
;; Policy 4: Failover with Health Checks
;; ============================================================================

(define failover
  '(policy "failover"
    (match
      (lambda (query)
        (= (query-name query) "service.example.com")))

    (response
      (lambda ()
        (if (all-healthy? primary-pool)
            (pool-ips primary-pool)
            (pool-ips backup-pool))))

    (config
      ((primary-pool ("192.0.2.1" "192.0.2.2"))
       (backup-pool ("192.0.2.10" "192.0.2.11"))
       (health-check-interval 30)
       (ttl 120)))))

;; ============================================================================
;; Policy 5: Load Balancing with Weights
;; ============================================================================

(define weighted-lb
  '(policy "weighted-lb"
    (match
      (lambda (query)
        (string-suffix? (query-name query) ".cdn.example.com")))

    (response
      (lambda ()
        (weighted-sample
          (list
            (cons (ipv4 "192.0.2.1") 0.7)   ; 70% of traffic
            (cons (ipv4 "192.0.2.2") 0.2)   ; 20% of traffic
            (cons (ipv4 "192.0.2.3") 0.1))))) ; 10% of traffic

    (config
      ((strategy "weighted")
       (ttl 30)))))

;; ============================================================================
;; Policy 6: IPv6 Support
;; ============================================================================

(define ipv6-policy
  '(policy "ipv6"
    (match
      (lambda (query)
        (and (= (query-name query) "api.example.com")
             (= (query-type query) AAAA))))

    (response
      (lambda ()
        (list (ipv6 "2001:db8::1")
              (ipv6 "2001:db8::2"))))

    (config
      ((ttl 3600)))))

;; ============================================================================
;; Policy 7: CNAME Response
;; ============================================================================

(define cname-policy
  '(policy "cname"
    (match
      (lambda (query)
        (= (query-name query) "www.example.com")))

    (response
      (lambda ()
        (cname "example.com")))

    (config
      ((ttl 3600)))))

;; ============================================================================
;; Policy 8: Rate Limiting by Client
;; ============================================================================

(define rate-limited
  '(policy "rate-limit"
    (match
      (lambda (query)
        (and (= (query-name query) "api.example.com")
             (exceeds-rate? (client-ip query) 100 60)))) ; 100 req/min

    (response
      (lambda ()
        (list (ipv4 "192.0.2.99")))) ; Rate-limited pool

    (config
      ((rate-limit 100)
       (rate-window 60)
       (ttl 1)))))

;; ============================================================================
;; Policy 9: Maintenance Mode
;; ============================================================================

(define maintenance-mode
  '(policy "maintenance"
    (match
      (lambda (query)
        (and (string-suffix? (query-name query) ".example.com")
             (get-config "maintenance-mode"))))

    (response
      (lambda ()
        (list (ipv4 "192.0.2.250")))) ; Maintenance page IP

    (config
      ((maintenance-mode #f)  ; Toggle via config
       (ttl 60)))))

;; ============================================================================
;; Policy 10: DDoS Mitigation
;; ============================================================================

(define ddos-mitigation
  '(policy "ddos-protect"
    (match
      (lambda (query)
        (and (= (query-name query) "protected.example.com")
             (suspicious-pattern? (client-ip query)))))

    (response
      (lambda ()
        (list (ipv4 "192.0.2.200")))) ; Challenge/CAPTCHA page

    (config
      ((threat-score-threshold 80)
       (ttl 1)))))

;; ============================================================================
;; Export all policies
;; ============================================================================

(provide basic-a-record
         round-robin
         geo-routing
         failover
         weighted-lb
         ipv6-policy
         cname-policy
         rate-limited
         maintenance-mode
         ddos-mitigation)
