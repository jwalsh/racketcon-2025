#lang rosette

;; Practical string encoding for DNS verification
;; Demonstrates workarounds for Rosette's lack of string theory support

;; ============================================================================
;; Approach 1: Enumerate all possible values
;; ============================================================================

(define dns-domains
  '(api.example.com
    www.example.com
    mail.example.com
    ftp.example.com
    cdn.example.com))

(define-symbolic* domain-index integer?)

(define (index->domain idx)
  (list-ref dns-domains (modulo idx (length dns-domains))))

(displayln "=== Approach 1: Enumeration ===")
(define result1
  (solve
    (assert
      (equal? (index->domain domain-index) 'api.example.com))))

(displayln (format "Domain index for api.example.com: ~a"
                  (evaluate domain-index result1)))
(newline)

;; ============================================================================
;; Approach 2: Hash-based encoding
;; ============================================================================

(displayln "=== Approach 2: Hash-based ===")

(define (domain->hash domain)
  (equal-hash-code domain))

(define domain-hashes
  (make-hash
    (for/list ([d dns-domains])
      (cons d (domain->hash d)))))

(define-symbolic* query-hash integer?)

(define (matches-domain? hash target-domain)
  (= hash (hash-ref domain-hashes target-domain)))

(define result2
  (solve
    (assert
      (matches-domain? query-hash 'www.example.com))))

(displayln (format "Hash for www.example.com: ~a"
                  (evaluate query-hash result2)))
(displayln (format "Expected hash: ~a"
                  (domain->hash 'www.example.com)))
(newline)

;; ============================================================================
;; Approach 3: Boolean predicates for string properties
;; ============================================================================

(displayln "=== Approach 3: Boolean Predicates ===")

(define-symbolic* is-api-subdomain? boolean?)
(define-symbolic* is-www-subdomain? boolean?)
(define-symbolic* is-wildcard? boolean?)

(define (domain-constraints)
  (assert
    ;; Mutual exclusion: can't be both api and www
    (not (and is-api-subdomain? is-www-subdomain?))
    ;; Wildcard implies neither specific subdomain
    (=> is-wildcard?
        (not (or is-api-subdomain? is-www-subdomain?)))))

(define result3
  (solve
    (begin
      (domain-constraints)
      (assert is-api-subdomain?))))

(when (sat? result3)
  (displayln "Found satisfying assignment:")
  (displayln (format "  is-api-subdomain?: ~a"
                    (evaluate is-api-subdomain? result3)))
  (displayln (format "  is-www-subdomain?: ~a"
                    (evaluate is-www-subdomain? result3)))
  (displayln (format "  is-wildcard?: ~a"
                    (evaluate is-wildcard? result3))))
(newline)

;; ============================================================================
;; Approach 4: Fixed-length character encoding
;; ============================================================================

(displayln "=== Approach 4: Character Encoding ===")

;; Simple alphabet: lowercase letters and dot
(define char-codes
  (hash #\a 0 #\b 1 #\c 2 #\d 3 #\e 4
        #\f 5 #\g 6 #\h 7 #\i 8 #\j 9
        #\k 10 #\l 11 #\m 12 #\n 13 #\o 14
        #\p 15 #\q 16 #\r 17 #\s 18 #\t 19
        #\u 20 #\v 21 #\w 22 #\x 23 #\y 24
        #\z 25 #\. 26))

(define (char->code c)
  (hash-ref char-codes c 0))

(define (string->codes s [max-len 16])
  (define chars (string->list s))
  (define codes (map char->code chars))
  (append codes (make-list (- max-len (length codes)) 0)))

;; Create symbolic character codes
(define-symbolic* c0 c1 c2 integer?)

;; Known pattern: "api"
(define api-codes (take (string->codes "api") 3))

(define result4
  (solve
    (assert
      (and (= c0 (first api-codes))
           (= c1 (second api-codes))
           (= c2 (third api-codes))))))

(when (sat? result4)
  (displayln "Character codes for 'api':")
  (displayln (format "  c0 (a): ~a" (evaluate c0 result4)))
  (displayln (format "  c1 (p): ~a" (evaluate c1 result4)))
  (displayln (format "  c2 (i): ~a" (evaluate c2 result4))))
(newline)

;; ============================================================================
;; Approach 5: Bitvector encoding for compact representation
;; ============================================================================

(displayln "=== Approach 5: Bitvector Encoding ===")

(require rosette/lib/synthax)

(define (char->bv c)
  (bv (char->code c) 5))  ; 5 bits = 32 possible chars

(define (string->bvs s [max-len 8])
  (define chars (string->list s))
  (define bvs (map char->bv chars))
  (append bvs (make-list (- max-len (length chars)) (bv 0 5))))

;; Symbolic bitvector string
(define-symbolic* bv0 bv1 bv2 (bitvector 5))

;; Match "com"
(define com-bvs (take (string->bvs "com") 3))

(define result5
  (solve
    (assert
      (and (bveq bv0 (first com-bvs))
           (bveq bv1 (second com-bvs))
           (bveq bv2 (third com-bvs))))))

(when (sat? result5)
  (displayln "Bitvector encoding for 'com':")
  (displayln (format "  bv0 (c): ~a" (evaluate bv0 result5)))
  (displayln (format "  bv1 (o): ~a" (evaluate bv1 result5)))
  (displayln (format "  bv2 (m): ~a" (evaluate bv2 result5))))
(newline)

;; ============================================================================
;; Summary
;; ============================================================================

(displayln "=== Summary ===")
(displayln "Rosette doesn't support SMT string theory, so we use:")
(displayln "1. Enumeration - finite set of possible values")
(displayln "2. Hashing - map strings to integers")
(displayln "3. Boolean predicates - abstract string properties")
(displayln "4. Character encoding - fixed-length integer sequences")
(displayln "5. Bitvector encoding - compact binary representation")
(displayln "")
(displayln "Each approach has tradeoffs:")
(displayln "- Enumeration: Simple but limited domain")
(displayln "- Hashing: Fast but loses structure")
(displayln "- Predicates: Abstract but less precise")
(displayln "- Character encoding: Flexible but verbose")
(displayln "- Bitvector: Compact but fixed length")
