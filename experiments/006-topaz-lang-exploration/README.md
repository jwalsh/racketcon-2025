# Experiment 006: Topaz-lang Exploration

## Overview

Understanding Cloudflare's topaz-lang DSL for DNS policy verification. This experiment explores how Rosette enables formal verification of DNS behavior at scale.

**Reference**: [Topaz Policy Engine Design](https://blog.cloudflare.com/topaz-policy-engine-design/)

## What is Topaz?

Topaz is Cloudflare's policy engine for managing DNS behavior across 100+ million domains. It consists of:

1. **topaz-lang**: A custom DSL for expressing DNS policies
2. **Verifier**: Written in Rosette, checks policies for bugs before deployment
3. **Runtime**: Executes policies on Cloudflare's edge network in real-time

## The Challenge

### DNS at Cloudflare Scale
- **Domains**: 100+ million
- **Queries**: Trillions per day
- **PoPs**: 300+ worldwide
- **Latency requirement**: Milliseconds

### Problems with Traditional Approach
- Manual configuration prone to errors
- Testing doesn't catch all edge cases
- One bug affects millions of domains
- Difficult to reason about policy interactions

## topaz-lang Design

### Language Characteristics

**Based on Scheme/Lisp**:
- S-expression syntax
- Dynamically typed
- Functional style

**Simplified for Verification**:
- Not Turing complete (enables decidability)
- Each expression evaluates to exactly one value
- Limited to SMT-decidable operations

**Type System**:
```scheme
; Basic types
(define ttl 3600)           ; Integer
(define enabled #t)         ; Boolean
(define ips '(192.0.2.1))  ; List

; DNS-specific types
(define addr (ipv4 "192.0.2.1"))
(define addr6 (ipv6 "2001:db8::1"))
(define time (ttl 3600))
```

### Policy Structure

Each policy has three components:

**1. Match Function**: When should this policy execute?
```scheme
(define (match-function query-name query-type)
  (and (string-suffix? query-name ".example.com")
       (= query-type A)))
```

**2. Response Function**: What IPs to return?
```scheme
(define (response-function)
  (list (ipv4 "192.0.2.1")
        (ipv4 "192.0.2.2")))
```

**3. Configuration**: Parameterize behavior
```scheme
(define config
  '((primary-ip "192.0.2.1")
    (backup-ip "192.0.2.2")
    (ttl 300)))
```

## Example Policies

### Simple A Record

```scheme
; Match any query for api.example.com
(define (match query)
  (= (query-name query) "api.example.com"))

; Return single IP
(define (response)
  (list (ipv4 "192.0.2.1")))

; TTL configuration
(define config '((ttl 3600)))
```

### Geo-based Routing

```scheme
; Match queries from specific region
(define (match query)
  (and (= (query-name query) "www.example.com")
       (in-region? (client-ip query) "US-WEST")))

; Return regional IPs
(define (response)
  (list (ipv4 "192.0.2.10")
        (ipv4 "192.0.2.11")))
```

### Failover Policy

```scheme
; Configuration with health check
(define config
  '((primary-pool ("192.0.2.1" "192.0.2.2"))
    (backup-pool ("192.0.2.10" "192.0.2.11"))
    (health-check #t)))

; Match function
(define (match query)
  (= (query-name query) "app.example.com"))

; Response with failover logic
(define (response)
  (if (all-healthy? (config-get 'primary-pool))
      (map ipv4 (config-get 'primary-pool))
      (map ipv4 (config-get 'backup-pool))))
```

## Verification with Rosette

### Three Key Properties

**1. Satisfiability**: Can this policy ever match?

```racket
#lang rosette

; Verify policy can match at least one query
(define-symbolic* query-name string?)
(define-symbolic* query-type integer?)

(verify
  (assert
    (exists ([qn string?] [qt integer?])
      (match-function qn qt))))
```

**2. Reachability**: Can this policy be reached given previous policies?

```racket
; Given policies P1, P2, ..., Pn, can Pn+1 ever execute?
(define-symbolic* query DNS-Query?)

(verify
  (assert
    (exists ([q DNS-Query?])
      (and (not (match-p1 q))
           (not (match-p2 q))
           ...
           (match-pn+1 q)))))
```

**3. Conflict Detection**: Do exclusive policies overlap?

```racket
; Policies marked as exclusive should not match same query
(verify
  (assert
    (not
      (exists ([q DNS-Query?])
        (and (match-policy-a q)
             (match-policy-b q))))))
```

### Verification Workflow

```
1. Engineer writes policy in topaz-lang
2. Policy submitted to verification system
3. Rosette translator converts to symbolic constraints
4. Z3 SMT solver checks properties
5. If verified: Deploy to edge
   If failed: Return counterexample to engineer
```

### Performance Characteristics

Based on Cloudflare's blog post:
- **~6 seconds**: Verify 7 policies
- **~300 seconds**: Verify 50 policies
- **Scales**: Linear in number of policies

## Implementation Architecture

### Topaz System Components

```
┌─────────────────┐
│ Engineer writes │
│  topaz policy   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   Rosette       │
│   Verifier      │  ← Written in Racket/Rosette
└────────┬────────┘
         │
    ┌────┴────┐
    │         │
    ▼         ▼
 [UNSAT]   [SAT + Model]
    │         │
    │         └──→ Counterexample
    ▼
┌─────────────────┐
│ Deploy to Edge  │
│  (Verified ✓)   │
└─────────────────┘
```

### Rosette Verifier Structure

```racket
#lang rosette

; 1. Define symbolic DNS query
(struct DNS-Query
  (name type class client-ip)
  #:transparent)

(define-symbolic* qname string?)
(define-symbolic* qtype integer?)
(define-symbolic* client ipv4?)

; 2. Translate topaz-lang to Rosette
(define (translate-policy policy)
  ; Parse S-expressions
  ; Build symbolic constraints
  ...)

; 3. Verification predicates
(define (check-satisfiability policy)
  (solve
    (assert (match-function (DNS-Query qname qtype ...)))))

(define (check-reachability policies new-policy)
  (solve
    (assert
      (exists ([q DNS-Query?])
        (and (not (any-previous-match? policies q))
             (match-function new-policy q))))))

(define (check-conflicts policy-a policy-b)
  (verify
    (assert
      (not (exists ([q DNS-Query?])
             (and (match-function policy-a q)
                  (match-function policy-b q)))))))
```

## Limitations and Trade-offs

### What topaz-lang Can Do
✅ Integer/bitVector operations
✅ Boolean logic
✅ IP address matching
✅ List operations (bounded)
✅ Conditional logic

### What topaz-lang Cannot Do
❌ Complex string manipulation
❌ Unbounded loops
❌ External API calls
❌ Arbitrary recursion
❌ Side effects

### Why These Limitations?

**Decidability**: SMT solvers need decidable logic
**Performance**: Verification must complete in seconds
**Safety**: Limited operations reduce attack surface

## Recreating topaz-lang Concepts

### Mini topaz-lang in Rosette

See `mini-topaz.rkt` for a simplified implementation demonstrating:
- Policy structure
- Symbolic DNS queries
- Verification predicates
- Counterexample generation

## Real-World Impact

### Before Topaz
- Manual DNS configuration
- Testing on subset of queries
- Bugs found in production
- Difficult to reason about interactions

### After Topaz
- Policies verified before deployment
- Bugs caught automatically
- Confidence in correctness
- Clear policy interactions

### Bugs Prevented

From Cloudflare's experience:
1. **Unsatisfiable policies**: Can never match any query
2. **Unreachable policies**: Shadowed by earlier policies
3. **Conflicting exclusives**: Multiple exclusive policies match same query
4. **Type errors**: IP address format mismatches

## Learning Path

### 1. Understand DNS Basics
- See `experiments/004-dns-tools-macos-freebsd`

### 2. Learn Rosette Fundamentals
- See `experiments/001-rosette-fundamentals`

### 3. Study Verification
- See `experiments/002-formal-methods-overview`

### 4. Build Mini DSL
- Implement simplified topaz-lang
- Add verification checks
- Test with sample policies

### 5. Read Cloudflare Blog Posts
- [Topaz Policy Engine](https://blog.cloudflare.com/topaz-policy-engine-design/)
- [DNS Architecture](https://blog.cloudflare.com/dns-architecture/)

## Exercises

### Exercise 1: Satisfiability
Write a policy that can never match and verify it's unsatisfiable.

### Exercise 2: Reachability
Create two policies where the second is unreachable.

### Exercise 3: Conflicts
Create two "exclusive" policies that conflict.

### Exercise 4: Mini Verifier
Implement a simple verifier for a subset of topaz-lang.

## Resources

### Papers
- "Growing Solver-Aided Languages with Rosette" (Torlak & Bodik)
- Rosette documentation

### Code Examples
- `mini-topaz.rkt`: Simplified topaz-lang
- `verifier.rkt`: Basic verification examples
- `policies.rkt`: Example DNS policies

### External Links
- [Cloudflare Blog](https://blog.cloudflare.com/tag/dns/)
- [Rosette Guide](https://docs.racket-lang.org/rosette-guide/)
- [Z3 SMT Solver](https://github.com/Z3Prover/z3)

## Next Steps

1. Read the Cloudflare blog post in detail
2. Experiment with Rosette verification
3. Build a minimal DNS policy DSL
4. Verify simple policies
5. Explore advanced features (geo-routing, load balancing)
