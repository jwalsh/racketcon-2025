# Experiment 003: Rosette vs Lean vs TLA+

## Overview

Detailed comparison of three popular formal methods tools: Rosette (SMT-based), Lean (theorem prover), and TLA+ (model checker). Each excels in different scenarios.

## Side-by-Side Comparison

### Philosophy

**Rosette**
- Solver-aided programming: verification as programming
- Embedded in Racket, full language integration
- "Correct code through symbolic execution"

**Lean**
- Dependently-typed theorem proving
- Mathematics-first, then programs
- "Correct code through proofs"

**TLA+**
- Temporal logic specification
- State machines and invariants
- "Correct systems through model checking"

### Workflow

**Rosette**
```racket
#lang rosette
(define-symbolic x integer?)
(verify (assert (property x)))
```
Write code, add symbolic values, verify inline.

**Lean**
```lean
theorem property (x : ℤ) : ... := by
  intro
  simp
  ...
```
State theorem, construct proof, extract code.

**TLA+**
```tla
THEOREM Property == ...
PROOF ... QED
```
Specify state machine, check with TLC model checker.

## Feature Comparison

| Feature | Rosette | Lean | TLA+ |
|---------|---------|------|------|
| **Automation** | High (SMT) | Low (manual proofs) | High (model checking) |
| **Expressiveness** | Bounded verification | Full mathematics | Temporal properties |
| **Learning Curve** | Medium (Racket + SMT) | Steep (type theory) | Medium (logic + spec) |
| **Integration** | Racket programs | Standalone + extraction | Specification only |
| **Synthesis** | Yes (built-in) | Limited | No |
| **Debugging** | Counterexamples | Proof goals | Error traces |
| **Scale** | Medium (bounded) | Large (compositional) | Small-Medium (states) |

## Example: Verifying a Max Function

### Rosette

```racket
#lang rosette

(define-symbolic x y integer?)

(define (max2 a b)
  (if (>= a b) a b))

(verify
  (assert
    (and (>= (max2 x y) x)
         (>= (max2 x y) y)
         (or (= (max2 x y) x)
             (= (max2 x y) y)))))
```

**Pros**: Concise, automatic, finds counterexamples
**Cons**: Bounded to SMT-decidable logic

### Lean

```lean
def max2 (a b : Int) : Int :=
  if a ≥ b then a else b

theorem max2_ge_left (a b : Int) : max2 a b ≥ a := by
  unfold max2
  split
  · linarith
  · linarith

theorem max2_ge_right (a b : Int) : max2 a b ≥ b := by
  unfold max2
  split
  · linarith
  · linarith

theorem max2_is_one (a b : Int) : max2 a b = a ∨ max2 a b = b := by
  unfold max2
  split <;> simp
```

**Pros**: Proven for all integers, reusable proofs
**Cons**: Manual proof construction, verbose

### TLA+

```tla
---- MODULE Max ----
EXTENDS Integers

Max(a, b) == IF a >= b THEN a ELSE b

MaxCorrect ==
  \A a, b \in Int:
    /\ Max(a, b) >= a
    /\ Max(a, b) >= b
    /\ Max(a, b) \in {a, b}

THEOREM MaxCorrect
====
```

**Pros**: Clear specification, model checker validates
**Cons**: Not executable code, finite models only

## Example: Distributed System (Two-Phase Commit)

### Rosette

```racket
#lang rosette

(struct state (phase votes committed) #:transparent)

(define (coordinator-step s)
  (match s
    [(state 'prepare votes #f)
     (if (andmap identity votes)
         (state 'commit votes #t)
         (state 'abort votes #f))]
    [_ s]))

; Verify safety: once committed, all voted yes
(define-symbolic* vote1 vote2 boolean?)
(define s (state 'prepare (list vote1 vote2) #f))

(verify
  (assert
    (=> (state-committed (coordinator-step s))
        (and vote1 vote2))))
```

**Good for**: Quick prototyping, bounded verification
**Limited**: State space size, temporal properties

### TLA+ (Best Choice)

```tla
---- MODULE TwoPhaseCommit ----
VARIABLES phase, votes, committed

Init ==
  /\ phase = "prepare"
  /\ votes \in [Participant -> BOOLEAN]
  /\ committed = FALSE

CoordinatorCommit ==
  /\ phase = "prepare"
  /\ \A p \in Participant: votes[p] = TRUE
  /\ committed' = TRUE
  /\ UNCHANGED <<phase, votes>>

SafetyInvariant ==
  committed => (\A p \in Participant: votes[p] = TRUE)

THEOREM SafetyInvariant
====
```

**Good for**: Protocol design, temporal reasoning
**Industry use**: AWS, Microsoft, MongoDB

### Lean

```lean
structure State where
  phase : String
  votes : List Bool
  committed : Bool

def coordinatorStep (s : State) : State :=
  match s.phase with
  | "prepare" =>
    if s.votes.all id then
      { s with committed := true }
    else
      { s with committed := false }
  | _ => s

theorem safety (s : State) :
  s.phase = "prepare" →
  (coordinatorStep s).committed →
  s.votes.all id := by
  intro h_phase h_committed
  unfold coordinatorStep at h_committed
  simp [h_phase] at h_committed
  split at h_committed <;> simp_all
```

**Good for**: Deep correctness proofs
**Overkill for**: Protocol design exploration

## When to Choose Each

### Choose Rosette When:
- ✅ Building verified DSLs
- ✅ Synthesizing code from specs
- ✅ Symbolic execution of programs
- ✅ Need tight language integration
- ❌ Need infinite-state verification
- ❌ Complex mathematical proofs

### Choose Lean When:
- ✅ Mathematical formalization
- ✅ Verified compiler/interpreter
- ✅ Deep correctness guarantees
- ✅ Reusable proof libraries
- ❌ Quick iteration on designs
- ❌ Automated verification needed

### Choose TLA+ When:
- ✅ Distributed protocols
- ✅ Concurrent algorithms
- ✅ System-level design
- ✅ Temporal properties
- ❌ Need executable code
- ❌ Complex data structures

## Real-World Examples

### Rosette
- **Cloudflare**: DNS policy verification (topaz-lang)
- **Neutrons**: Network verification
- **Ferrite**: Synthesis of numerical code

### Lean
- **Mathlib**: Comprehensive math library
- **Lean 4 compiler**: Self-hosting verified compiler
- **Duper**: Automated theorem proving

### TLA+
- **AWS**: S3, DynamoDB, EBS protocols
- **Microsoft**: Azure Cosmos DB
- **MongoDB**: Replication protocol

## Learning Resources

### Rosette
- [Rosette Guide](https://docs.racket-lang.org/rosette-guide/)
- [ONWARD! 2013 Paper](https://homes.cs.washington.edu/~emina/pubs/rosette.onward13.pdf)
- [Cloudflare Blog](https://blog.cloudflare.com/tag/rosette/)

### Lean
- [Theorem Proving in Lean 4](https://leanprover.github.io/theorem_proving_in_lean4/)
- [Mathematics in Lean](https://leanprover-community.github.io/mathematics_in_lean/)
- [Lean Zulip Chat](https://leanprover.zulipchat.com/)

### TLA+
- [Learn TLA+](https://learntla.com/)
- [Practical TLA+](https://www.apress.com/gp/book/9781484238288)
- [TLA+ Examples](https://github.com/tlaplus/Examples)

## Conclusion

**Rosette**: Best for solver-aided programming and DSLs
**Lean**: Best for mathematical proofs and verified software
**TLA+**: Best for distributed systems and protocols

For Cloudflare's DNS use case, Rosette was the right choice:
- Needed DSL integration (topaz-lang)
- Bounded verification sufficient
- Synthesis capabilities valuable
- Racket ecosystem fit well
