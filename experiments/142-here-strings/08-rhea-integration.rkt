#lang racket

;; Experiment 142.8: Here Strings with RHEA Framework
;; Using here strings for scientific documentation

(displayln "=== Experiment 142.8: RHEA Integration ===\n")

;; ============================================================================
;; Example 1: Hypothesis with Here Strings
;; ============================================================================

(displayln "Example 1: RHEA Hypothesis Documentation")

(struct hypothesis (id description assumptions predictions metadata) #:transparent)

(define lens-composition-hypothesis
  (hypothesis
   "lens-composition"
   #<<DESCRIPTION
Research Question: Do lenses compose associatively?

Background:
Lenses are bidirectional accessors with getter and setter functions.
Composition of lenses should follow associativity: (a ∘ b) ∘ c = a ∘ (b ∘ c)

This property is critical for:
1. Building complex nested accessors
2. Ensuring predictable behavior
3. Optimizing lens chains
DESCRIPTION

   #<<ASSUMPTIONS
1. Individual lenses satisfy lens laws:
   - GetPut: get(set(target, value)) = value
   - PutGet: set(target, get(target)) = target
   - PutPut: set(set(target, v1), v2) = set(target, v2)

2. Composition operator preserves types correctly

3. Test data covers diverse nesting depths (2-5 levels)

4. Property-based testing generates sufficient cases (1000+)
ASSUMPTIONS

   #<<PREDICTIONS
1. All composition orders yield identical results

2. Property tests pass for 1000+ randomized lens chains

3. Performance scales linearly with chain depth

4. No counterexamples found in exhaustive testing
PREDICTIONS

   (hash 'domain 'functional-programming
         'track 'optics
         'experiments '(41 42 45 47 48)
         'status 'active)))

(displayln "Hypothesis ID: " (hypothesis-id lens-composition-hypothesis))
(displayln "\n" (hypothesis-description lens-composition-hypothesis))
(displayln "\nAssumptions:")
(displayln (hypothesis-assumptions lens-composition-hypothesis))
(displayln "\nPredictions:")
(displayln (hypothesis-predictions lens-composition-hypothesis))
(newline)

;; ============================================================================
;; Example 2: Experiment Protocol
;; ============================================================================

(displayln "Example 2: Experiment Protocol")

(define experiment-protocol
  #<<PROTOCOL
EXPERIMENT: Lens Composition Associativity Test

SETUP:
1. Generate random nested data structures (depth 2-5)
2. Create lens chains of varying lengths (2-10 lenses)
3. Prepare property-based test generators

PROCEDURE:
1. For each test case:
   a. Generate random lens composition: (a ∘ b ∘ c)
   b. Compute left-associated: ((a ∘ b) ∘ c)
   c. Compute right-associated: (a ∘ (b ∘ c))
   d. Verify results are equal

2. Collect timing data for performance analysis

3. Log any counterexamples for investigation

EXPECTED RESULTS:
- All test cases pass (100% success rate)
- No performance degradation with chain length
- Timing data shows linear scaling

SUCCESS CRITERIA:
- Zero counterexamples in 1000+ tests
- Mean execution time < 1ms per test
- Standard deviation < 0.5ms
PROTOCOL
)

(displayln experiment-protocol)
(newline)

;; ============================================================================
;; Example 3: Test Data Generation
;; ============================================================================

(displayln "Example 3: Test Data Templates")

(define test-data-spec
  #<<TESTDATA
# Lens Composition Test Data Specification

## Nested Structures

person:
  name: string
  age: number
  address:
    street: string
    city: string
    state: string
    zip: string
    coordinates:
      lat: float
      lon: float

## Test Cases

case_1:
  description: Simple two-level nesting
  structure: person -> address -> city
  lenses: [person-address-lens, address-city-lens]

case_2:
  description: Deep three-level nesting
  structure: person -> address -> coordinates -> lat
  lenses: [person-address-lens, address-coords-lens, coords-lat-lens]

case_3:
  description: Complex composition chain
  structure: company -> departments -> employees -> person -> address
  depth: 5
  lenses: 4 compositions
TESTDATA
)

(displayln test-data-spec)
(newline)

;; ============================================================================
;; Example 4: Analysis Results Template
;; ============================================================================

(displayln "Example 4: Analysis Results")

(define (format-analysis-results passed failed mean-time)
  (format #<<ANALYSIS
RHEA ANALYSIS RESULTS
=====================

Hypothesis: Lens Composition Associativity
Experiment: Property-Based Testing
Date: 2025-10-04

QUANTITATIVE RESULTS:
  Tests Passed:    ~a / ~a (~a%)
  Tests Failed:    ~a
  Mean Time:       ~a ms
  Status:          ~a

QUALITATIVE FINDINGS:
  ✓ All property tests passed
  ✓ Performance scales linearly
  ✓ No edge cases discovered

INTERPRETATION:
  The hypothesis is SUPPORTED by empirical evidence.
  Lens composition demonstrates associativity across
  all tested configurations and data types.

CONFIDENCE LEVEL: HIGH (1000+ tests, zero failures)

RECOMMENDATIONS:
  1. Proceed with optimization based on associativity
  2. Document composition laws for library users
  3. Add compile-time composition optimization
ANALYSIS
          passed
          (+ passed failed)
          (exact->inexact (* (/ passed (+ passed failed)) 100))
          failed
          mean-time
          (if (zero? failed) "PASSED" "FAILED")))

(displayln (format-analysis-results 1000 0 0.42))
(newline)

;; ============================================================================
;; Example 5: Org-Mode Export Template
;; ============================================================================

(displayln "Example 5: Org-Mode Export")

(define (export-hypothesis-to-org hyp)
  (format #<<ORG
#+TITLE: RHEA Hypothesis: ~a
#+AUTHOR: RacketCon 2025
#+DATE: 2025-10-04

* Hypothesis

** Description
~a

** Assumptions
~a

** Predictions
~a

* Metadata

| Property | Value |
|----------+-------|
| Domain   | ~a    |
| Track    | ~a    |
| Status   | ~a    |

* Related Experiments

~a

* Workflow

#+BEGIN_SRC mermaid
graph TD
  H[Hypothesis] --> E[Experiment]
  E --> D[Data Collection]
  D --> A[Analysis]
  A --> R{Result}
  R -->|Supported| C[Conclusion]
  R -->|Rejected| H2[Refined Hypothesis]
#+END_SRC
ORG
          (hypothesis-id hyp)
          (hypothesis-description hyp)
          (hypothesis-assumptions hyp)
          (hypothesis-predictions hyp)
          (hash-ref (hypothesis-metadata hyp) 'domain)
          (hash-ref (hypothesis-metadata hyp) 'track)
          (hash-ref (hypothesis-metadata hyp) 'status)
          (string-join
           (map (lambda (n) (format "- [[file:../~3,'0d-*/README.org][Experiment ~3,'0d]]" n n))
                (hash-ref (hypothesis-metadata hyp) 'experiments))
           "\n")))

(displayln (export-hypothesis-to-org lens-composition-hypothesis))

;; ============================================================================
;; Example 6: Combining Multiple Sections
;; ============================================================================

(displayln "\n=== Example 6: Complete RHEA Document ===")

(define complete-rhea-doc
  (string-append
   #<<HEADER
# RHEA FRAMEWORK DOCUMENTATION
# Racket Hypothesis-Experiment-Analysis
#
# This document follows the scientific method for
# programming language experiments.

HEADER

   #<<HYPOTHESIS_SECTION
## HYPOTHESIS

Research Question: Can we verify DNS policy rules using Rosette?

Assumptions:
- DNS queries follow RFC standards
- Policy rules are expressible in topaz-lang DSL
- SMT solver can handle policy complexity

Predictions:
- All valid queries pass verification
- Invalid queries are caught with specific error codes
- Verification completes in < 1 second

HYPOTHESIS_SECTION

   #<<EXPERIMENT_SECTION
## EXPERIMENT

Setup:
1. Install Rosette and topaz-lang
2. Prepare test DNS queries (100 valid, 100 invalid)
3. Define policy rules in DSL

Procedure:
1. Run each query through verifier
2. Collect results and timing data
3. Analyze error patterns

EXPERIMENT_SECTION

   #<<ANALYSIS_SECTION
## ANALYSIS

Results:
- 100/100 valid queries passed (100%)
- 98/100 invalid queries caught (98%)
- Mean verification time: 0.8s

Interpretation:
Hypothesis SUPPORTED with high confidence.
Two edge cases require policy refinement.

ANALYSIS_SECTION
   ))

(displayln complete-rhea-doc)

;; ============================================================================
;; Summary
;; ============================================================================

(displayln "\n=== Summary ===")
(displayln "✓ Here strings perfect for RHEA documentation")
(displayln "✓ Multi-line hypotheses, assumptions, predictions")
(displayln "✓ Experiment protocols and test specifications")
(displayln "✓ Analysis results and interpretations")
(displayln "✓ Org-mode export templates")
(displayln "✓ Combines readability with programmatic generation")
