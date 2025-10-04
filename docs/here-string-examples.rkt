#lang racket

;; Examples of Here Strings and At-Expressions in Racket

(displayln "=== Here String Examples ===\n")

;; Example 1: Multi-line text
(define description
  #<<EOF
RacketCon 2025
October 4-5, 2025
Boston, Massachusetts

A conference for Racket programming language enthusiasts.
EOF
)

(displayln "Description:")
(displayln description)

;; Example 2: SQL Query
(define sql-query
  #<<SQL
SELECT
  e.name,
  e.track,
  e.status
FROM experiments e
WHERE e.status = 'completed'
ORDER BY e.number
SQL
)

(displayln "SQL Query:")
(displayln sql-query)

;; Example 3: JSON Data
(define json-config
  #<<JSON
{
  "project": "racketcon-2025",
  "experiments": {
    "total": 310,
    "completed": 49,
    "in_progress": 3
  },
  "frameworks": ["RHEA", "Rosette", "Ocular-Patdown"]
}
JSON
)

(displayln "JSON Config:")
(displayln json-config)

;; Example 4: Python Script
(define python-code
  #<<PYTHON
def analyze_experiments(data):
    completed = [e for e in data if e['status'] == 'completed']
    return {
        'total': len(data),
        'completed': len(completed),
        'percentage': len(completed) / len(data) * 100
    }
PYTHON
)

(displayln "Python Code:")
(displayln python-code)

;; Example 5: Racket Code Block (meta!)
(define racket-code
  #<<RACKET
(define (lens-compose . lenses)
  (lambda (target)
    (foldl (lambda (lens acc)
             (lens acc))
           target
           lenses)))
RACKET
)

(displayln "Racket Code:")
(displayln racket-code)

;; Example 6: HTML Template
(define html-template
  #<<HTML
<!DOCTYPE html>
<html>
<head>
  <title>RacketCon 2025</title>
  <style>
    body { font-family: sans-serif; }
    h1 { color: #2c3e50; }
  </style>
</head>
<body>
  <h1>Welcome to RacketCon 2025</h1>
  <p>Experiments and explorations in Racket.</p>
</body>
</html>
HTML
)

(displayln "HTML Template:")
(displayln html-template)

;; Example 7: Test Data
(define test-data
  #<<DATA
001,Rosette Fundamentals,completed
002,Formal Methods,completed
041,Lenses from Scratch,completed
042,Optics Composition,completed
049,Ocular-Patdown Guide,in_progress
DATA
)

(displayln "Test Data:")
(displayln test-data)

;; Example 8: Documentation
(define module-doc
  #<<DOC
Module: lens-laws
Purpose: Verify lens laws for optics

Lens Laws:
1. GetPut: get(set(target, value)) = value
2. PutGet: set(target, get(target)) = target
3. PutPut: set(set(target, v1), v2) = set(target, v2)

Usage:
  (verify-lens-laws my-lens test-target)
DOC
)

(displayln "Documentation:")
(displayln module-doc)

;; Example 9: Using format with here strings
(define (make-experiment-readme number title)
  (format #<<README
#+TITLE: Experiment ~a: ~a
#+AUTHOR: RacketCon 2025
#+DATE: 2025-10-04

* Overview

Description of experiment ~a.

* Goals

* Implementation

* Results
README
          (~r number #:min-width 3 #:pad-string "0")
          title
          number))

(displayln "Generated README:")
(displayln (make-experiment-readme 50 "Advanced Pattern Matching"))

;; Example 10: RHEA Hypothesis with Here Strings
(displayln "\nRHEA Framework Example:")

(define hypothesis-text
  #<<HYPOTHESIS
Research Question: Can optics compose associatively?

Background:
Lenses, prisms, and traversals form a hierarchy of optics.
Composition should be associative: (a ∘ b) ∘ c = a ∘ (b ∘ c)

Hypothesis:
All optic compositions are associative regardless of optic type.

Predictions:
1. Property-based tests verify associativity for 1000+ cases
2. Formal proof validates for all optic types
3. No counterexamples found in random testing
HYPOTHESIS
)

(displayln hypothesis-text)
