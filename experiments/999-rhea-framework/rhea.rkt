#lang racket

;; RHEA Framework - Racket Hypothesis-Experiment-Analysis
;; A scientific method framework for programming language experiments

(require syntax/parse/define
         racket/match
         racket/contract
         racket/date)

(provide (struct-out hypothesis)
         (struct-out experiment)
         (struct-out observation)
         (struct-out analysis)
         define-hypothesis
         run-experiment
         analyze-results
         refine-hypothesis
         export-to-org)

;; ============================================================================
;; Core Data Structures
;; ============================================================================

(struct hypothesis (id description assumptions predictions metadata)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc h port mode)
     (fprintf port "#<hypothesis:~a>" (hypothesis-id h)))])

(struct experiment (id hypothesis-id setup procedure data-collector metadata)
  #:transparent)

(struct observation (id experiment-id timestamp data context)
  #:transparent)

(struct analysis (id observations statistical-tests results interpretation)
  #:transparent)

;; ============================================================================
;; DSL Macros
;; ============================================================================

(define-syntax-parse-rule
  (define-hypothesis name:id
    [#:description desc:expr]
    [#:assumes assumption ...]
    [#:predicts prediction ...]
    [#:metadata meta-expr ...])
  (define name
    (hypothesis 'name
                desc
                (list assumption ...)
                (list prediction ...)
                (hash meta-expr ...))))

(define-syntax-parse-rule
  (run-experiment exp-name:id
    [#:tests hypothesis-val:expr]
    [#:setup setup-thunk:expr]
    [#:procedure proc:expr]
    [#:collect collector:expr]
    [#:replicate n:expr])
  (begin
    (define exp-name
      (experiment 'exp-name
                  (hypothesis-id hypothesis-val)
                  setup-thunk
                  proc
                  collector
                  (hash 'replicates n
                        'timestamp (current-seconds))))
    (execute-experiment exp-name n)))

;; ============================================================================
;; Experiment Execution Engine
;; ============================================================================

(define (execute-experiment exp replicates)
  (define setup (experiment-setup exp))
  (define proc (experiment-procedure exp))
  (define collect (experiment-data-collector exp))

  (for/list ([i (in-range replicates)])
    (define env (setup))
    (define result (proc env))
    (observation (gensym 'obs)
                 (experiment-id exp)
                 (current-seconds)
                 (collect result)
                 (hash 'replicate i 'environment env))))

;; ============================================================================
;; Statistical Analysis
;; ============================================================================

(define (analyze-results observations
                        #:tests [tests '(mean variance t-test)]
                        #:alpha [alpha 0.05])
  (define data (map observation-data observations))

  (define statistical-results
    (for/hash ([test tests])
      (values test
              (match test
                ['mean (mean data)]
                ['variance (variance data)]
                ['t-test (t-test data alpha)]
                ['anova (anova-test data alpha)]
                [_ (error "Unknown test:" test)]))))

  (analysis (gensym 'analysis)
            observations
            tests
            statistical-results
            (interpret-results statistical-results alpha)))

(define (mean data)
  (/ (apply + data) (length data)))

(define (variance data)
  (define μ (mean data))
  (/ (apply + (map (λ (x) (expt (- x μ) 2)) data))
     (- (length data) 1)))

(define (t-test data alpha)
  ;; Simplified one-sample t-test
  (define μ (mean data))
  (define s (sqrt (variance data)))
  (define n (length data))
  (define t-stat (/ μ (/ s (sqrt n))))
  (hash 't-statistic t-stat
        'significant? (> (abs t-stat) 2.0))) ; Simplified

(define (anova-test data alpha)
  ;; Placeholder for ANOVA
  (hash 'f-statistic 0.0 'p-value 1.0))

(define (interpret-results results alpha)
  (for/list ([(test result) (in-hash results)])
    (list test
          (match result
            [(hash-table ['significant? #t] _ ...)
             'significant]
            [(hash-table ['significant? #f] _ ...)
             'not-significant]
            [_ 'inconclusive]))))

;; ============================================================================
;; Hypothesis Refinement
;; ============================================================================

(define (refine-hypothesis h analysis-result)
  (match (analysis-interpretation analysis-result)
    [(list-no-order (list _ 'significant) ...)
     (hypothesis (gensym 'hyp)
                 (format "Refined: ~a" (hypothesis-description h))
                 (hypothesis-assumptions h)
                 (hypothesis-predictions h)
                 (hash-set (hypothesis-metadata h)
                          'status 'supported))]
    [_
     (hypothesis (gensym 'hyp)
                 (format "Alternative: ~a" (hypothesis-description h))
                 (hypothesis-assumptions h)
                 '() ;; Need new predictions
                 (hash-set (hypothesis-metadata h)
                          'status 'rejected))]))

;; ============================================================================
;; Org-Mode Export (for literate programming)
;; ============================================================================

(define (export-to-org hypothesis experiments analyses [filename "research.org"])
  (with-output-to-file filename
    #:exists 'replace
    (λ ()
      (printf "#+TITLE: RHEA Research: ~a\n" (hypothesis-description hypothesis))
      (printf "#+AUTHOR: Generated by RHEA Framework\n")
      (printf "#+DATE: ~a\n\n" (date->string (current-date) #t))

      (printf "* Hypothesis\n")
      (printf "** Description\n")
      (printf "~a\n\n" (hypothesis-description hypothesis))

      (printf "** Assumptions\n")
      (for ([assumption (hypothesis-assumptions hypothesis)])
        (printf "- ~a\n" assumption))
      (printf "\n")

      (printf "** Predictions\n")
      (for ([prediction (hypothesis-predictions hypothesis)])
        (printf "- ~a\n" prediction))
      (printf "\n")

      (printf "* Experiments\n")
      (for ([exp experiments])
        (printf "** Experiment: ~a\n" (experiment-id exp))
        (printf "#+BEGIN_SRC racket :tangle experiment-~a.rkt\n"
                (experiment-id exp))
        (printf "~s\n" (experiment-procedure exp))
        (printf "#+END_SRC\n\n"))

      (printf "* Analysis\n")
      (for ([analysis analyses])
        (printf "** Analysis Results\n")
        (printf "#+BEGIN_SRC racket :results output\n")
        (printf "~s\n" (analysis-results analysis))
        (printf "#+END_SRC\n\n")

        (printf "** Interpretation\n")
        (printf "~a\n\n" (analysis-interpretation analysis)))

      (printf "* Workflow Diagram\n")
      (printf "#+BEGIN_SRC mermaid\n")
      (export-mermaid-workflow hypothesis experiments analyses)
      (printf "#+END_SRC\n"))))

(define (export-mermaid-workflow hypothesis experiments analyses)
  (printf "graph TD\n")
  (printf "  H[\"~a\"] --> E\n" (hypothesis-id hypothesis))
  (for ([exp experiments])
    (printf "  E[Experiment: ~a] --> O~a[Observations]\n"
            (experiment-id exp)
            (experiment-id exp)))
  (for ([analysis analyses])
    (printf "  O --> A~a[\"Analysis: ~a\"]\n"
            (analysis-id analysis)
            (car (analysis-interpretation analysis))))
  (printf "  A --> R{\"Refine Hypothesis\"}\n")
  (printf "  R -->|Supported| H2[New Hypothesis]\n")
  (printf "  R -->|Rejected| H3[Alternative Hypothesis]\n"))

;; ============================================================================
;; Example Usage
;; ============================================================================

(module+ example
  (define-hypothesis lens-composition
    [#:description "Lens composition preserves lens laws"]
    [#:assumes "Individual lenses satisfy GetPut, PutGet, PutPut"
               "Composition is associative"]
    [#:predicts "Composed lenses satisfy all three lens laws"
                "Property tests verify laws for 1000+ cases"]
    [#:metadata 'domain 'functional-programming
                'status 'proposed
                'related-experiments '(045 047 048)])

  (run-experiment lens-law-test
    [#:tests lens-composition]
    [#:setup (λ () (hash 'test-data '(1 2 3 4 5)))]
    [#:procedure (λ (env)
                   ;; Simulate lens law verification
                   (if (> (random) 0.05) 1 0))] ; 95% success rate
    [#:collect identity]
    [#:replicate 100])

  (define results (analyze-results lens-law-test
                                   #:tests '(mean variance t-test)))

  (define refined (refine-hypothesis lens-composition results))

  (displayln "=== RHEA Framework Example Output ===")
  (displayln (format "Hypothesis: ~a" (hypothesis-description lens-composition)))
  (displayln (format "Mean success rate: ~a" (hash-ref (analysis-results results) 'mean)))
  (displayln (format "Status: ~a"
                     (if (equal? 'significant
                                (cadar (analysis-interpretation results)))
                         "SUPPORTED"
                         "REJECTED")))

  (export-to-org lens-composition
                 (list lens-law-test)
                 (list results)))
