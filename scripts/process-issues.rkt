#lang racket

;; Process GitHub issues JSON and generate org-mode report

(require json
         racket/date)

(define (read-json-file path)
  "Read JSON file and return parsed data."
  (call-with-input-file path
    (lambda (in)
      (read-json in))))

(define (extract-issue-info issue)
  "Extract relevant fields from issue hash."
  (hash 'number (hash-ref issue 'number)
        'title (hash-ref issue 'title)
        'state (hash-ref issue 'state)
        'assignees (map (lambda (a) (hash-ref a 'login))
                       (hash-ref issue 'assignees '()))))

(define (issue->org-row issue)
  "Convert issue hash to org-mode table row."
  (let ([num (hash-ref issue 'number)]
        [title (hash-ref issue 'title)]
        [state (hash-ref issue 'state)]
        [assignees (hash-ref issue 'assignees)])
    (format "| #~a | ~a | ~a | ~a |"
            num
            title
            state
            (if (null? assignees)
                ""
                (string-join (map (lambda (a) (format "@~a" a)) assignees) ", ")))))

(define (count-by-state issues)
  "Count issues by state."
  (define (state-eq? target)
    (lambda (i) (equal? (hash-ref i 'state) target)))
  (hash 'OPEN (length (filter (state-eq? "OPEN") issues))
        'CLOSED (length (filter (state-eq? "CLOSED") issues))))

(define (count-with-assignees issues)
  "Count issues with assignees."
  (length (filter (lambda (i)
                   (not (null? (hash-ref i 'assignees))))
                 issues)))

(define (generate-org-report issues)
  "Generate org-mode report from issues."
  (displayln "#+TITLE: RacketCon 2025 - Experiment Status")
  (displayln "#+AUTHOR: Generated from GitHub Issues")
  (displayln (format "#+DATE: ~a" (date->string (current-date) #t)))
  (displayln "#+STARTUP: overview")
  (displayln "")
  (displayln "* Overview")
  (displayln "")
  (displayln "This document is auto-generated from GitHub issues.")
  (displayln "")
  (displayln "* Statistics")
  (displayln "")

  (let ([stats (count-by-state issues)])
    (displayln (format "- Total Issues: ~a" (length issues)))
    (displayln (format "- Open: ~a" (hash-ref stats 'OPEN)))
    (displayln (format "- Closed: ~a" (hash-ref stats 'CLOSED)))
    (displayln (format "- With Assignees: ~a" (count-with-assignees issues))))

  (displayln "")
  (displayln "* All Issues")
  (displayln "")
  (displayln "| Issue | Title | State | Assignees |")
  (displayln "|-------|-------|-------|-----------|")

  (for-each (lambda (issue)
              (displayln (issue->org-row issue)))
            issues)

  (displayln "")
  (displayln "* In Progress")
  (displayln "")
  (displayln "Issues with assignees:")
  (displayln "")

  (for-each (lambda (issue)
              (when (not (null? (hash-ref issue 'assignees)))
                (displayln (format "- [[https://github.com/jwalsh/racketcon-2025/issues/~a][#~a]]: ~a (~a)"
                                 (hash-ref issue 'number)
                                 (hash-ref issue 'number)
                                 (hash-ref issue 'title)
                                 (string-join (map (lambda (a) (format "@~a" a))
                                                 (hash-ref issue 'assignees))
                                            ", ")))))
            issues))

(define (main args)
  (let* ([json-file (if (null? args) "/tmp/issues.json" (car args))]
         [issues-data (read-json-file json-file)]
         [issues (map extract-issue-info issues-data)])
    (generate-org-report issues)))

(module+ main
  (main (vector->list (current-command-line-arguments))))
