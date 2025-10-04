#lang racket

;; Experiment 142: Here Strings - Basic Syntax
;; Demonstrates fundamental here string patterns

(displayln "=== Experiment 142.1: Basic Here String Syntax ===\n")

;; ============================================================================
;; Example 1: Standard EOF Delimiter
;; ============================================================================

(displayln "Example 1: Standard EOF")

(define text1
  #<<EOF
This is a multi-line string.
It can contain "quotes" and 'apostrophes'.
No escaping needed!
EOF
)

(displayln text1)
(newline)

;; ============================================================================
;; Example 2: Custom Delimiters
;; ============================================================================

(displayln "Example 2: Custom Delimiters")

(define code-block
  #<<CODE
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))
CODE
)

(define data-section
  #<<DATA
Alice,30,Boston
Bob,25,Cambridge
Carol,35,Somerville
DATA
)

(displayln "Code:")
(displayln code-block)
(displayln "\nData:")
(displayln data-section)
(newline)

;; ============================================================================
;; Example 3: Special Characters - No Escaping
;; ============================================================================

(displayln "Example 3: Special Characters")

(define special-chars
  #<<TEXT
This contains: \ backslash
"double quotes"
'single quotes'
$dollar $signs
{braces} [brackets] (parens)
\n literal backslash-n (not a newline escape)
TEXT
)

(displayln special-chars)
(newline)

;; ============================================================================
;; Example 4: Indentation Preserved
;; ============================================================================

(displayln "Example 4: Indentation")

(define indented
  #<<YAML
server:
  host: localhost
  port: 8080
  config:
    debug: true
    timeout: 30
YAML
)

(displayln indented)
(newline)

;; ============================================================================
;; Example 5: Empty Lines Preserved
;; ============================================================================

(displayln "Example 5: Empty Lines")

(define with-empty-lines
  #<<POEM
Roses are red,

Violets are blue,

Here strings are literal,

And easy to use!
POEM
)

(displayln with-empty-lines)
(newline)

;; ============================================================================
;; Example 6: Using in Functions
;; ============================================================================

(displayln "Example 6: In Functions")

(define (make-email recipient subject body)
  (format #<<EMAIL
To: ~a
Subject: ~a

Dear ~a,

~a

Best regards,
RacketCon Team
EMAIL
          recipient
          subject
          recipient
          body))

(displayln (make-email "Alice"
                       "Welcome!"
                       "Thank you for registering."))
(newline)

;; ============================================================================
;; Example 7: Multiple Here Strings
;; ============================================================================

(displayln "Example 7: Multiple Here Strings")

(define header
  #<<HEADER
RacketCon 2025
October 4-5, 2025
HEADER
)

(define body
  #<<BODY
Join us for two days of:
- Talks
- Workshops
- Networking
BODY
)

(define footer
  #<<FOOTER
Location: Boston, MA
Website: con.racket-lang.org
FOOTER
)

(displayln (string-append header "\n\n" body "\n\n" footer))
(newline)

;; ============================================================================
;; Example 8: Delimiter Naming Conventions
;; ============================================================================

(displayln "Example 8: Delimiter Conventions")

(define sql-query
  #<<SQL
SELECT * FROM users
SQL
)

(define html-snippet
  #<<HTML
<h1>Hello</h1>
HTML
)

(define json-config
  #<<JSON
{"name": "racketcon"}
JSON
)

(displayln "SQL:")
(displayln sql-query)
(displayln "\nHTML:")
(displayln html-snippet)
(displayln "\nJSON:")
(displayln json-config)
(newline)

;; ============================================================================
;; Example 9: Comparison with Regular Strings
;; ============================================================================

(displayln "Example 9: Regular vs Here Strings")

;; Regular string - requires escaping
(define regular
  "Line 1\nLine 2\n\"Quoted\" text\n\\Backslash")

;; Here string - no escaping
(define here-string
  #<<TEXT
Line 1
Line 2
"Quoted" text
\Backslash
TEXT
)

(displayln "Regular string:")
(displayln regular)
(displayln "\nHere string:")
(displayln here-string)
(newline)

;; ============================================================================
;; Example 10: Practical Pattern - License Headers
;; ============================================================================

(displayln "Example 10: License Headers")

(define (add-license filename)
  (string-append
    (format #<<LICENSE
;;; ~a --- Description
;;
;; Copyright (C) 2025 RacketCon
;;
;; This file is part of the RacketCon 2025 experiments.
;;
;; Permission is hereby granted, free of charge...
;;
;;; Code:

LICENSE
            filename)
    "\n;; Your code here\n"))

(displayln (add-license "my-module.rkt"))

;; ============================================================================
;; Summary
;; ============================================================================

(displayln "=== Summary ===")
(displayln "✓ Here strings use #<<DELIMITER ... DELIMITER")
(displayln "✓ Content is completely literal (no escaping)")
(displayln "✓ Delimiters can be any identifier")
(displayln "✓ Perfect for SQL, HTML, JSON, config files")
(displayln "✓ Use format for variable interpolation")
