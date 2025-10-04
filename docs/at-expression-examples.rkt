#lang at-exp racket

;; Examples of At-Expressions (@-expressions) in Racket
;; Note: Requires #lang at-exp racket

(displayln "=== At-Expression Examples ===\n")

;; Example 1: Basic At-Expression with string-append
(define greeting
  @string-append{
    Hello from RacketCon 2025!
    This is using at-expressions.
  })

(displayln "Greeting:")
(displayln greeting)
(newline)

;; Example 2: With Variable Interpolation
(define conference "RacketCon 2025")
(define city "Boston")

(define announcement
  @string-append{
    Welcome to @|conference|!
    We're meeting in @|city|, Massachusetts.
  })

(displayln "Announcement:")
(displayln announcement)
(newline)

;; Example 3: HTML Generation
(define (html-page title content)
  @string-append{
    <!DOCTYPE html>
    <html>
      <head>
        <title>@|title|</title>
      </head>
      <body>
        <h1>@|title|</h1>
        <div class="content">
          @|content|
        </div>
      </body>
    </html>
  })

(displayln "HTML Page:")
(displayln (html-page "RacketCon 2025"
                      @string-append{
                        <p>Join us for two days of Racket!</p>
                        <p>October 4-5, 2025</p>
                      }))
(newline)

;; Example 4: Nested At-Expressions
(define name "Alice")
(define age 30)

(define profile
  @string-append{
    Name: @|name|
    Age: @|(~a age)|
    Status: @|(if (> age 25) "Senior" "Junior")|
  })

(displayln "Profile:")
(displayln profile)
(newline)

;; Example 5: List Generation
(define experiments '(("001" "Rosette")
                      ("041" "Lenses")
                      ("049" "Ocular-Patdown")))

(define experiment-list
  @string-append{
    Experiments:
    @|(string-join
       (for/list ([exp experiments])
         (format "  - Experiment ~a: ~a" (first exp) (second exp)))
       "\n")|
  })

(displayln experiment-list)
(newline)

;; Example 6: Configuration Template
(define (make-config env host port)
  @string-append{
    # Configuration for @|env|

    server {
      host: @|host|
      port: @|(~a port)|
      environment: @|env|
    }

    logging {
      level: @|(if (string=? env "production") "error" "debug")|
    }
  })

(displayln "Development Config:")
(displayln (make-config "development" "localhost" 8080))
(newline)

(displayln "Production Config:")
(displayln (make-config "production" "api.example.com" 443))
(newline)

;; Example 7: Markdown Generation
(define (markdown-section title items)
  @string-append{
    ## @|title|

    @|(string-join
       (for/list ([item items])
         (format "- ~a" item))
       "\n")|
  })

(displayln (markdown-section "Key Features"
                             '("Lenses" "Traversals" "Isomorphisms" "Prisms")))
(newline)

;; Example 8: SQL Query Builder
(define (select-query table columns where)
  @string-append{
    SELECT @|(string-join columns ", ")|
    FROM @|table|
    WHERE @|where|;
  })

(displayln (select-query "experiments"
                         '("number" "name" "status")
                         "status = 'completed'"))
(newline)

;; Example 9: Email Template
(define (email-template recipient subject body)
  @string-append{
    To: @|recipient|
    Subject: @|subject|

    Dear @|recipient|,

    @|body|

    Best regards,
    RacketCon 2025 Team
  })

(displayln (email-template "Alice"
                           "Welcome to RacketCon!"
                           @string-append{
                             Thank you for registering for RacketCon 2025.

                             We look forward to seeing you in Boston!
                           }))
(newline)

;; Example 10: JSON-like Generation
(define (experiment-json number name status)
  @string-append{
    {
      "number": "@|number|",
      "name": "@|name|",
      "status": "@|status|",
      "framework": "RHEA",
      "date": "2025-10-04"
    }
  })

(displayln (experiment-json "049" "Ocular-Patdown Guide" "in_progress"))
