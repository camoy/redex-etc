#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide plstx-language-template
         plstx-rhs-procedure
         plstx-production-procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/string
         "parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defaults

(define plstx-language-template
  "~a\\begin{plstx}\n~a\n\\end{plstx}")

(define (plstx-rhs-procedure extend? first? rhs desc)
  (format "~a ~a"
          (if (and first? (not extend?)) "" "|")
          rhs))

(define (plstx-production-procedure rhs? name set rhs-list)
  (define fmt-str
    (if rhs?
        ": ~a \\in {\\sf ~a} ::= ~a \\\\"
        "*: ~a \\in {\\sf ~a} [ ] ~a \\\\"))
  (format fmt-str
          ((current-nt-procedure) name)
          set
          (if rhs?
              (string-join rhs-list)
              "")))
