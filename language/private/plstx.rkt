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
  "~a\n\\begin{plstx}\n~a\n\\end{plstx}")

(define (plstx-rhs-procedure extend? first? rhs desc)
  (format "~a ~a"
          (if (and first? (not extend?)) "" "|")
          rhs))

(define (plstx-production-procedure extend? rhs? name set rhs-list)
  (define extend-line
    ((current-rhs-procedure) #f #t (current-production-extend) ""))
  (define rhs-list*
    (if extend? (cons extend-line rhs-list) rhs-list))
  (define fmt-str
    (if rhs?
        "~a : ~a ::= ~a \\\\"
        "*~a: ~a [\\in] ~a \\\\"))
  (format fmt-str
          set
          ((current-nt-procedure) name)
          (string-join rhs-list*)))
