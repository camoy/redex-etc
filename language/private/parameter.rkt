#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide default-language-template
         default-set-template
         default-set-procedure
         default-rhs-template
         default-rhs-procedure
         default-nt-template
         default-nt-procedure
         default-production-with-rhs-template
         default-production-no-rhs-template
         default-terminal-template
         default-space-procedure
         default-repeat-procedure

         current-language-template
         current-set-template
         current-set-procedure
         current-rhs-template
         current-rhs-procedure
         current-nt-template
         current-nt-procedure
         current-production-with-rhs-template
         current-production-no-rhs-template
         current-terminal-template
         current-terminal-procedure
         current-space-procedure
         current-repeat-procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/string
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defaults

(define default-language-template
  "~a\n\\begin{tabular}{r@{ } c@{ } l@{\\qquad} l}\n~a\n\\end{tabular}")

(define default-set-template "\\in {\\sf ~a}")

(define (default-set-procedure s)
  (if (symbol? s)
      (format (current-set-template) s)
      s))

(define default-rhs-template
  "& \\(~a\\) & \\(~a\\) & ~a \\\\")

(define (default-rhs-procedure first? rhs desc)
  (format (current-rhs-template)
          (if first? "=" "\\mid")
          rhs
          desc))

(define default-nt-template "{\\tt ~a}")

(define (default-nt-procedure nt)
  (define (fail nt)
    (format (current-nt-template) nt))
  (unicode-lookup nt fail))

(define default-production-with-rhs-template
  "\\(~a ~a\\) ~a")

#;":~a ~a ::= ~a \\\\"

(define default-production-no-rhs-template
  "\\(~a ~a\\) \\\\")

#;"*:~a ~a \\\\"

(define default-terminal-template
  "{\\tt ~a}")

(define (default-terminal-procedure x)
  (define (format-terminal term)
    (format (current-terminal-template) term))
  (cond
    [(symbol? x) (unicode-lookup x format-terminal)]
    [(boolean? x) (format-terminal (if x "true" "false"))]
    [x (error 'default-terminal-procedure
              "don't know how to render literal ~a"
              x)]))

(define (default-space-procedure xs)
  (string-join xs "\\,"))

(define (default-repeat-procedure x)
  (format "~a \\ldots" x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

(define current-language-template
  (make-parameter default-language-template))

(define current-set-template
  (make-parameter default-set-template))

(define current-set-procedure
  (make-parameter default-set-procedure))

(define current-rhs-template
  (make-parameter default-rhs-template))

(define current-rhs-procedure
  (make-parameter default-rhs-procedure))

(define current-nt-template
  (make-parameter default-nt-template))

(define current-nt-procedure
  (make-parameter default-nt-procedure))

(define current-production-with-rhs-template
  (make-parameter default-production-with-rhs-template))

(define current-production-no-rhs-template
  (make-parameter default-production-no-rhs-template))

(define current-terminal-template
  (make-parameter default-terminal-template))

(define current-terminal-procedure
  (make-parameter default-terminal-procedure))

(define current-space-procedure
  (make-parameter default-space-procedure))

(define current-repeat-procedure
  (make-parameter default-repeat-procedure))