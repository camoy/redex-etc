#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide unicode-lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require latex-utils/scribble/unmap
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define (unicode-lookup c failure-thunk)
  (define str (symbol->string c))
  (if (= (string-length str) 1)
      (let ([c (string-ref str 0)])
        (match (translate-char c)
          [(regexp #rx"\\${(.+)}\\$" (list _ cmd)) cmd]
          [_ (make-string 1 c)]))
      (failure-thunk str)))
