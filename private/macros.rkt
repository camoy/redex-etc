#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide match-term
         not-match?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require redex/reduction-semantics
         syntax/parse/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define-simple-macro (match-term L:id e0:expr [pat e:expr] ...)
  ((term-match/single L [pat e] ...) e0))

(define-syntax-rule (not-match? lang p e)
  (not (redex-match? lang p (term e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require "../test/common.rkt"
           chk)

  (chk
   #:t (not-match? Λ (λ (x) e) x)
   #:! #:t (not-match? Λ (λ (x) e) (λ (y) y))
   #:t
   (match-term
    Λ (term (λ (x) x))
    [(λ (_ _) e) #f]
    [(λ (x) e) (equal? (term x) (term e))])
   ))
