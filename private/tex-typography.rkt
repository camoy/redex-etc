#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide language->texexpr
         metafunction->texexpr
         reduction-relation->texexpr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/function
         racket/list
         racket/match
         (only-in redex/pict lw?)
         redex/private/judgment-form
         redex/private/lang-struct
         redex/private/reduction-semantics
         redex/private/struct
         "rewriter.rkt"
         "texexpr.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; language

(define (language->texexpr lang)
  (define langs-vec (compiled-lang-pict-builder lang))
  (define prods (vector-ref langs-vec (sub1 (vector-length langs-vec))))
  (define lines (map production->texexpr prods))
  `(env:plstx () () "\n" ,@lines))

(define (production->texexpr prod)
  (match-define (cons lhs rhs) prod)
  (list ":"
        (add-between (map atomic-rewrite lhs) ",")
        "::="
        (add-between (map lw->texexpr rhs) " | ")
        "\\\\\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metafunction

(define-syntax-rule (metafunction->texexpr mf)
  (metafunction->texexpr/proc (metafunction mf)))

(define (metafunction->texexpr/proc mf)
  (define mf* (metafunction-proc mf))
  (define info (metafunc-proc-pict-info mf*))
  (define name (metafunc-proc-name mf*))
  (match-define (list (list lhs-ctcs rhs-ctcs _) rules) info)
  (define lines (map (curry metafunction-rule->texexpr name) rules))
  `(env:align* () () "\n" ,@lines))

(define (metafunction-rule->texexpr name rule)
  (match-define (list lhs-lw extras rhs-lw) rule)
  (define lhs* (drop-right (drop (lw->texexpr lhs-lw) 1) 1))
  (list (symbol->string name)
        "("
        (add-between lhs* ",")
        ")"
        "&="
        (lw->texexpr rhs-lw)
        "\\\\\n"
        (map metafunction-extra->texexpr extras)))

(define (metafunction-extra->texexpr extra)
  (match extra
    [(metafunc-extra-side-cond expr)
     `("&\\qquad" (text "when ")
                  ,(lw->texexpr expr)
                  "\\\\\n")]
    [(metafunc-extra-where lhs rhs)
     `("&\\qquad" (text "where ")
                  ,(lw->texexpr lhs) "=" ,(lw->texexpr rhs)
                  "\\\\\n")]
    [(metafunc-extra-fresh vars)
     `("&\\qquad" (text "fresh ")
                  ,@vars
                  "\\\\\n")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reduction relation

(define (reduction-relation->texexpr rr)
  (define infos (reduction-relation-lws rr))
  (define lines (map reduction-relation-rule->texexpr infos))
  `(env:align* () () "\n" ,@lines))

;; TODO label
(define (reduction-relation-rule->texexpr info)
  (match-define (rule-pict-info arr lhs rhs label _ extras vars) info)
  (list (lw->texexpr lhs)
        "&"
        (atomic-rewrite arr)
        (lw->texexpr rhs)
        "\\\\\n"
        (map reduction-relation-extra->texexpr extras)))

(define (reduction-relation-extra->texexpr extra)
  (match extra
    [(? lw?)
     `("&\\qquad" (text "when ")
                  ,(lw->texexpr extra)
                  "\\\\\n")]
    [(cons lhs rhs)
     `("&\\qquad" (text "where ")
                  ,(lw->texexpr lhs) "=" ,(lw->texexpr rhs)
                  "\\\\\n")]))
