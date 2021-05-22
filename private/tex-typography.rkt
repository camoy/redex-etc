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
         racket/set
         (only-in redex/pict lw? lw)
         redex/private/judgment-form
         redex/private/lang-struct
         redex/private/reduction-semantics
         redex/private/struct
         "rewriter.rkt"
         "texexpr.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; language

(define (language->texexpr lang #:sets [sets '()] #:set-only [set-only '()])
  (define set-hash (apply hash sets))
  (define set-only-set (list->set set-only))
  (define prods (lang->productions lang))
  (define nts (lang->nonterminals lang))
  (define lines
    (parameterize ([current-nonterminals nts]
                   [current-fallback-atomic-rewriter
                    language-fallback-atomic-rewriter]
                   [current-fallback-compound-rewriter
                    language-fallback-compound-rewriter])
      (filter-map (curry production->texexpr set-hash set-only-set) prods)))
  `(env:plstx "\n" ,@lines))

(define (production->texexpr set-hash set-only-set prod)
  (match-define (cons lhs rhs) prod)
  (define set-name
    (for*/first ([sym (in-list lhs)]
                 [set-name (in-value (hash-ref set-hash sym (λ _ #f)))]
                 #:when set-name)
      set-name))
  (define set-only?
    (for/or ([sym (in-list lhs)])
      (set-member? set-only-set sym)))
  (define lhs* (add-between (map atomic-rewrite lhs) ","))
  (define set-name* (and set-name (format "\\in \\textsf{~a}" set-name)))
  (define rhs* (add-between (map lw->texexpr rhs) " | "))
  (cond
    [(not set-name*) #f]
    [set-only? (set-only->texexpr lhs* set-name*)]
    [else (with-rhs->texexpr lhs* set-name* rhs*)]))

(define (set-only->texexpr lhs set-name)
  `(@ "*:" ,@lhs ,set-name "[ ]  \\\\\n"))

(define (with-rhs->texexpr lhs set-name rhs)
  `(@ ":" ,@lhs ,set-name "::=" ,@rhs "\\\\\n"))

(define (lang->productions lang)
  (define v (compiled-lang-pict-builder lang))
  (cond
    [(vector? v) (vector-ref v (sub1 (vector-length v)))]
    [(list? v) v]))

(define (lang->all-productions lang)
  (define v (compiled-lang-pict-builder lang))
  (cond
    [(vector? v) (vector->list v)]
    [(list? v) (list v)]))

(define (lang->nonterminals lang)
  (define prods (lang->all-productions lang))
  (define nts
    (for*/list ([langs (in-list prods)]
                [prod (in-list langs)])
      (list->set (first prod))))
  (apply set-union (set) nts))

(define current-nonterminals (make-parameter (set)))

(define (language-fallback-atomic-rewriter x)
  (define sym (if (string? x) (string->symbol x) x))
  (define str (if (symbol? x) (symbol->string x) x))
  (match str
    [(regexp #rx"(.+)_(.+)" (list _ base sub))
     (define base* (string->symbol base))
     (define sub* (string->symbol sub))
     (if (eq? sub* '′)
         `(@@ ,(atomic-rewrite base*) "'")
         `(_ ,(atomic-rewrite base*) ,(atomic-rewrite sub*)))]
    [_
     (if (set-member? (current-nonterminals) sym)
         str
         `(texttt ,str))]))

(define (language-fallback-compound-rewriter l mf?)
  (cond
    [mf?
     (match-define (cons x xt) l)
     `(@@ ,x "(" ,@(add-between xt ",") ")")]
    [else
     `(@ ,@(add-between l "\\,"))]))

;;  current-fallback-atomic-rewriter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metafunction

(define-syntax-rule (metafunction->texexpr mf)
  (metafunction->texexpr/proc (metafunction mf)))

(define (metafunction->texexpr/proc mf)
  (define mf* (metafunction-proc mf))
  (define nts (lang->nonterminals (metafunc-proc-lang mf*)))
  (define info (metafunc-proc-pict-info mf*))
  (define name (metafunc-proc-name mf*))
  (match-define (list (list lhs-ctcs rhs-ctcs _) rules) info)
  (define lines
    (parameterize ([current-nonterminals nts]
                   [current-fallback-atomic-rewriter
                    language-fallback-atomic-rewriter]
                   [current-fallback-compound-rewriter
                    language-fallback-compound-rewriter])
      (map (curry metafunction-rule->texexpr name) rules)))
  `(env:align* "\n" ,@lines))

(define (metafunction-rule->texexpr name rule)
  (match-define (list (struct* lw ([e lhs-e])) extras rhs-lw) rule)
  (define lhs* (list->texexpr lhs-e #:rewrite? #f))
  `(@ (@@ ,(atomic-rewrite name)
          "("
          ,@(add-between lhs* ", ")
          ")")
      "&="
      ,(lw->texexpr rhs-lw)
      "\\\\\n"
      ,@(map metafunction-extra->texexpr extras)))

(define (metafunction-extra->texexpr extra)
  (match extra
    [(metafunc-extra-side-cond expr)
     `(@ (@@ "&" (mathbin (phantom "=")))
         (text "when ")
         ,(lw->texexpr expr)
         "\\\\\n")]
    [(metafunc-extra-where lhs rhs)
     `(@ (@@ "&" (mathbin (phantom "=")))
         (text "where ")
         ,(lw->texexpr lhs) "=" ,(lw->texexpr rhs)
         "\\\\\n")]
    [(metafunc-extra-fresh vars)
     `(@ (@@ "&" (mathbin (phantom "=")))
         (text "fresh ")
         ,@vars
         "\\\\\n")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reduction relation

(define (reduction-relation->texexpr rr)
  (define lang (reduction-relation-lang rr))
  (define infos (reduction-relation-lws rr))
  (define nts (lang->nonterminals lang))
  (define lines
    (parameterize ([current-nonterminals nts]
                   [current-fallback-atomic-rewriter
                    language-fallback-atomic-rewriter]
                   [current-fallback-compound-rewriter
                    language-fallback-compound-rewriter])
      (map reduction-relation-rule->texexpr infos)))
  `(env:align* "\n" ,@lines))

;; TODO label
(define (reduction-relation-rule->texexpr info)
  (match-define (rule-pict-info arr lhs rhs label _ extras vars) info)
  `(@ ,(lw->texexpr lhs)
      "&"
      ,(atomic-rewrite arr)
      ,(lw->texexpr rhs)
      "\\\\\n"
      ,@(map reduction-relation-extra->texexpr extras)))

(define (reduction-relation-extra->texexpr extra)
  (match extra
    [(? lw?)
     `(@ (@@ "&" (mathbin (phantom "=")))
         (text "when ")
         ,(lw->texexpr extra)
         "\\\\\n")]
    [(cons lhs rhs)
     `(@ (@@ "&" (mathbin (phantom "=")))
         (text "where ")
         ,(lw->texexpr lhs) "=" ,(lw->texexpr rhs)
         "\\\\\n")]))
