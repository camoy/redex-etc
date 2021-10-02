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
         racket/string
         (only-in redex/pict lw? lw lw-e)
         redex/private/judgment-form
         redex/private/lang-struct
         redex/private/reduction-semantics
         redex/private/struct
         "rewriter.rkt"
         "texexpr.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; language

(define (language->texexpr lang
                           #:extends [extends #f]
                           #:sets [set-hash (hash)]
                           #:set-only [set-only-hash (hash)])
  (define name (compiled-lang-language-name lang))
  (define extended-name
    (and extends
         (string-titlecase
          (symbol->string (compiled-lang-language-name extends)))))
  (define prods (lang->productions lang))
  (define nts (lang->nonterminals lang))
  (define lines
    (parameterize ([current-nonterminals nts]
                   [current-fallback-atomic-rewriter
                    language-fallback-atomic-rewriter]
                   [current-fallback-compound-rewriter
                    language-fallback-compound-rewriter])
      (filter-map (curry production->texexpr set-hash set-only-hash) prods)))
  `(@@ footnotesize
       "\n"
       (fbox ,(format "~a Syntax" (string-titlecase (symbol->string name))))
       ,(if extended-name (format " extends ~a Syntax" extended-name) "")
       "\n"
       (env:plstx "\n" ,@lines)))

(define (production->texexpr set-hash set-only-hash prod)
  (match-define (cons lhs rhs) prod)
  (define set-name
    (for*/first ([sym (in-list lhs)]
                 [set-name (in-value (hash-ref set-hash sym (λ _ #f)))]
                 #:when set-name)
      set-name))
  (define set-only
    (for/first ([sym (in-list lhs)]
             #:when (hash-has-key? set-only-hash sym))
      (or (hash-ref set-only-hash sym) 'none)))
  (define lhs* (add-between (map atomic-rewrite lhs) ","))
  (define set-name* (and set-name (format "\\in \\textsf{~a}" set-name)))
  (define rhs* (add-between (map lw->texexpr rhs) " | "))
  (cond
    [(not set-name*) #f]
    [set-only (set-only->texexpr lhs* set-name* set-only)]
    [else (with-rhs->texexpr lhs* set-name* rhs*)]))

(define (set-only->texexpr lhs set-name set-only)
  (if (eq? set-only 'none)
      `(@ "*:" ,@lhs ,set-name "[ ]  \\\\\n")
      `(@ "*:" ,@lhs ,set-name "[=]" ,set-only "\\\\\n")))

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
(define non-tt? (make-parameter #f))

(define (language-fallback-atomic-rewriter x)
  (define sym (if (string? x) (string->symbol x) x))
  (define str (if (symbol? x) (symbol->string x) x))
  (match str
    [(regexp #rx"(.+)_(.+)" (list _ base sub))
     (define base* (string->symbol base))
     (define sub* (string->symbol sub))
     (if (eq? sub* '′)
         `(@@ ,(atomic-rewrite base*) "'")
         `(_ ,(atomic-rewrite base*)
             ,(parameterize ([non-tt? #t])
                (atomic-rewrite sub*))))]
    [_
     (if (or (set-member? (current-nonterminals) sym) (non-tt?))
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
  (define name (atomic-rewrite (metafunc-proc-name mf*)))
  (match-define (list (list lhs-ctcs rhs-ctcs _) rules) info)
  (parameterize ([current-nonterminals nts]
                 [current-fallback-atomic-rewriter
                  language-fallback-atomic-rewriter]
                 [current-fallback-compound-rewriter
                  language-fallback-compound-rewriter])
    (define contract (metafunction-ctc name lhs-ctcs rhs-ctcs))
    (define lines (map (curry metafunction-rule->texexpr name) rules))
    `(@@ footnotesize
         "\n"
         (fbox ,contract)
         "\n"
         (env:align* "\n" ,@lines))))

(define (metafunction-ctc name lhs-ctcs rhs-ctcs)
  (define lhs-ctcs* (list->texexpr lhs-ctcs))
  (define rhs-ctcs* (list->texexpr rhs-ctcs))
  `($ ,name ":" ,@(add-between lhs-ctcs* 'times) to ,@rhs-ctcs*))

(define (metafunction-rule->texexpr name rule)
  (match-define (list (struct* lw ([e lhs-e])) extras rhs-lw) rule)
  (define lhs* (list->texexpr lhs-e #:rewrite? #f))
  `(@ (@@ ,name
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
  (define lhs-ctc (reduction-relation-domain-pat rr))
  (define rhs-ctc (reduction-relation-codomain-pat rr))
  (parameterize ([current-nonterminals nts]
                 [current-fallback-atomic-rewriter
                  language-fallback-atomic-rewriter]
                 [current-fallback-compound-rewriter
                  language-fallback-compound-rewriter])
    (define arr (rule-pict-info-arrow (first infos)))
    (define lines (map reduction-relation-rule->texexpr infos))
    (define contract (reduction-relation-ctc arr lhs-ctc rhs-ctc))
    `(@@ footnotesize
         "\n"
         (fbox ,contract)
         "\n"
         (env:align* "\n" ,@(add-between lines "\\\\\n") "\n"))))

(define (reduction-relation-ctc arr lhs rhs)
  `($ ,(atomic-rewrite (second lhs))
      ,(atomic-rewrite arr)
      ,(atomic-rewrite (second rhs))))

(define (newline? clauses)
  (and (not (empty? clauses))
       (match (first clauses)
         [(struct* lw ([e (list _ _ (struct* lw ([e 'newline])))])) #t]
         [_ #f])))

;; TODO label
(define (reduction-relation-rule->texexpr info)
  (match-define (rule-pict-info arr lhs rhs label _ extras vars) info)
  (define arr* (atomic-rewrite arr))
  (define nl? (newline? extras))
  (define extras*
    (if nl?
        (map (curry reduction-relation-extra->texexpr arr*) (rest extras))
        (map (curry reduction-relation-extra->texexpr arr*) extras)))
  #;`(@ (textsc ,(symbol->string label))
      "&&"
      ,(lw->texexpr lhs)
      "&"
      ,arr*
      ,(lw->texexpr rhs)
      ,@(if (empty? extras) '() '((text " if ")))
      ,@(add-between extras* ", "))
  #;`(@ (textsc ,(symbol->string label))
      "&&"
      ,(lw->texexpr lhs)
      "&"
      ,arr*
      ,(lw->texexpr rhs)
      ,@(if (empty? extras)
            '()
            `("\\\\\n" "&&&" (mathbin (phantom ,arr)) (text "if ")))
      ,@(add-between extras* ", ")
      "\\\\\n")
  #;`(@ (textsc ,(symbol->string label))
      "&&"
      ,(lw->texexpr lhs)
      "&"
      ,arr*
      ,(lw->texexpr rhs)
      "&"
      ,@(if (empty? extras) '() '((text "if ") "&"))
      ,@(add-between extras* "\\\\\n&&&&&")
      "\\\\\n")
  (if nl?
      `(@ ,(lw->texexpr lhs)
          "&"
          ,arr*
          ,(lw->texexpr rhs)
          "&&" (textsc ,(symbol->string label))
          "\\\\\n" "&" (mathbin (phantom ,arr)) (text "if ")
          ,@(add-between extras* ","))
      `(@ ,(lw->texexpr lhs)
          "&"
          ,arr*
          ,(lw->texexpr rhs)
          ,(if (empty? extras*) "" `(text " if "))
          ,@(add-between extras* ",")
          "&&" (textsc ,(symbol->string label)))))

(define (reduction-relation-extra->texexpr arr extra)
  (match extra
    [(? lw?)
     (lw->texexpr extra)
     #;`(@ (@@ "&" (mathbin (phantom ,arr)))
         (text "when ")
         ,(lw->texexpr extra))]
    [(cons lhs rhs)
     `(@@ ,(lw->texexpr lhs) "=" ,(lw->texexpr rhs))
     #;`(@ (@@ "&" (mathbin (phantom ,arr)))
         (text "where ")
         ,(lw->texexpr lhs) "=" ,(lw->texexpr rhs))]
    [_ #f]))
