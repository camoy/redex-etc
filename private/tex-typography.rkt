#lang racket/base

(require racket/match
         racket/list
         racket/string
         racket/function
         (prefix-in pict: redex/pict)
         redex/private/reduction-semantics
         redex/private/struct
         redex/private/lang-struct
         redex/private/judgment-form
         "texexpr.rkt"
         "../../trace-contract/model/evaluation.rkt"
         "../../trace-contract/model/semantics.rkt")

;; (lw e line line-span column column-span unq? metafunction?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewriters

(struct rewriter (symbol proc))
(struct compound-rewriter rewriter ())
(struct atomic-rewriter rewriter ())
(struct unquote-rewriter rewriter ())

(struct rewriters (compounds atomics unquotes))

(define (make-rewriters . rws)
  (for/fold ([compounds (hash)]
             [atomics (hash)]
             [unquotes (hash)]
             #:result (rewriters compounds atomics unquotes))
            ([rw (in-list rws)])
    (match rw
      [(compound-rewriter symbol proc)
       (values (hash-set compounds symbol proc) atomics unquotes)]
      [(atomic-rewriter symbol proc)
       (values compounds (hash-set atomics symbol proc) unquotes)]
      [(unquote-rewriter symbol proc)
       (values compounds atomics (hash-set unquotes symbol proc))])))

(define current-rewriters
  (make-parameter (make-rewriters)))

(define (default-fallback-compound-rewriter l)
  `("(" ,@l ")"))

(define current-fallback-compound-rewriter
  (make-parameter default-fallback-compound-rewriter))

(define (compound-rewrite head args)
  (define compounds (rewriters-compounds (current-rewriters)))
  (define proc (hash-ref compounds (pict:lw-e head) (λ _ #f)))
  (if proc
      (apply proc (list->sexp args))
      ((current-fallback-compound-rewriter)
       (list->sexp (cons head args) #:rewrite? #f))))

(define (default-fallback-atomic-rewriter l)
  l)

(define current-fallback-atomic-rewriter
  (make-parameter default-fallback-atomic-rewriter))

(define (atomic-rewrite sym)
  (define atomics (rewriters-atomics (current-rewriters)))
  (define proc (hash-ref atomics sym (λ _ #f)))
  (if proc
      (proc sym)
      ((current-fallback-atomic-rewriter)
       (symbol->string sym))))

(define (default-fallback-unquote-rewriter l)
  `("(" ,@l ")"))

(define current-fallback-unquote-rewriter
  (make-parameter default-fallback-unquote-rewriter))

(define (unquote-rewrite l)
  (match (filter pict:lw? l)
    [(or (list (struct* pict:lw ([e "("])) x xt ... (struct* pict:lw ([e ")"])))
         (list (struct* pict:lw ([e "["])) x xt ... (struct* pict:lw ([e "]"])))
         (list (struct* pict:lw ([e "{"])) x xt ... (struct* pict:lw ([e "}"]))))
     (define unquotes (rewriters-unquotes (current-rewriters)))
     (define proc (hash-ref unquotes (pict:lw-e x) (λ _ #f)))
     (if proc
         (apply proc (list->sexp xt))
         ((current-fallback-unquote-rewriter)
          (list->sexp (cons x xt))))]
    [l* (map lw->sexp l*)]))

;;
;; lw
;;

(define (lw->sexp l)
  (match-define (struct* pict:lw ([e e] [unq? unq?])) l)
  (match e
    ["" null]
    [(? string? e) e]
    [(? symbol? e) (atomic-rewrite e)]
    [(? list? e)
     (if unq?
         (unquote-rewrite e)
         (list->sexp e))]))

(define (list->sexp l #:rewrite? [rw? #t])
  (match (filter pict:lw? l)
    [(or (list (struct* pict:lw ([e "("])) lws ... (struct* pict:lw ([e ")"])))
         (list (struct* pict:lw ([e "["])) lws ... (struct* pict:lw ([e "]"])))
         (list (struct* pict:lw ([e "{"])) lws ... (struct* pict:lw ([e "}"]))))
     (cond
       [(empty? lws) null]
       [rw? (compound-rewrite (first lws) (rest lws))]
       [else (map lw->sexp lws)])]
    [l* (map lw->sexp l*)]))

;;
;; language
;;

(define (language->sexp lang)
  (define langs-vec (compiled-lang-pict-builder evaluation))
  (define prods (vector-ref langs-vec (sub1 (vector-length langs-vec))))
  (define lines (map production->sexp prods))
  `(env:plstx () () "\n" ,@lines))

(define (production->sexp prod)
  (match-define (cons lhs rhs) prod)
  (list ":"
        (add-between (map atomic-rewrite lhs) ",")
        "::="
        (add-between (map lw->sexp rhs) " | ")
        "\\\\\n"))

;;
;; metafunction
;;

(define-syntax-rule (metafunction->sexp mf)
  (metafunction->sexp/proc (metafunction mf)))

(define (metafunction->sexp/proc mf)
  (define mf* (metafunction-proc mf))
  (define info (metafunc-proc-pict-info mf*))
  (define name (metafunc-proc-name mf*))
  (match-define (list (list lhs-ctcs rhs-ctcs _) rules) info)
  (define lines (map (curry metafunction-rule->sexp name) rules))
  `(env:align* () () "\n" ,@lines))

(define (metafunction-rule->sexp name rule)
  (match-define (list lhs-lw extras rhs-lw) rule)
  (define lhs* (drop-right (drop (lw->sexp lhs-lw) 1) 1))
  (list (symbol->string name)
        "("
        (add-between lhs* ",")
        ")"
        "&="
        (lw->sexp rhs-lw)
        "\\\\\n"
        (map metafunction-extra->sexp extras)))

(define (metafunction-extra->sexp extra)
  (match extra
    [(metafunc-extra-side-cond expr)
     `("&\\qquad" (text "when ")
                  ,(lw->sexp expr)
                  "\\\\\n")]
    [(metafunc-extra-where lhs rhs)
     `("&\\qquad" (text "where ")
                  ,(lw->sexp lhs) "=" ,(lw->sexp rhs)
                  "\\\\\n")]
    [(metafunc-extra-fresh vars)
     `("&\\qquad" (text "fresh ")
                  ,@vars
                  "\\\\\n")]))

;;
;; reduction relation
;;

(define (reduction-relation->sexp rr)
  (define infos (reduction-relation-lws rr))
  (define lines (map reduction-relation-rule->sexp infos))
  `(env:align* () () "\n" ,@lines))

;; TODO label
(define (reduction-relation-rule->sexp info)
  (match-define (rule-pict-info arr lhs rhs label _ extras vars) info)
  (list (lw->sexp lhs)
        "&"
        (atomic-rewrite arr)
        (lw->sexp rhs)
        "\\\\\n"
        (map reduction-relation-extra->sexp extras)))

(define (reduction-relation-extra->sexp extra)
  (match extra
    [(? pict:lw?)
     `("&\\qquad" (text "when ")
                  ,(lw->sexp extra)
                  "\\\\\n")]
    [(cons lhs rhs)
     `("&\\qquad" (text "where ")
                  ,(lw->sexp lhs) "=" ,(lw->sexp rhs)
                  "\\\\\n")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(require racket/pretty)

(define ~ (compound-rewriter '~ (λ (x y) `("\\langle" ,x "," ,y "\\rangle"))))
(define α (atomic-rewriter 'α (λ _ "\\alpha")))
(define :-> (atomic-rewriter ':-> (λ _ "\\longmapsto")))

#;(parameterize ([current-rewriters (make-rewriters ~ α)])
    (displayln (texexpr->string (language->sexp evaluation))))
#;(parameterize ([current-rewriters (make-rewriters ~ α)])
  (displayln (texexpr->string (metafunction->sexp δ_1))))
(parameterize ([current-rewriters (make-rewriters ~ α :->)])
  (displayln (texexpr->string (reduction-relation->sexp ↦))))

;;
;; LANGUAGE
;;

#|

(compiled-lang-pict-builder evaluation)

(vector
 (list
  (list
   '(e)
   (lw 'v 4 0 3 1 #f #f)
   (lw
    (list
     (lw "(" 5 0 3 1 #f #f)
     (lw '⊖ 5 0 4 1 #f #f)
     (lw 'e 5 0 6 1 #f #f)
     (lw ")" 5 0 7 1 #f #f))
    5
    0
    3
    5
    #f
    #f)
   (lw
    (list
     (lw "(" 6 0 3 1 #f #f)
     (lw '⊕ 6 0 4 1 #f #f)
     (lw 'e 6 0 6 1 #f #f)
     (lw 'e 6 0 8 1 #f #f)
     (lw ")" 6 0 9 1 #f #f))
    6
    0
    3
    7
    #f
    #f)
   (lw
    (list
     (lw "(" 7 0 3 1 #f #f)
     (lw 'if 7 0 4 2 #f #f)
     (lw 'e 7 0 7 1 #f #f)
     (lw 'e 7 0 9 1 #f #f)
     (lw 'e 7 0 11 1 #f #f)
     (lw ")" 7 0 12 1 #f #f))
    7
    0
    3
    10
    #f
    #f)
   (lw
    (list
     (lw "(" 8 0 3 1 #f #f)
     (lw 'e 8 0 4 1 #f #f)
     (lw 'e 8 0 6 1 #f #f)
     (lw ")" 8 0 7 1 #f #f))
    8
    0
    3
    5
    #f
    #f)
   (lw
    (list
     (lw "(" 9 0 3 1 #f #f)
     (lw 'mon 9 0 4 3 #f #f)
     (lw 'j 9 0 8 1 #f #f)
     (lw 'k 9 0 10 1 #f #f)
     (lw 'l 9 0 12 1 #f #f)
     (lw 'e 9 0 14 1 #f #f)
     (lw 'e 9 0 16 1 #f #f)
     (lw ")" 9 0 17 1 #f #f))
    9
    0
    3
    15
    #f
    #f))
  (list
   '(v)
   (lw 'b 12 0 3 1 #f #f)
   (lw 'x 13 0 3 1 #f #f)
   (lw 'f 14 0 3 1 #f #f)
   (lw 't 15 0 3 1 #f #f)
   (lw 'κ 16 0 3 1 #f #f))
  (list '(b) (lw 'true 19 0 3 4 #f #f) (lw 'false 20 0 3 5 #f #f))
  (list '(x y z) (lw 'variable-not-otherwise-mentioned 21 0 18 32 #f #f))
  (list
   '(f)
   (lw
    (list
     (lw "(" 23 0 3 1 #f #f)
     (lw 'λ 23 0 4 1 #f #f)
     (lw 'x 23 0 6 1 #f #f)
     (lw 'e 23 0 8 1 #f #f)
     (lw ")" 23 0 9 1 #f #f))
    23
    0
    3
    7
    #f
    #f))
  (list '(t) (lw 'null 25 0 3 4 #f #f))
  (list '(κ) (lw 'true 27 0 3 4 #f #f) (lw 'false 28 0 3 5 #f #f))
  (list
   '(⊖)
   (lw 'null? 31 0 3 5 #f #f)
   (lw 'head 32 0 3 4 #f #f)
   (lw 'tail 33 0 3 4 #f #f)
   (lw 'ref 34 0 3 3 #f #f)
   (lw '! 35 0 3 1 #f #f)
   (lw '? 36 0 3 1 #f #f))
  (list
   '(⊕)
   (lw ':: 38 0 3 2 #f #f)
   (lw '@ 39 0 3 1 #f #f)
   (lw ':= 40 0 3 2 #f #f)
   (lw '->i 41 0 3 3 #f #f)
   (lw '>>t 42 0 3 3 #f #f))
  (list '(j k l) (lw 'string 43 0 18 6 #f #f)))
 (list
  (list
   '(ς)
   (lw
    (list
     (lw "(" 4 0 3 1 #f #f)
     (lw '~ 4 0 4 1 #f #f)
     (lw 'e 4 0 6 1 #f #f)
     (lw 'Σ 4 0 8 1 #f #f)
     (lw ")" 4 0 9 1 #f #f))
    4
    0
    3
    7
    #f
    #f))
  (list
   '(e)
   (lw '.... 7 0 3 4 #f #f)
   (lw
    (list
     (lw "(" 8 0 3 1 #f #f)
     (lw 'mon 8 0 4 3 #f #f)
     (lw 'j 8 0 8 1 #f #f)
     (lw 'k 8 0 10 1 #f #f)
     (lw 'e 8 0 12 1 #f #f)
     (lw 'e 8 0 14 1 #f #f)
     (lw ")" 8 0 15 1 #f #f))
    8
    0
    3
    13
    #f
    #f)
   (lw
    (list
     (lw "(" 9 0 3 1 #f #f)
     (lw 'err 9 0 4 3 #f #f)
     (lw 'j 9 0 8 1 #f #f)
     (lw 'k 9 0 10 1 #f #f)
     (lw ")" 9 0 11 1 #f #f))
    9
    0
    3
    9
    #f
    #f))
  (list
   '(v)
   (lw '.... 11 0 3 4 #f #f)
   (lw 'α 12 0 3 1 #f #f)
   (lw 'l 13 0 3 1 #f #f))
  (list
   '(t)
   (lw '.... 15 0 3 4 #f #f)
   (lw
    (list
     (lw "(" 16 0 3 1 #f #f)
     (lw 'cons 16 0 4 4 #f #f)
     (lw 'v 16 0 9 1 #f #f)
     (lw 't 16 0 11 1 #f #f)
     (lw ")" 16 0 12 1 #f #f))
    16
    0
    3
    10
    #f
    #f))
  (list
   '(κ)
   (lw '.... 18 0 3 4 #f #f)
   (lw
    (list
     (lw "(" 19 0 3 1 #f #f)
     (lw 'flat 19 0 4 4 #f #f)
     (lw 'v 19 0 9 1 #f #f)
     (lw ")" 19 0 10 1 #f #f))
    19
    0
    3
    8
    #f
    #f)
   (lw
    (list
     (lw "(" 20 0 3 1 #f #f)
     (lw 'arr 20 0 4 3 #f #f)
     (lw 'v 20 0 8 1 #f #f)
     (lw 'v 20 0 10 1 #f #f)
     (lw ")" 20 0 11 1 #f #f))
    20
    0
    3
    9
    #f
    #f)
   (lw
    (list
     (lw "(" 21 0 3 1 #f #f)
     (lw 'trace 21 0 4 5 #f #f)
     (lw 'v 21 0 10 1 #f #f)
     (lw 'v 21 0 12 1 #f #f)
     (lw ")" 21 0 13 1 #f #f))
    21
    0
    3
    11
    #f
    #f)
   (lw
    (list
     (lw "(" 22 0 3 1 #f #f)
     (lw 'coll 22 0 4 4 #f #f)
     (lw 'v 22 0 9 1 #f #f)
     (lw 'v 22 0 11 1 #f #f)
     (lw ")" 22 0 12 1 #f #f))
    22
    0
    3
    10
    #f
    #f))
  (list
   '(f)
   (lw '.... 24 0 3 4 #f #f)
   (lw
    (list
     (lw "(" 25 0 3 1 #f #f)
     (lw 'grd 25 0 4 3 #f #f)
     (lw 'j 25 0 8 1 #f #f)
     (lw 'k 25 0 10 1 #f #f)
     (lw 'v 25 0 12 1 #f #f)
     (lw 'v 25 0 14 1 #f #f)
     (lw ")" 25 0 15 1 #f #f))
    25
    0
    3
    13
    #f
    #f))
  (list
   '(Σ)
   (lw
    (list
     (lw "(" 27 0 16 1 #f #f)
     (lw
      (list
       (lw "[" 27 0 17 1 #f #f)
       (lw 'α 27 0 18 1 #f #f)
       (lw 'v 27 0 20 1 #f #f)
       (lw "]" 27 0 21 1 #f #f))
      27
      0
      17
      5
      #f
      #f)
     (lw '... 27 0 23 3 #f #f)
     (lw ")" 27 0 26 1 #f #f))
    27
    0
    16
    11
    #f
    #f))
  (list '(α) (lw 'natural 28 0 15 7 #f #f))
  (list
   '(r)
   (lw 'v 30 0 13 1 #f #f)
   (lw
    (list
     (lw "(" 30 0 15 1 #f #f)
     (lw 'err 30 0 16 3 #f #f)
     (lw 'j 30 0 20 1 #f #f)
     (lw 'k 30 0 22 1 #f #f)
     (lw ")" 30 0 23 1 #f #f))
    30
    0
    15
    9
    #f
    #f))
  (list
   '(a)
   (lw 'b 32 0 3 1 #f #f)
   (lw 'function 33 0 3 8 #f #f)
   (lw 'list 34 0 3 4 #f #f)
   (lw 'contract 35 0 3 8 #f #f)
   (lw 'reference 36 0 3 9 #f #f)
   (lw
    (list
     (lw "(" 37 0 3 1 #f #f)
     (lw 'err 37 0 4 3 #f #f)
     (lw 'j 37 0 8 1 #f #f)
     (lw 'k 37 0 10 1 #f #f)
     (lw ")" 37 0 11 1 #f #f))
    37
    0
    3
    9
    #f
    #f))
  (list
   '(E)
   (lw 'hole 40 0 3 4 #f #f)
   (lw
    (list
     (lw "(" 41 0 3 1 #f #f)
     (lw '⊖ 41 0 4 1 #f #f)
     (lw 'E 41 0 6 1 #f #f)
     (lw ")" 41 0 7 1 #f #f))
    41
    0
    3
    5
    #f
    #f)
   (lw
    (list
     (lw "(" 42 0 3 1 #f #f)
     (lw '⊕ 42 0 4 1 #f #f)
     (lw 'E 42 0 6 1 #f #f)
     (lw 'e 42 0 8 1 #f #f)
     (lw ")" 42 0 9 1 #f #f))
    42
    0
    3
    7
    #f
    #f)
   (lw
    (list
     (lw "(" 43 0 3 1 #f #f)
     (lw '⊕ 43 0 4 1 #f #f)
     (lw 'v 43 0 6 1 #f #f)
     (lw 'E 43 0 8 1 #f #f)
     (lw ")" 43 0 9 1 #f #f))
    43
    0
    3
    7
    #f
    #f)
   (lw
    (list
     (lw "(" 44 0 3 1 #f #f)
     (lw 'if 44 0 4 2 #f #f)
     (lw 'E 44 0 7 1 #f #f)
     (lw 'e 44 0 9 1 #f #f)
     (lw 'e 44 0 11 1 #f #f)
     (lw ")" 44 0 12 1 #f #f))
    44
    0
    3
    10
    #f
    #f)
   (lw
    (list
     (lw "(" 45 0 3 1 #f #f)
     (lw 'E 45 0 4 1 #f #f)
     (lw 'e 45 0 6 1 #f #f)
     (lw ")" 45 0 7 1 #f #f))
    45
    0
    3
    5
    #f
    #f)
   (lw
    (list
     (lw "(" 46 0 3 1 #f #f)
     (lw 'v 46 0 4 1 #f #f)
     (lw 'E 46 0 6 1 #f #f)
     (lw ")" 46 0 7 1 #f #f))
    46
    0
    3
    5
    #f
    #f)
   (lw
    (list
     (lw "(" 47 0 3 1 #f #f)
     (lw 'mon 47 0 4 3 #f #f)
     (lw 'j 47 0 8 1 #f #f)
     (lw 'k 47 0 10 1 #f #f)
     (lw 'E 47 0 12 1 #f #f)
     (lw 'e 47 0 14 1 #f #f)
     (lw ")" 47 0 15 1 #f #f))
    47
    0
    3
    13
    #f
    #f)
   (lw
    (list
     (lw "(" 48 0 3 1 #f #f)
     (lw 'mon 48 0 4 3 #f #f)
     (lw 'j 48 0 8 1 #f #f)
     (lw 'k 48 0 10 1 #f #f)
     (lw 'v 48 0 12 1 #f #f)
     (lw 'E 48 0 14 1 #f #f)
     (lw ")" 48 0 15 1 #f #f))
    48
    0
    3
    13
    #f
    #f))))
|#

;;
;; REDUCTION RELATION
;;

#|
(define rpis (reduction-relation-lws ↦))
(define rpi (car rpis))

(rule-pict-info-arrow rpi)
(rule-pict-info-lhs rpi)
(rule-pict-info-rhs rpi)
(rule-pict-info-label rpi)
(rule-pict-info-computed-label rpi)
(rule-pict-info-side-conditions/pattern-binds rpi)
(rule-pict-info-fresh-vars rpi)

':->
(lw
 (list
  (lw "(" 64 0 8 1 #f #f)
  (lw '~ 64 0 9 1 #f #f)
  (lw
   (list
    (lw "(" 64 0 11 1 #f #f)
    (lw 'in-hole 64 0 12 7 #f #f)
    (lw 'E 64 0 20 1 #f #f)
    (lw
     (list
      (lw "(" 64 0 22 1 #f #f)
      (lw '⊖ 64 0 23 1 #f #f)
      (lw 'v_1 64 0 25 3 #f #f)
      (lw ")" 64 0 28 1 #f #f))
     64
     0
     22
     7
     #f
     #f)
    (lw ")" 64 0 29 1 #f #f))
   64
   0
   11
   19
   #f
   #f)
  (lw 'Σ 64 0 31 1 #f #f)
  (lw ")" 64 0 32 1 #f #f))
 64
 0
 8
 25
 #f
 #f)
(lw
 (list
  (lw "(" 65 0 8 1 #f #f)
  (lw '~ 65 0 9 1 #f #f)
  (lw
   (list
    (lw "(" 65 0 11 1 #f #f)
    (lw 'in-hole 65 0 12 7 #f #f)
    (lw 'E 65 0 20 1 #f #f)
    (lw 'r 65 0 22 1 #f #f)
    (lw ")" 65 0 23 1 #f #f))
   65
   0
   11
   13
   #f
   #f)
  (lw 'Σ_′ 65 0 25 3 #f #f)
  (lw ")" 65 0 28 1 #f #f))
 65
 0
 8
 21
 #f
 #f)
'δ_1
#f
(list
 (cons
  (lw
   (list
    (lw "(" 66 0 15 1 #f #f)
    (lw 'r 66 0 16 1 #f #f)
    (lw 'Σ_′ 66 0 18 3 #f #f)
    (lw ")" 66 0 21 1 #f #f))
   66
   0
   15
   7
   #f
   #f)
  (lw
   (list
    (lw "" 66 0 23 0 #f #f)
    'spring
    (lw
     (list
      (lw "(" 66 0 24 1 #t #f)
      (lw 'dont-cache 66 0 25 10 #t #f)
      (lw
       (list
        (lw "(" 66 0 36 1 #t #f)
        (lw 'δ_1 66 0 37 3 #f #t)
        (lw '⊖ 66 0 41 1 #t #f)
        (lw 'v_1 66 0 43 3 #t #f)
        (lw 'Σ 66 0 47 1 #t #f)
        (lw ")" 66 0 48 1 #t #f))
       66
       0
       36
       13
       #t
       #f)
      (lw ")" 66 0 49 1 #t #f))
     66
     0
     24
     26
     #t
     #f))
   66
   0
   23
   27
   #f
   #f)))
'()

|#

;;
;; METAFUNCTION
;;

#|
(define p (metafunction-proc (metafunction res->ans)))
(metafunc-proc-clause-names p)
(metafunc-proc-pict-info p)
(metafunc-proc-lang p)
(metafunc-proc-multi-arg? p)
(metafunc-proc-name p)
(metafunc-proc-in-dom? p)
(metafunc-proc-dom-pat p)
(metafunc-proc-cases p)
(metafunc-proc-gen-clauses p)

#<language: evaluation>
#<reduction-relation>
'(#f #f #f #f #f #f)
(list
 (list (list (lw 'r 186 0 13 1 #f #f)) (list (lw 'a 186 0 18 1 #f #f)) '())
 (list
  (list
   (lw
    (list
     (lw "(" 187 0 3 1 #f #f)
     (lw 'b 187 0 13 1 #f #f)
     (lw ")" 187 0 14 1 #f #f))
    187
    0
    3
    12
    #f
    #f)
   '()
   (lw 'b 187 0 16 1 #f #f))
  (list
   (lw
    (list
     (lw "(" 188 0 3 1 #f #f)
     (lw 'f 188 0 13 1 #f #f)
     (lw ")" 188 0 14 1 #f #f))
    188
    0
    3
    12
    #f
    #f)
   '()
   (lw 'function 188 0 16 8 #f #f))
  (list
   (lw
    (list
     (lw "(" 189 0 3 1 #f #f)
     (lw 't 189 0 13 1 #f #f)
     (lw ")" 189 0 14 1 #f #f))
    189
    0
    3
    12
    #f
    #f)
   '()
   (lw 'list 189 0 16 4 #f #f))
  (list
   (lw
    (list
     (lw "(" 190 0 3 1 #f #f)
     (lw 'κ 190 0 13 1 #f #f)
     (lw ")" 190 0 14 1 #f #f))
    190
    0
    3
    12
    #f
    #f)
   '()
   (lw 'contract 190 0 16 8 #f #f))
  (list
   (lw
    (list
     (lw "(" 191 0 3 1 #f #f)
     (lw 'α 191 0 13 1 #f #f)
     (lw ")" 191 0 14 1 #f #f))
    191
    0
    3
    12
    #f
    #f)
   '()
   (lw 'reference 191 0 16 9 #f #f))
  (list
   (lw
    (list
     (lw "(" 192 0 3 1 #f #f)
     (lw
      (list
       (lw "(" 192 0 13 1 #f #f)
       (lw 'err 192 0 14 3 #f #f)
       (lw 'j 192 0 18 1 #f #f)
       (lw 'k 192 0 20 1 #f #f)
       (lw ")" 192 0 21 1 #f #f))
      192
      0
      13
      9
      #f
      #f)
     (lw ")" 192 0 22 1 #f #f))
    192
    0
    3
    20
    #f
    #f)
   '()
   (lw
    (list
     (lw "(" 192 0 24 1 #f #f)
     (lw 'err 192 0 25 3 #f #f)
     (lw 'j 192 0 29 1 #f #f)
     (lw 'k 192 0 31 1 #f #f)
     (lw ")" 192 0 32 1 #f #f))
    192
    0
    24
    9
    #f
    #f))))
#<language: evaluation>
#t
'res->ans
#<procedure:res->ans>
'(list (nt r))
(list
 (metafunc-case
  #<procedure:...ction-semantics.rkt:1638:22>
  '(list (name b (nt b)))
  #<procedure:...ction-semantics.rkt:1570:40>
  "<pkgs>/trace-contract/model/semantics.rkt:187:4"
  'g39086)
 (metafunc-case
  #<procedure:...ction-semantics.rkt:1638:22>
  '(list (name f (nt f)))
  #<procedure:...ction-semantics.rkt:1570:40>
  "<pkgs>/trace-contract/model/semantics.rkt:188:4"
  'g39087)
 (metafunc-case
  #<procedure:...ction-semantics.rkt:1638:22>
  '(list (name t (nt t)))
  #<procedure:...ction-semantics.rkt:1570:40>
  "<pkgs>/trace-contract/model/semantics.rkt:189:4"
  'g39088)
 (metafunc-case
  #<procedure:...ction-semantics.rkt:1638:22>
  '(list (name κ (nt κ)))
  #<procedure:...ction-semantics.rkt:1570:40>
  "<pkgs>/trace-contract/model/semantics.rkt:190:4"
  'g39089)
 (metafunc-case
  #<procedure:...ction-semantics.rkt:1638:22>
  '(list (name α (nt α)))
  #<procedure:...ction-semantics.rkt:1570:40>
  "<pkgs>/trace-contract/model/semantics.rkt:191:4"
  'g39090)
 (metafunc-case
  #<procedure:...ction-semantics.rkt:1638:22>
  '(list (list err (name j (nt j)) (name k (nt j))))
  #<procedure:...ction-semantics.rkt:1570:40>
  "<pkgs>/trace-contract/model/semantics.rkt:192:4"
  'g39091))
#<procedure:...ction-semantics.rkt:1665:4>

|#
