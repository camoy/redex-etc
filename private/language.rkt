#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require private-in)
(require (only-in (private-in redex/private/core-layout)
                  adjust
                  find-enclosing-loc-wrapper
                  lw->pict)
         (only-in (private-in redex/private/pict)
                  save-as-ps/pdf
                  pict-info->all-nonterminals
                  check-non-terminals
                  remove-unwanted-nts
                  flatten-grammar-info
                  nts->str
                  sequence-of-non-terminals
                  make-bar
                  make-::=
                  basic-text)
         (for-syntax racket/base
                     racket/match
                     racket/struct-info
                     racket/syntax
                     syntax/parse)
         racket/class
         racket/draw
         racket/match
         redex/private/lang-struct
         redex/reduction-semantics
         (except-in redex/pict
                    lw->pict)
         pict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `define-language/style`

(struct stylish-compiled-lang compiled-lang (sets))

(begin-for-syntax
  (define-splicing-syntax-class clause
    #:datum-literals (∈ ::=)
    (pattern [nt:id ... ∈ s:id ::= rhs ...]
             #:with set-only #'#f)
    (pattern (~seq #:set-only [nt:id ... ∈ s:id ::= rhs ...])
             #:with set-only #'#t)))

(define-syntax (define-language/style stx)
  (syntax-parse stx
    [(_ ?name:id ?c:clause ... (~optional (~seq #:binding-forms ?b ...)))
     #:with ?name* (generate-temporary #'?name)
     #'(begin
         (define-language ?name*
           [?c.nt ... ::= ?c.rhs ...] ...
           (~? (~@ #:binding-forms ?b ...)))
         (define ?name
           (apply stylish-compiled-lang
                  (append (struct->list/opaque compiled-lang ?name*)
                          (list (hash (~@ '(?c.nt ...)
                                          (cons '?c.s ?c.set-only))
                                          ...))))))]))

(define-syntax (struct->list/opaque stx)
  (syntax-parse stx
    [(_ ?s:id ?e:expr)
     (define si (syntax-local-value #'?s (λ _ #f)))
     (unless si
       (raise-syntax-error 'struct->list/opaque
                           "not bound to a struct identifier"
                           #'?s))
     (match-define (list desc mk pred accs muts sup)
       (extract-struct-info si))
     (define accs* (reverse accs))
     #`(let ([v ?e])
         (map (λ (f) (f v)) (list #,@accs*)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `render-language/style`

(define (render-language/style lang
                               [filename #f]
                               #:nts [nts (render-language-nts)])
  (if filename
      (save-as-ps/pdf
       (λ () (do-language->pict 'render-language lang nts)) filename)
      (parameterize ([dc-for-text-size
                      (make-object bitmap-dc% (make-object bitmap% 1 1))])
        (do-language->pict 'render-language lang nts))))

(define (language->pict lang #:nts [nts (render-language-nts)])
  (do-language->pict 'language->pict lang nts))

(define (do-language->pict what lang specd-non-terminals)
  (unless (compiled-lang-pict-builder lang)
    (error what "cannot render the result of define-union-language"))
  (define pict-info (compiled-lang-pict-builder lang))
  (define all-non-terminals (pict-info->all-nonterminals pict-info))
  (when specd-non-terminals
    (check-non-terminals what specd-non-terminals lang))
  (make-grammar-pict what
                     pict-info
                     (or specd-non-terminals all-non-terminals)
                     all-non-terminals
                     (stylish-compiled-lang-sets lang)))

(define (make-grammar-pict what raw-info nts all-nts sets)
  (define info
    (remove-unwanted-nts nts (flatten-grammar-info raw-info all-nts nts)))
  (cond
    [(null? info)
     (error what
            (string-append
             "expected some non-terminals to render, but there were none\n"
             "  language's nts: ~a\n"
             "  requested nts: ~a")
            (nts->str all-nts)
            (nts->str nts))]
    [else
     (define term-space
       (launder
        (ghost
         (apply cc-superimpose
                (map (λ (x) (lhs (car x) (car (hash-ref sets (car x)))))
                     info)))))
     (apply vl-append
            (non-terminal-gap-space)
            (for/list ([line (in-list info)])
              (define nt (car line))
              (match-define (cons nt-set set-only?) (hash-ref sets nt))
              ((adjust 'language-production)
               (htl-append
                (rc-superimpose term-space (lhs (car line) nt-set))
                (if set-only?
                    (blank)
                    (lw->pict
                     all-nts
                     (find-enclosing-loc-wrapper (add-bars-and-::= (cdr line)))
                     (adjust 'language-line)))))))]))

(define (lhs nt nt-set)
  (htl-append
   (sequence-of-non-terminals nt)
   (basic-text (string-append " ∈ " (symbol->string nt-set))
               (grammar-style))))

(define (add-bars-and-::= lst)
  (cond
    [(null? lst) null]
    [else
     (cons
      (let ([fst (car lst)])
        (build-lw
         (rc-superimpose (ghost (make-bar)) (make-::=))
         (lw-line fst)
         (lw-line-span fst)
         (lw-column fst)
         0))
      (let loop ([fst (car lst)]
                 [rst (cdr lst)])
        (cond
          [(null? rst) (list fst)]
          [else
           (let* ([snd (car rst)]
                  [bar
                   (cond
                     [(= (lw-line snd)
                         (lw-line fst))
                      (let* ([line (lw-line snd)]
                             [line-span (lw-line-span snd)]
                             [column (+ (lw-column fst)
                                        (lw-column-span fst))]
                             [column-span
                              (max (- (lw-column snd)
                                      (+ (lw-column fst)
                                         (lw-column-span fst)))
                                   0)])
                        (build-lw (make-bar) line line-span column column-span))]
                     [else
                      (build-lw
                       (rc-superimpose (make-bar) (ghost (make-::=)))
                       (lw-line snd)
                       (lw-line-span snd)
                       (lw-column snd)
                       0)])])
             (list* fst
                    bar
                    (loop snd (cdr rst))))])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require chk)

  (define-language/style Λ
    [e ∈ Expr ::= x v (e e ...)]
    [v ∈ Val ::= (λ (x ...) e)]
    #:set-only [x ∈ Var ::= variable-not-otherwise-mentioned]
    [E ∈ Ctx ::= hole (v ... E e ...)]

    #:binding-forms
    (λ (x ...) e #:refers-to (shadow x ...)))

  (render-language/style Λ)

  #;(chk
   ))
