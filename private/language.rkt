#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide define-language/style
         define-extended-language/style
         render-language/style
         nt-set
         nt-set?)

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
                  add-bars-and-::=
                  basic-text)
         (for-syntax racket/base
                     racket/match
                     racket/struct-info
                     racket/syntax
                     syntax/parse)
         "typography.rkt"
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

(define language-set-hash (make-hash))

(begin-for-syntax
  (define-splicing-syntax-class clause
    #:datum-literals (∈ ::=)
    (pattern [nt:id ... ∈ s:id ::= rhs:expr ...]
             #:with def #'#f)
    (pattern [nt:id ... ∈ s:id ::= rhs:expr ... #:define]
             #:with def #'"")
    (pattern [nt:id ... ∈ s:id ::= rhs:expr ... #:define def:expr]))

  (define ((make-define-language/style form) stx)
    (syntax-parse stx
      [(_ ?name:id
          (~optional ?ext:id)
          ?c:clause ...
          (~optional (~seq #:binding-forms ?b ...)))
       #`(begin
           (#,form ?name (~? ?ext)
             [?c.nt ... ::= ?c.rhs ...] ...
             (~? (~@ #:binding-forms ?b ...)))
           (hash-set!
            language-set-hash
            ?name
            (hash (~@ '(?c.nt ...)
                      (cons '?c.s ?c.def)) ...)))]))
  )

(define-syntax define-language/style
  (make-define-language/style #'define-language))

(define-syntax define-extended-language/style
  (make-define-language/style #'define-extended-language))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `render-language/style`

(define (render-language/style lang
                               [filename #f]
                               #:nts [nts (render-language-nts)])
  (with-style
    (if filename
        (save-as-ps/pdf
         (λ () (do-language->pict 'render-language lang nts)) filename)
        (parameterize ([dc-for-text-size
                        (make-object bitmap-dc% (make-object bitmap% 1 1))])
          (do-language->pict 'render-language lang nts)))))

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
                     (hash-ref language-set-hash lang)))

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
              (match-define (cons nt-set def) (hash-ref sets nt))
              ((adjust 'language-production)
               (htl-append
                (rc-superimpose term-space (lhs (car line) nt-set))
                (cond
                  [(nt-set? def)
                   (hbl-append (basic-text " = " (grammar-style))
                               (nt-set-render def))]
                  [(pict? def)
                   (hbl-append (basic-text " = " (grammar-style))
                               def)]
                  [(equal? def "") (blank)]
                  [def (basic-text (format "  =  ~a" def) (grammar-style))]
                  [else
                   (lw->pict
                    all-nts
                    (find-enclosing-loc-wrapper (add-bars-and-::= (cdr line)))
                    (adjust 'language-line))])))))]))

(define (lhs nt nt-set)
  (htl-append
   (sequence-of-non-terminals nt)
   (basic-text (string-append " ∈ " (symbol->string nt-set))
               (grammar-style))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-terminal set

(struct nt-set (s))

(define (nt-set-render nt)
  (define s (nt-set-s nt))
  (hbl-append
   (basic-text "{ " (current-sans-serif-font))
   (basic-text s (cons 'italic (current-sans-serif-font)))
   (basic-text " }" (current-sans-serif-font))))
