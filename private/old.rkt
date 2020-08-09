#lang racket/base
#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract
         racket/math)
(provide
  current-serif-font
  current-sans-serif-font
  with-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         racket/function
         racket/match
         redex/reduction-semantics
         redex/pict
         racket/list
         syntax/parse/define
         typeset-rewriter
         unstable/gui/redex)


;; {Symbol} → Symbol
;; Generates a "location," a fresh variable prefixed with ℓ.
(define (genloc [x '||])
  (gensym (format "ℓ~a" x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match-term



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metafunctions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

(require rackunit
         pict)

(require/expose redex/private/pict (rp->pict-label current-label-extra-space))
(require/expose redex/private/core-layout (adjust))

(define (basic-text str style) ((current-text) str style (default-font-size)))

(define ((rule-picts->pict/horizontal left-column-align side-conditions-same-line?) rps)
  (let* ([sep 2]
         [max-rhs (apply max
                         0
                         (map pict-width
                              (map rule-pict-info-rhs rps)))]
         [max-w (apply max
                       0
                       (map (lambda (rp)
                              (+ sep sep
                                 (pict-width (rule-pict-info-lhs rp))
                                 (pict-width (arrow->pict (rule-pict-info-arrow rp)))
                                 (pict-width (rule-pict-info-rhs rp))))
                            rps))])
    (define rowss
      (for/list ([rp (in-list rps)])
        (let ([arrow (hbl-append (blank (arrow-space) 0)
                                 (arrow->pict (rule-pict-info-arrow rp))
                                 (blank (arrow-space) 0))]
              [lhs (rule-pict-info-lhs rp)]
              [rhs (rule-pict-info-rhs rp)]
              [spc (basic-text " " (default-style))]
              [label (hbl-append (blank (label-space) 0) (rp->pict-label rp))]
              [sep (blank 4)])
          (if side-conditions-same-line?
              (list
               (list lhs arrow
                     (hbl-append
                      rhs
                      (rule-pict-info->side-condition-pict rp 1))
                     #;label))
              (list
               (list lhs arrow rhs #;label)
               (list (blank) (blank)
                     (let ([sc (rule-pict-info->side-condition-pict rp 1)])
                       (inset sc (min 0 (- max-rhs (pict-width sc))) 0 0 0))
                     (blank)))))))
    ;; Combine like `table`, but in a way that (adjust 'reduction-relation-rule)
    ;; and (adjust 'reduction-relation-line) can be applied
    (define all-cols
      (for*/fold ([all-cols (list (blank) (blank) (blank) (blank))]) ([rows (in-list rowss)]
                                                                      [row (in-list rows)])
        (for/list ([col (in-list all-cols)]
                   [p (in-list row)])
          (ltl-superimpose col (blank (pict-width p) 0)))))

    (apply vl-append
           (+ (reduction-relation-rule-extra-separation)
              (reduction-relation-rule-separation))
           (for/list ([rows (in-list rowss)])
             ((adjust 'reduction-relation-rule)
              (apply vl-append
                     (reduction-relation-rule-line-separation)
                     (for/list ([row (in-list rows)])
                       ((adjust 'reduction-relation-line)
                        (apply htl-append
                               (for/list ([elem (in-list row)]
                                          [col (in-list all-cols)]
                                          [combine
                                           (in-list
                                            (list left-column-align
                                                  ctl-superimpose
                                                  ltl-superimpose
                                                  ltl-superimpose))]
                                          [sep
                                           (in-list (list 0
                                                          sep
                                                          sep
                                                          (+ sep (current-label-extra-space))))])
                                 (inset (combine elem col) sep 0 0 0)))))))))))

(define rr-style
  (rule-picts->pict/horizontal rtl-superimpose #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; typography


(define-syntax-rule (with-style ?body ...)
  (let ([serif-font (current-serif-font)]
        [sans-serif-font (current-sans-serif-font)]
        [size (current-font-size)]
        [symbol->arrow-pict (current-arrow-pict)])
    (set-arrow-pict! ':-> (symbol->arrow-pict ':->))
    (parameterize ([judgment-form-show-rule-names #f]
                   [rule-pict-style rr-style]
                   [white-square-bracket styled-white-square-bracket]
                   [label-style serif-font]
                   [grammar-style sans-serif-font]
                   [non-terminal-set nt-set]
                   [paren-style sans-serif-font]
                   [literal-style sans-serif-font]
                   [metafunction-style sans-serif-font]
                   [non-terminal-style
                    (cons 'italic serif-font)]
                   [non-terminal-subscript-style
                    (list* 'italic 'subscript serif-font)]
                   [non-terminal-superscript-style
                    (list* 'italic 'superscript serif-font)]
                   [default-style serif-font]
                   [label-font-size size]
                   [metafunction-font-size size]
                   [default-font-size size])
      (with-unquote-rewriter
        unquote-rw
        (with-default-rws ?body ...)))))

(define (nt-set nts)
  (case nts
    [((ς)) "State"]
    [((e)) "Expr"]
    [((v)) "Val"]
    [((o)) "Op"]
    [((b)) "Bool"]
    [((t)) "Trace"]
    [((κ)) "Ctc"]
    [((j k l)) "Lab"]
    [((ℓ x)) "Var"]
    [((σ)) "Store"]
    [((u)) "SVal"]
    [((E)) "Ctx"]
    [else #f]))

(define (unquoted-function? lw x)
  (define body (lw-e lw))
  (and (list? body)
       (>= (length body) 2)
       (let ([x (list-ref body 0)])
         (and (lw? x)
              (lw-unq? x)))
       (let ([y (list-ref body 1)])
         (and (lw? y)
              (eq? (lw-e y) x)))))

(define (unquote-rw lw)
  (cond
    [(unquoted-function? lw 'not-hole?)
     ((rewrite-lw "E ≠ □") lw)]
    [(unquoted-function? lw 'set-add)
     ((rewrite-lw "σ(ℓₖ) ∪ {k}") lw)]
    [(unquoted-function? lw 'not-value?)
     ((rewrite-lw "(o v ...) ∉ Vals") lw)]
    [(unquoted-function? lw 'add1)
     ((rewrite-lw "n + 1") lw)]
    [else lw]))

(define ((rewrite-lw e*) x)
  (struct-copy lw
               x
               [e e*]
               [unq? #f]))

(define ((macro-rw name) lws)
  (match-define (list l _ es ...) lws)
  (list* l (format "~a " name) es))


(define-rw-context with-default-rws
  #:atomic (['-> "→"]
            ['->i "→i"]
            ['real "ℝ"]
            ['number "ℂ"]
            ['integer "ℤ"]
            ['natural "ℕ"]
            ['hole "□"]
            [dots "…"]
            ['string "Strs"]
            ['variable-not-otherwise-mentioned "Vars"])
  #:compound (['lookup lookup-rw]
              ['substitute (substitute-rw "→")]
              ['substitute* (substitute*-rw "→")]
              ['ext (substitute*-rw "↦")]
              ['let (macro-rw "let")]
              ['mon* (macro-rw "mon")]
              ['⌢ frown-rw]
              ['~ machine-rw]))
|#
