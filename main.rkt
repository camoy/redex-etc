#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide substitute-env
         substitute*
         lookup
         ext
         unique
         rem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         redex/reduction-semantics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metafunctions

;; The `metafunction-leave-default-language-alone` is an internal language used
;; to define metafunctions that work across all languages. This is used by the
;; substitute metafunction for example. We'll use this language for the same
;; purpose. This is a hack that let's us get at that binding.
(define-syntax REDEX
  (make-rename-transformer
   (syntax-binding-set->syntax
    (syntax-binding-set-extend
     (syntax-binding-set)
     'metafunction-leave-default-language-alone
     0
     (module-path-index-join 'redex/private/reduction-semantics #f))
    'metafunction-leave-default-language-alone)))

(define-metafunction REDEX
  substitute-env : any ([any any] ...) -> any
  [(substitute-env any ()) any]
  [(substitute-env any ([any_0 any_1] any_2 ...))
   (substitute (substitute-env any (any_2 ...)) any_0 any_1)])

(define-metafunction REDEX
  substitute* : any [any any] ... -> any
  [(substitute* any [any_0 any_1] ...)
   (substitute-env any ([any_0 any_1] ...))])

(define-metafunction REDEX
  lookup : ([any any] ...) any -> any or #f
  [(lookup (_ ... [any any_0] _ ...) any) any_0])

(define-metafunction REDEX
  ext1 : ([any any] ...) [any any] -> ([any any] ...)
  [(ext1 (any_0 ... [any_k any_v0] any_1 ...) [any_k any_v1])
   (any_0 ... [any_k any_v1] any_1 ...)]
  [(ext1 (any_0 ...) [any_k any_v1])
   ([any_k any_v1] any_0 ...)])

(define-metafunction REDEX
  ext : ([any any] ...) [any any] ... -> ([any any] ...)
  [(ext any) any]
  [(ext any any_0 any_1 ...)
   (ext1 (ext any any_1 ...) any_0)])

(define-relation REDEX
  unique ⊆ (any ...)
  [(unique (any_!_1 ...))])

(define-metafunction REDEX
  rem1 : ([any any] ...) any -> ([any any] ...)
  [(rem1 (any_0 ... [any_k any_v] any_1 ...) any_k)
   (any_0 ... any_1 ...)]
  [(rem1 (any_0 ...) any_k)
   (any_0 ...)])

(define-metafunction REDEX
  rem : ([any any] ...) any ... -> ([any any] ...)
  [(rem any) any]
  [(rem any any_0 any_1 ...)
   (rem1 (rem any any_1 ...) any_0)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define-language Λ
    [e ::= x v (e e ...)]
    [v ::= (λ (x ...) e)]
    [x ::= variable-not-otherwise-mentioned]

    #:binding-forms
    (λ (x ...) e #:refers-to (shadow x ...)))

  (default-language Λ)
  (chk
   (term (substitute-env x ())) (term x)
   (term (substitute-env x ([x 1]))) (term 1)
   (term (substitute-env x ([x 1] [y 2]))) (term 1)
   (term (substitute-env (x y) ([x 1] [y 2]))) (term (1 2))
   (term (substitute* (x y) [x 1] [y 2])) (term (1 2))

   (term (lookup ([x 1]) x)) (term 1)
   (term (lookup ([x 1] [y 2]) y)) (term 2)

   (term (lookup (ext () [x 1]) x)) (term 1)
   (term (lookup (ext () [x 1] [y 2]) x)) (term 1)
   (term (lookup (ext () [x 1] [y 2]) y)) (term 2)

   (term (unique (1 2 3))) #t
   (term (unique (1 2 2))) #f

   (term (rem ([x 1] [y 2] [z 3] [w 4]) x z)) (term ([y 2] [w 4]))
   ))
