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

(require private-in)
(require (for-syntax racket/base)
         ;; The `metafunction-leave-default-language-alone` is an internal
         ;; language used to define metafunctions that work across all
         ;; languages.
         (only-in (private-in redex/private/reduction-semantics)
                  [metafunction-leave-default-language-alone LANG])
         redex/reduction-semantics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metafunctions and judgments

(define-metafunction LANG
  substitute-env : any ([any any] ...) -> any
  [(substitute-env any ()) any]
  [(substitute-env any (any_2 ... [any_0 any_1]))
   (substitute (substitute-env any (any_2 ...)) any_0 any_1)])

(define-metafunction LANG
  substitute* : any [any any] ... -> any
  [(substitute* any [any_0 any_1] ...)
   (substitute-env any ([any_0 any_1] ...))])

(define-metafunction LANG
  lookup : ([any any] ...) any -> any or #f
  [(lookup (_ ... [any any_0] _ ...) any) any_0])

(define-metafunction LANG
  ext1 : ([any any] ...) [any any] -> ([any any] ...)
  [(ext1 (any_0 ... [any_k any_v0] any_1 ...) [any_k any_v1])
   (any_0 ... [any_k any_v1] any_1 ...)]
  [(ext1 (any_0 ...) [any_k any_v1])
   ([any_k any_v1] any_0 ...)])

(define-metafunction LANG
  ext : ([any any] ...) [any any] ... -> ([any any] ...)
  [(ext any) any]
  [(ext any any_1 ... any_0)
   (ext1 (ext any any_1 ...) any_0)])

(define-relation LANG
  unique ⊆ (any ...)
  [(unique (any_!_1 ...))])

(define-metafunction LANG
  rem1 : ([any any] ...) any -> ([any any] ...)
  [(rem1 (any_0 ... [any_k any_v] any_1 ...) any_k)
   (any_0 ... any_1 ...)]
  [(rem1 (any_0 ...) any_k)
   (any_0 ...)])

(define-metafunction LANG
  rem : ([any any] ...) any ... -> ([any any] ...)
  [(rem any) any]
  [(rem any any_0 any_1 ...)
   (rem1 (rem any any_1 ...) any_0)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require "../test/common.rkt"
           chk)

  (default-language Λ)

  (chk
   (term (substitute-env x ())) (term x)
   (term (substitute-env x ([x 1]))) (term 1)
   (term (substitute-env x ([x 1] [y 2]))) (term 1)
   (term (substitute-env (x y) ([x 1] [y 2]))) (term (1 2))
   (term (substitute* (x y) [x 1] [y 2])) (term (1 2))
   (term (substitute* x [x (x y)] [x 1] [y 2])) (term (1 2))

   (term (lookup ([x 1]) x)) (term 1)
   (term (lookup ([x 1] [y 2]) y)) (term 2)

   (term (lookup (ext () [x 1]) x)) (term 1)
   (term (lookup (ext () [x 1] [y 2]) x)) (term 1)
   (term (lookup (ext () [x 1] [y 2]) y)) (term 2)
   (term (lookup (ext () [x 1] [x 2]) x)) (term 2)

   (term (unique (1 2 3))) #t
   (term (unique (1 2 2))) #f

   (term (rem ([x 1] [y 2] [z 3] [w 4]) x z)) (term ([y 2] [w 4]))
   ))
