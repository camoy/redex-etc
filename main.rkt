#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract
         racket/math)
(provide
 (contract-out
  [current-max-steps (parameter/c natural?)]
  [make-eval (->* (reduction-relation?)
                  (#:inject (-> any/c any)
                   #:project (-> any/c any)
                   #:program? predicate/c
                   #:answer? predicate/c)
                  (-> any/c any))]
  [genloc (-> symbol? symbol?)])
  match-term
  substitute-env
  substitute*
  lookup
  ext
  unique
  rem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base)
         racket/function
         racket/match
         redex/reduction-semantics
         syntax/parse/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-eval

(define current-max-steps (make-parameter 50))

(define ((make-eval rr
                    #:inject [inject values]
                    #:project [project values]
                    #:program? [program? (const #t)]
                    #:answer? [answer? (const #t)])
         e)
  (unless (program? e)
    (error 'make-eval "input is not a program"))
  (define gas (current-max-steps))
  (define results
    (apply-reduction-relation*
     rr (inject e)
     #:stop-when
     (λ _
       (set! gas (sub1 gas))
       (< gas 0))))
  (when (< gas 0)
    (error 'make-eval "exceeded maximum number of steps"))
  (match results
    [(list s)
     (define result (project s))
     (unless (answer? result)
       (error 'make-eval "output is not an answer: ~a" result))
     result]
    [_ (error 'make-eval
              "non-deterministic or non-terminating reduction relation")]))

;; Symbol → Symbol
;; Generates a "location," a fresh variable prefixed with ℓ.
(define (genloc x)
  (gensym (format "ℓ~a" x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match-term

(define-simple-macro (match-term L:id e0:expr [pat e:expr] ...)
  ((term-match/single L [pat e] ...) e0))

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
  [(substitute-env any (any_2 ... [any_0 any_1]))
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
  [(ext any any_1 ... any_0)
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
  (require chk
           racket/string)

  (define-language Λ
    [e ::= x v (e e ...)]
    [v ::= (λ (x ...) e)]
    [x ::= variable-not-otherwise-mentioned]
    [E ::= hole (v ... E e ...)]

    #:binding-forms
    (λ (x ...) e #:refers-to (shadow x ...)))

  (default-language Λ)

  (define v
    (reduction-relation
     Λ
     [--> (in-hole E ((λ (x ..._a) e) v ..._a))
          (in-hole E (substitute* e [x v] ...))
          βv]))

  (define (program? prog)
    (redex-match? Λ e prog))

  (define (answer? res)
    (redex-match? Λ v res))

  (define ⇓ (make-eval v #:program? program? #:answer? answer?))

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

   #:t
   (match-term
    Λ (term (λ (x) x))
    [(λ (_ _) e) #f]
    [(λ (x) e) (equal? (term x) (term e))])

   #:eq alpha-equivalent?
   (⇓ (term ((λ (f x y) (f x y)) (λ (x y) y) (λ (x) (x x)) (λ (x) x))))
   (term (λ (x) x))

   #:x
   (⇓ (term ((λ (x) (x x)) (λ (x) (x x)))))
   "non-terminating"

   #:x
   (parameterize ([current-max-steps 2])
     (⇓ (term ((λ (f x y) (f x y)) (λ (x y) y) (λ (x) (x x)) (λ (x) x)))))
   "exceeded"

   #:x
   (⇓ (term ((λ (x) y) (λ (x) x))))
   "output is not"

   #:x
   (⇓ (term 5))
   "input is not"

   #:t (string-prefix? (symbol->string (genloc "x")) "ℓx")
   ))
