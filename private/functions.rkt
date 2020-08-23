#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract
         racket/math
         racket/set)
(provide
 (contract-out
  [current-max-steps (parameter/c natural?)]
  [make-eval (->* (reduction-relation?)
                  (#:inject (-> any/c any)
                   #:project (-> any/c any)
                   #:program? predicate/c
                   #:answer? predicate/c)
                  (-> any/c any))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/function
         racket/match
         redex/reduction-semantics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `make-eval`

;; [Parameter Natural]
;; Maximum number of steps for evaluator.
(define current-max-steps (make-parameter 50))

;; Reduction-Relation {Options} → (Term → Term)
;; Constructs an evaluator for the given reduction relation.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require "../test/common.rkt"
           "metas.rkt"
           chk)

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
   ))
