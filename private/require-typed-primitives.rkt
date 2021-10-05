#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide require-typed-primitives)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/match
                     racket/set
                     racket/sequence
                     racket/promise
                     racket/require-transform
                     typed-racket/typecheck/possible-domains
                     typed-racket/types/generalize
                     typed-racket/env/env-req
                     typed-racket/env/global-env
                     typed-racket/standard-inits
                     typed-racket/utils/tc-utils
                     typed-racket/tc-setup
                     syntax/strip-context
                     syntax/parse
                     syntax/modresolve)
         redex/reduction-semantics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

(begin-for-syntax
  ;; Since Redex itself uses Typed Racket internally, it registers a few type
  ;; aliases which we have to remove from the alias list.
  (define REDEX-TYPES '(Pattern Term Env TEnv Tag Tag))

  ;; See: https://github.com/racket/typed-racket/issues/1144
  (define baddies
    '(exn:srclocs?
      prop:print-convert-constructor-name
      rename-transformer?
      set!-transformer?
      struct-type-property-predicate-procedure?))

  ;; Syntax → Resolved-Module-Path
  (define (module-path-syntax-resolve mp-stx)
    (make-resolved-module-path
     (resolve-module-path
      (syntax->datum mp-stx))))

  ;; Resolved module paths to `typed/racket` and `typed/racket/base`.
  (define tr (module-path-syntax-resolve #'typed/racket))
  (define tr-base (module-path-syntax-resolve #'typed/racket/base))

  ;; [Listof Resolved-Module-Path] → Any
  ;; Initializes the given typed modules. Importing `typed/racket` or
  ;; `typed/racket/base` causes a `do-standard-inits` instead.
  (define (add-import-mods! resolved-srcs)
    (for ([resolved-src (in-list resolved-srcs)]
          #:when (not (or (equal? tr resolved-src)
                          (equal? tr-base resolved-src))))
      (add-mod! resolved-src))
    (do-standard-inits)
    (define names (remove-redex-aliases (force (init-current-type-names))))
    (current-type-names (lazy names)))

  ;; Syntax → [Listof Import] [Listof Resolved-Module-Path]
  ;; Given a syntax contain require specs, returns the corresponding
  ;; imports and resolved module paths.
  (define (expand-imports reqs)
    (for/fold ([imports null]
               [resolved-import-srcs null])
              ([req-spec (in-syntax reqs)])
      (define-values (cur-imports cur-import-srcs)
        (expand-import req-spec))
      (define cur-resolved-import-srcs
        (map (compose module-path-syntax-resolve
                      import-source-mod-path-stx)
             cur-import-srcs))
      (values (append cur-imports imports)
              (append cur-resolved-import-srcs resolved-import-srcs))))

  ;; [Listof Import] → [Immutable-Hash Import Type]
  ;; Returns a hash mapping imports to its type.
  (define (import-type-map imports)
    (define sym-type-ht
      (make-immutable-hash
       (filter
        values
        (type-env-map
         (λ (name-stx ty-or-proc)
           (define name (syntax-e name-stx))
           (and (not (member name baddies))
                (let ([ty (if (procedure? ty-or-proc)
                              (ty-or-proc)
                              ty-or-proc)])
                  (cons name (generalize (cleanup-type ty))))))))))
    (for*/hash ([import (in-list imports)]
                [src-sym (in-value (import-src-sym import))]
                #:when (hash-has-key? sym-type-ht src-sym)
                #:when (zero? (import-mode import)))
      (values import (hash-ref sym-type-ht src-sym))))

  ;; AList → AList
  ;; Removes Redex aliases from given alias association list.
  (define (remove-redex-aliases aliases)
    (filter (λ (alias)
              (not (set-member? REDEX-TYPES (car alias))))
            aliases))

  ;; Type → Datum
  ;; Default conversion between Typed Racket types and a datum.
  (define (type->datum ty)
    (read (open-input-string (format "~a" ty)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro

;; Defines primop reduction metafunction δ and primop typing
;; metafunction Δ based on imports from the given Typed Racket
;; modules. Uses the `#:convert-type` argument to convert types
;; into a Redex-suitable representation if supplied.
(define-syntax (require-typed-primitives stx)
  (syntax-parse stx
    [(_ ?lang:id ?δ:id ?Δ:id
        (~optional (~seq #:convert-type ?convert-type:id))
        ?req-spec ...)
     #:cut
     #:do [(define convert-type
             (if (attribute ?convert-type)
                 (syntax-local-value #'?convert-type (λ () #f))
                 values))]
     #:fail-when
     (and (not convert-type) #'?convert-type)
     "not a transformer binding"
     #:do [(define-values (imports resolved-import-srcs)
             (expand-imports #'(?req-spec ...)))
           (add-import-mods! resolved-import-srcs)
           (define type-map (import-type-map imports))]
     #:with ([?prim ?type] ...)
     (for/list ([import (in-list imports)]
                #:when (hash-has-key? type-map import))
       (define local-id (import-local-id import))
       (define type-sexp (convert-type (type->datum (hash-ref type-map import))))
       (list local-id type-sexp))
     #'(begin
         (require ?req-spec ...)

         (define-metafunction ?lang
           [(?δ (?prim any (... ...)))
            ,(apply ?prim (term (any (... ...))))]
           ...)

         (define-metafunction ?lang
           [(?Δ ?prim) ?type] ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require (for-syntax typed-racket/types/base-abbrev
                       typed-racket/types/numeric-tower
                       typed-racket/rep/type-rep
                       typed-racket/rep/values-rep)
           chk
           "functions.rkt")

  ;; The standard simply typed λ-calculus (multi-arity functions) with integers,
  ;; booleans, and primitive operations `even?` and `odd?`.
  (define-language Λ
    [e ::= x v (e e ...)]
    [v ::= integer boolean o (λ ([x τ] ...) e)]
    [o ::= even? odd?]
    [x ::= variable-not-otherwise-mentioned]
    [E ::= hole (v ... E e ...)]

    [τ ::= Integer Boolean (-> τ τ ...)]
    [Γ ::= ([x τ] ...)]

    #:binding-forms
    (λ (x ...) e #:refers-to (shadow x ...)))

  ;; Here, we list all of the primitives we want.
  (require-typed-primitives
   Λ δ Δ
   (only-in typed/racket even? odd?))

  ;; The usual typing judgment. The types of primitives consult Δ. Some of the
  ;; helper metafunctions (`lookup` and `ext`) come from `redex-etc`.
  (define-judgment-form Λ
    #:mode (⊢ I I I O)
    #:contract (⊢ Γ e : τ)
    [(⊢ Γ x : τ)
     (where τ (lookup Γ x))]
    [(⊢ Γ integer : Integer)]
    [(⊢ Γ boolean : Boolean)]
    [(⊢ Γ o : (Δ o))]

    [(⊢ (ext Γ [x τ_a] ...) e : τ_r)
     --------------------------------------------
     (⊢ Γ (λ ([x τ_a] ...) e) : (-> τ_a ... τ_r))]

    [(⊢ Γ e_f : (-> τ_a ... τ_r))
     (⊢ Γ e_v : τ_a) ...
     ----------------------------
     (⊢ Γ (e_f e_v ...) : τ_r)])

  ;; The usual CBV reduction relation. Reduction of primitive application
  ;; consults δ. The `substitute*` comes from `redex-etc`.
  (define ↦v
    (reduction-relation
     Λ
     [--> (in-hole E ((λ ([x τ] ..._a) e) v ..._a))
          (in-hole E (substitute* e [x v] ...))
          βv]

     [--> (in-hole E (o v ...))
          (in-hole E (δ (o v ...)))
          δ]))

  ;; Programs are well-typed expressions.
  (define (program? e)
    (judgment-holds (⊢ () ,e : τ)))

  ;; Answers are values.
  (define (answer? e)
    (redex-match? Λ v e))

  ;; This helper comes from `redex-etc` too. It generates an evaluator
  ;; based on ↦v which checks that the input is a program and that
  ;; the output is an answer.
  (define ⇓
    (make-eval ↦v
               #:program? program?
               #:answer? answer?))

  ;; And, we can test it!
  (chk
   #:t (⇓ (term (even? 42)))
   #:! #:t (⇓ (term (even? 43)))
   #:! #:t (⇓ (term (odd? 42)))
   #:t (⇓ (term (odd? 43)))
   ))
