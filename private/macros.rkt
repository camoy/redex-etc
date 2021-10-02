#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide match-term
         not-match?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/match
                     racket/sequence
                     racket/require-transform
                     typed-racket/typecheck/possible-domains
                     typed-racket/types/generalize
                     typed-racket/env/env-req
                     typed-racket/env/global-env
                     typed-racket/standard-inits
                     syntax/strip-context
                     syntax/parse
                     syntax/modresolve)
         redex/reduction-semantics
         syntax/parse/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define-simple-macro (match-term L:id e0:expr [pat e:expr] ...)
  ((term-match/single L [pat e] ...) e0))

(define-syntax-rule (not-match? lang p e)
  (not (redex-match? lang p (term e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `require-typed-primitives`

(begin-for-syntax
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
    (define contains-tr?
      (for/or ([resolved-src (in-list resolved-srcs)])
        (define tr?
          (or (equal? tr resolved-src)
              (equal? tr-base resolved-src)))
        (unless tr? (add-mod! resolved-src))
        tr?))
    (if contains-tr?
        (do-standard-inits)
        (do-requires)))

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
                #:when (hash-has-key? sym-type-ht src-sym))
      (values import (hash-ref sym-type-ht src-sym))))

  ;; Type → Datum
  ;; Default conversion between Typed Racket types and a datum.
  (define (type->datum ty)
    (read (open-input-string (format "~a" ty))))
  )

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
                 type->datum))]
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
       (list local-id (convert-type (hash-ref type-map import))))
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
  (require "../test/common.rkt"
           chk)

  (chk
   #:t (not-match? Λ (λ (x) e) x)
   #:! #:t (not-match? Λ (λ (x) e) (λ (y) y))
   #:t
   (match-term
    Λ (term (λ (x) x))
    [(λ (_ _) e) #f]
    [(λ (x) e) (equal? (term x) (term e))])
   ))
