#lang scribble/manual

@require[@for-label[pict
                    pict/convert
                    redex-etc
                    racket/base
                    racket/math
                    racket/contract
                    racket/function
                    redex/reduction-semantics
                    redex/pict
                    unstable/gui/redex]
         scribble/example]

@(define evaluator
  (make-base-eval
    '(require redex/reduction-semantics
              redex-etc)))

@title{Redex Miscellanea}
@author{Cameron Moy}

@defmodule[redex-etc]

@margin-note{
This library is under development;
compatibility will not be maintained.
}

This package implements miscellaneous
metafunctions, macros, and functions
for Redex.
For the purposes of illustration,
we will use the following language
in our examples.

@examples[#:eval evaluator #:label #f #:no-result
  (define-language Λ
    [e ::= x v (e e ...)]
    [v ::= (λ (x ...) e)]
    [x ::= variable-not-otherwise-mentioned]
    [E ::= hole (v ... E e ...)]

    #:binding-forms
    (λ (x ...) e #:refers-to (shadow x ...)))

  (default-language Λ)]

@section{Metafunctions}

@defform[#:kind "metafunction"
         (substitute-env target ([key value] ...))]{
  Given an environment, a list of keys and values, replaces instances
  of @racket[key] with @racket[value] in the @racket[target] term.
  @examples[#:eval evaluator
    (term (substitute-env x ([x 1] [y 2])))]
}

@defform[#:kind "metafunction"
         (substitute* target [key value] ...)]{
  Shorthand for a call to @racket[substitute-env] with the given
  @racket[key]-@racket[value] pairs as an environment.
  @examples[#:eval evaluator
    (term (substitute* x [x 1] [y 2]))]
}

@defform[#:kind "metafunction"
         (lookup ([key value] ...) needle)]{
  Returns the first @racket[value] whose @racket[key] matches the @racket[needle].
  If none match, @racket[#f] is returned.
  @examples[#:eval evaluator
    (term (lookup ([x 1] [y 2]) x))]
}

@defform[#:kind "metafunction"
         (ext ([key value] ...) [new-key new-value] ...)]{
  Extends the given environment with the new @racket[key]-@racket[value] pairs,
  adding where the @racket[key] isn't already present,
  and overriding the @racket[value] if the @racket[key] is present.
  @examples[#:eval evaluator
    (term (ext ([x 1] [y 2]) [x 2] [z 3]))]
}

@defform[#:kind "metafunction"
         (unique (any ...))]{
  Returns whether the given elements are unique.
  @examples[#:eval evaluator
    (term (unique (1 2 3)))
    (term (unique (1 2 2)))]
}

@defform[#:kind "metafunction"
         (rem ([key value] ...) old-key ...)]{
  Removes the associations with the @racket[old-key] if they are present
  in the environment. Does nothing otherwise.
  @examples[#:eval evaluator
    (term (rem ([x 1] [y 2]) x z))]
}

@section{Macros}

@defform[(match-term lang expression [pattern expression] ...)]{
  Matches the given term against a series of patterns,
  choosing the expression corresponding to the first matching pattern.
  @examples[#:eval evaluator
    (match-term Λ (term (λ (x) (x x)))
      [(λ _ e) (term e)])]
}

@section{Functions}

@defproc[(make-eval [rr reduction-relation?]
                    [#:inject inject (-> any/c any) values]
                    [#:project project (-> any/c any) values]
                    [#:program? program? predicate/c (const #t)]
                    [#:answer? answer? predicate/c (const #t)])
         (-> any/c any)]{
  Returns an evaluation function that applies the given reduction relation
  until reaching a normal form. The evaluation function will
  @racket[inject] into an initial machine configuration
  and @racket[project] out of the final configuration.
  Initial and final expressions will be checked against @racket[program?]
  and @racket[answer?] respectively.
  The evaluation function takes a maximum of @racket[current-max-steps] steps
  before aborting.
  Additionally it will throw an error
  on a non-deterministic or non-terminating result.

  @examples[#:eval evaluator
    (define v
      (reduction-relation Λ
        [--> (in-hole E ((λ (x ..._a) e) v ..._a))
             (in-hole E (substitute* e [x v] ...))
             βv]))
    (define ⇓ (make-eval v))
    (⇓ (term ((λ (x) x) (λ (y) y))))]
}

@defparam[current-max-steps max-steps natural? #:value 50]{
  A parameter that determines the maximum number of steps the evaluation
  function created by @racket[make-eval] will take before quitting.

  @(evaluator '(current-max-steps 5))
  @examples[#:eval evaluator
    (eval:error (⇓ (term ((λ (x) ((x x) x)) (λ (y) ((y y) y))))))]
}

@section{Typography}

@defform[
(define-language/style lang-name
  non-terminal-def ...
  maybe-binding-spec)]{
  TODO
}

@defform[
(define-extended-language/style extended-lang base-lang
  non-terminal-def ...
  maybe-binding-spec)]{
  TODO
}

@defproc[
(render-language/style
  [lang compiled-lang?]
  [file (or/c #f path-string?) #f]
  [#:nts nts (or/c #f (listof (or/c string? symbol?))) (render-language-nts)])
  pict?]{
  TODO
}

@deftogether[(
  @defform*[(
    (render-metafunction/style metafunction-name maybe-contract)
    (render-metafunction/style metafunction-name filename maybe-contract))]
  @defform[(render-metafunctions/style metafunction-name ...
             maybe-filename maybe-contract maybe-only-contract)])]{
  TODO
}

@defform*[((render-judgment-form/style judgment-form-name)
           (render-judgment-form/style judgment-form-name filename))]{
  TODO
}

@defproc[
  (render-reduction-relation/style
    [rel reduction-relation?]
    [file	(or/c #f path-string?) #f]
    [#:style style reduction-rule-style/c (rule-pict-style)])
  (if file void? pict-convertible?)]{
  TODO
}

@defproc[(nt-set [non-terminal-def string?]) nt-set?]{
  TODO
}

@defproc[(nt-set? [x any/c]) boolean?]{
  TODO
}

@defparam[current-rule-label? rule-label? boolean? #:value #f]{}
@defparam[current-compact-threshold compact-threshold natural?]{}
@defparam[current-arrow-hash arrow-hash (hash/c symbol? string?)]{}

@deftogether[(
  @defparam[current-serif-font
            serif-font
            text-style/c
            #:value "Linux Libertine"]
  @defparam[current-sans-serif-font
            sans-serif-font
            text-style/c
            #:value "Linux Biolinum"]
  @defparam[current-mono-font
            mono-font
            text-style/c
            #:value "DejaVu Sans Mono"]
  @defparam[current-font-size
            font-size
            natural?
            #:value 22])]{
  TODO
}

@section{Rewriting}

@deftogether[(
  @defproc[(substitute-rw [op string?]
                          [#:flip? flip? boolean? #f])
           rw/c]
  @defproc[(substitute*-rw [op string?]
                           [#:flip? flip? boolean? #f])
           rw/c])]{
  TODO
}

@deftogether[(
  @defthing[lookup-rw rw/c]
  @defthing[lookup*-rw rw/c])]{
  TODO
}

@defproc[(typing-rw [op string?]) rw/c]{
  TODO
}

@defproc[(set-add-rw [op string?]) rw/c]{
  TODO
}

@deftogether[(
  @defproc[(sf-rw [font-family text-style/c]) rw/c]
  @defproc[(sans-rw [font-family text-style/c]) rw/c]
  @defproc[(mono-rw [font-family text-style/c]) rw/c])]{
  TODO
}

@defproc[(unquote-rw [replacement
                      (or/c string?
                            symbol?
                            pict-convertible?
                            (listof (or/c (symbols 'spring) lw?)))])
         (-> lw? lw?)]{
  TODO
}

@defthing[rw/c contract?]{
  Recognizes a rewriter.
  Equivalent to
  @racket[(-> (listof lw?) (listof (or/c lw? string? pict-convertible?)))].
}


@deftogether[(
  @defparam[default-atomic-rewriters
            atomic-rewriters
            (plistof symbol?
                     (listof (or/c lw?
                                   string?
                                   pict-convertible?)))]
  @defparam[default-compound-rewriters
            compound-rewriters
            (plistof symbol? rw/c)]
  @defparam[default-unquote-rewriters
            unquote-rewriters
            (plistof symbol? (-> lw? lw?))])]{
  TODO
}
