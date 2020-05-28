#lang scribble/manual

@require[@for-label[redex-etc
                    racket/base
                    racket/function
                    redex/reduction-semantics]
         scribble/example]

@(define evaluator
  (make-base-eval
    '(require redex/reduction-semantics
              redex-etc)))

@title{Redex Miscellanea}
@author{Cameron Moy}

@defmodule[redex-etc]

This package implements miscellaneous
functions, macros, and metafunctions
for Redex.
For the purposes of illustration,
we will use the following language
in our examples.

@(examples #:eval evaluator #:label #f #:no-result
  (define-language Λ
    [e ::= x v (e e ...)]
    [v ::= (λ (x ...) e)]
    [x ::= variable-not-otherwise-mentioned]

    #:binding-forms
    (λ (x ...) e #:refers-to (shadow x ...)))

  (default-language Λ))

@defproc[(make-eval [rr reduction-relation?]
                    [#:inject inject (-> any/c any) values]
                    [#:project project (-> any/c any) values]
                    [#:program? program? predicate/c (const #t)]
                    [#:answer? answer? predicate/c (const #t)])
         (-> any/c any)]{
  Returns an evaluation function that applies the given reduction relation
  until reaching a normal form. The evaluation function will use
  @racket[inject] into an initial configuration
  and @racket[project] out of the final configuration.
  Initial and final expressions will be checked against @racket[program?]
  and @racket[answer?] respectively.
  The evaluation function takes a maximum of @racket[current-max-steps] steps
  before aborting.
  Additionally it will throw an error
  on a non-deterministic result.
}

@defparam[current-max-steps max-steps natural? #:value 50]{
  A parameter that determines the maximum number of steps evaluation
  function created by @racket[make-eval] will take.
}

@defform[(match-term lang expression [pattern expression] ...)]{
  Matches the given term against a series of patterns,
  choosing the expression corresponding to the first matching pattern.
}

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
