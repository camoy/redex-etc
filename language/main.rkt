#lang racket/base

(provide
 #%datum
 (rename-out [-#%module-begin #%module-begin]))

(require (for-syntax racket/base
                     racket/path
                     syntax/parse)
         racket/cmdline
         redex/private/lang-struct
         redex/reduction-semantics
         "private/parameter.rkt"
         "private/plstx.rkt"
         "private/render.rkt")

(begin-for-syntax
  (define-splicing-syntax-class extend
    (pattern (~seq #:extend extend:string))
    (pattern (~seq) #:attr extend #f))

  (define-syntax-class in-lit
    #:datum-literals (in ∈)
    (pattern (~or in ∈)))

  (define-syntax-class in*-lit
    #:datum-literals (in* ∈*)
    (pattern (~or in* ∈*)))

  (define-splicing-syntax-class rhs
    (pattern (~seq e #:: desc:string))
    (pattern e
             #:with desc #'""))

  (define-syntax-class production
    #:attributes ([nt 1] [r.e 1] [r.desc 1] rhs? set)
    #:datum-literals (::=)
    (pattern [nt ...+ _:in*-lit set ::= r:rhs ...]
             #:with rhs? #'#f)
    (pattern [nt ...+ _:in-lit set ::= r:rhs ...]
             #:with rhs? #'#t)
    (pattern [nt ...+ ::= r:rhs ...]
             #:with rhs? #'#t
             #:with set #'#f))

  (define-splicing-syntax-class bindings
    #:attributes ([e 1])
    (pattern (~seq #:binding-forms e ...+))
    (pattern (~seq) #:with (e ...) #'())))

(begin-for-syntax
  (define (path->lang p)
    (string->symbol
     (path->string
      (file-name-from-path
       (path-replace-extension p #""))))))

(define (parameterize-plstx body)
  (parameterize
      ([current-language-template plstx-language-template]
       [current-rhs-procedure plstx-rhs-procedure]
       [current-production-procedure plstx-production-procedure])
    (body)))

(define-syntax (-#%module-begin stx)
  (syntax-parse stx
    [(_ ?e:extend ?p:production ... ?b:bindings)
     #:with ?name (path->lang (syntax-source stx))
     #:with ?extend-name
     (datum->syntax
      stx
      (cond
        [(attribute ?e.extend) => (λ (path) (path->lang (syntax-e path)))]
        [else #f]))
     #:with [?require (?define ...)]
     (if (attribute ?e.extend)
         #`[(require ?e.extend)
            (define-extended-language ?name ?extend-name)]
         #'[(void)
            (define-language ?name)])
     #'(#%module-begin
        (provide ?name)
        ?require
        (?define ...
          [?p.nt ... ::= ?p.r.e ...] ...
          #:binding-forms ?b.e ...)

        (module+ main
          (define (with-parameterization f) (f))
          (define provides? #t)

          (command-line
           #:once-any
           [("-p" "--plstx")
            "Format using the `plstx` environment"
            (set! with-parameterization parameterize-plstx)]
           [("-n" "--no-provide")
            "Don't include `providecommand` definitions."
            (set! provides? #f)])

          (with-parameterization
           (λ ()
             (displayln
              (render-lang
               '?name
               ?name
               ?extend-name
               provides?
               (list (list ?p.r.desc ...) ...)
               (list '?p.rhs? ...)
               (list (list '?p.nt ...) ...)
               (list '?p.set ...)))))))]))
