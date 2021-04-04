#lang racket/base

(provide
 #%datum
 (rename-out [-#%module-begin #%module-begin]))

(require (for-syntax racket/base
                     racket/path
                     syntax/parse)
         redex/reduction-semantics
         redex/private/lang-struct
         racket/string
         racket/match
         latex-utils/scribble/unmap
         file/sha1
         "private/render.rkt")

(begin-for-syntax
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
             #:with set #'#f)))

(define-syntax (-#%module-begin stx)
  (syntax-parse stx
    [(_ ?p:production ...)
     #:with ?filename
     (string->symbol
      (path->string
       (file-name-from-path
        (path-replace-extension (syntax-source stx) #""))))
     #'(#%module-begin
        (provide ?filename)

        (define-language ?filename
          [?p.nt ... ::= ?p.r.e ...]
          ...)

        (module+ main
          (displayln
           (render-lang
            '?filename
            ?filename
            (list (list ?p.r.desc ...) ...)
            (list '?p.rhs? ...)
            (list '?p.set ...)))))]))
