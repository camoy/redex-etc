#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 ;; `functions.rkt`
 current-max-steps
 make-eval

 ;; `macros.rkt`
 match-term

 ;; `metas.rkt`
 substitute-env
 substitute*
 lookup
 ext
 unique
 rem

 ;; `language.rkt`
 define-language/style
 define-extended-language/style
 render-language/style
 nt-set
 nt-set?

 ;; `rule-style.rkt`
 current-rule-label?
 current-compact-threshold

 ;; `typography.rkt`
 render-metafunction/style
 render-metafunctions/style
 render-judgment-form/style
 render-term/style
 render-reduction-relation/style

 substitute-rw
 substitute*-rw
 lookup-rw
 lookup*-rw
 typing-rw
 sf-rw
 sans-rw
 mono-rw
 set-add-rw
 unquote-rw
 rw/c

 default-atomic-rewriters
 default-compound-rewriters
 default-unquote-rewriters

 current-arrow-hash
 current-serif-font
 current-sans-serif-font
 current-mono-font
 current-font-size
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "private/functions.rkt"
         "private/macros.rkt"
         "private/metas.rkt"
         "private/language.rkt"
         "private/rule-style.rkt"
         "private/typography.rkt")
