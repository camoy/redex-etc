#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 ;; `functions.rkt`
 current-max-steps
 make-eval
 set-cons

 ;; `macros.rkt`
 match-term

 ;; `metafunctions.rkt`
 substitute-env
 substitute*
 lookup
 ext
 unique
 rem

 ;; `typography.rkt`
 acmart-style!
 render-language/style
 render-metafunction/style
 render-reduction-relation/style
 current-arrow-hash
 current-serif-font
 current-sans-serif-font
 current-font-size)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "private/functions.rkt"
         "private/macros.rkt"
         "private/metafunctions.rkt"
         "private/typography.rkt")
