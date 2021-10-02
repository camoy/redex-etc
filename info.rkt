#lang info

;; General

(define collection "redex-etc")
(define pkg-desc "Miscellaneous functions, macros, and metafunctions for Redex.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/redex-etc.scrbl" ())))
(define compile-omit-paths '("test"))

;; Dependencies

(define deps
  '("typed-racket-lib"
    "latex-utils"
    "redex-pict-lib"
    "unstable-redex"
    "base"
    "redex-lib"
    "private-in"
    "draw-lib"
    "pict-lib"))


(define build-deps
  '("redex-doc"
    "chk-lib"
    "racket-doc"
    "scribble-lib"
    "pict-doc"))
