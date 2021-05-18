#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide texexpr->string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/match
         racket/contract
         racket/string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(define (texexpr->string e)
  (match e
    [(? string?) e]
    [(list (? env? cmd) (list o ...) (list m ...) e ...)
     (define env (env->string cmd))
     (format "\\begin{~a}~a~a~a\\end{~a}"
             env
             (string-join (map option->string o) "")
             (string-join (map mandatory->string m) "")
             (string-join (map texexpr->string e) " ")
             env)]
    [(list (? symbol? cmd) (list o ...) (list m ...))
     (format "\\~a~a~a"
             cmd
             (string-join (map option->string o) "")
             (string-join (map mandatory->string m) ""))]
    [(list (? env? cmd) (list o ...) e ...)
     (define env (env->string cmd))
     (format "\\begin{~a}~a~a\\end{~a}"
             env
             (string-join (map option->string o) "")
             (string-join (map texexpr->string e) " ")
             env)]
    [(list (? symbol? cmd) (list o ...) e ...)
     (format "\\~a~a{~a}"
             cmd
             (string-join (map option->string o) "")
             (string-join (map texexpr->string e) " "))]
    [(list (? env? cmd) e ...)
     (define env (env->string cmd))
     (format "\\begin{~a}~a\\end{~a}"
             env
             (string-join (map texexpr->string e) " ")
             env)]
    [(list (? symbol? cmd) e ...)
     (format "\\~a{~a}"
             cmd
             (string-join (map texexpr->string e) " "))]
    [(? list?)
     (format "(~a)" (string-join (map texexpr->string e) " "))]))

(define (option->string e)
  (format "[~a]" (texexpr->string e)))

(define (mandatory->string e)
  (format "{~a}" (texexpr->string e)))

(define (env? x)
  (and (symbol? x)
       (string-prefix? (symbol->string x) "env:")))

(define (env->string x)
  (substring (symbol->string x) 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   (texexpr->string `(env:foo ("bar" "baz") ("qux" "quux") "cat" "dog"))
   "\\begin{foo}[bar][baz]{qux}{quux}cat dog\\end{foo}"
   (texexpr->string `(foo ("bar" "baz") ("qux" "quux")))
   "\\foo[bar][baz]{qux}{quux}"

   (texexpr->string `(env:foo ("bar" "baz") "cat" "dog"))
   "\\begin{foo}[bar][baz]cat dog\\end{foo}"
   (texexpr->string `(foo ("bar" "baz") "qux" "quux"))
   "\\foo[bar][baz]{qux quux}"

   (texexpr->string `(env:foo "cat" "dog"))
   "\\begin{foo}cat dog\\end{foo}"
   (texexpr->string `(foo "qux" "quux"))
   "\\foo{qux quux}"
   ))
