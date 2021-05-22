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
    [(? symbol?) (format "\\~a" e)]
    [(list '_ e1 e2)
     (format "~a_{~a}" (texexpr->string e1) (texexpr->string e2))]
    [(list '^ e1 e2)
     (format "~a^{~a}" (texexpr->string e1) (texexpr->string e2))]
    [(list '$ e ...) (format "$~a$" (map-join e))]
    [(list '@ e ...) (map-join e)]
    [(list '@@ e ...) (map-join e #:sep "")]
    #|
    [(list (? env? cmd) (list o ...) (list m ...) e ...)
     (define env (env->string cmd))
     (format "\\begin{~a}~a~a~a\\end{~a}"
             env
             (string-join (map option->string o) "")
             (string-join (map mandatory->string m) "")
             (map-join e)
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
             (map-join e)
             env)]
    [(list (? symbol? cmd) (list o ...) e ...)
     (format "\\~a~a{~a}"
             cmd
             (string-join (map option->string o) "")
             (map-join e))]
    |#
    [(list (? env? cmd) e ...)
     (define env (env->string cmd))
     (format "\\begin{~a}~a\\end{~a}"
             env
             (map-join e)
             env)]
    [(list (? symbol? cmd) e ...)
     (format "\\~a{~a}"
             cmd
             (map-join e))]
    [_ (raise-argument-error 'texexpr->string "texexpr?" e)]))

(define (map-join es #:sep [sep " "])
  (string-join (map texexpr->string es) sep))

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

   (texexpr->string `(@ "hello" "world"))
   "hello world"
   (texexpr->string `(@@ "hello" "world"))
   "helloworld"
   (texexpr->string `($ "1 + 1"))
   "\\[1 + 1\\]"
   ))
