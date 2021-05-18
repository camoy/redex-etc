#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide compound-rewriter
         atomic-rewriter
         unquote-rewriter
         make-rewriters
         current-rewriters
         current-fallback-compound-rewriter
         default-fallback-compound-rewriter
         compound-rewrite
         current-fallback-atomic-rewriter
         default-fallback-atomic-rewriter
         atomic-rewrite
         current-fallback-unquote-rewriter
         default-fallback-unquote-rewriter
         unquote-rewrite
         lw->texexpr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/match
         racket/list
         redex/pict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data (rewriter)

(struct rewriter (symbol proc))
(struct compound-rewriter rewriter ())
(struct atomic-rewriter rewriter ())
(struct unquote-rewriter rewriter ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data (rewriters)

(struct rewriters (compounds atomics unquotes))

(define (make-rewriters . rws)
  (for/fold ([compounds (hash)]
             [atomics (hash)]
             [unquotes (hash)]
             #:result (rewriters compounds atomics unquotes))
            ([rw (in-list rws)])
    (match rw
      [(compound-rewriter symbol proc)
       (values (hash-set compounds symbol proc) atomics unquotes)]
      [(atomic-rewriter symbol proc)
       (values compounds (hash-set atomics symbol proc) unquotes)]
      [(unquote-rewriter symbol proc)
       (values compounds atomics (hash-set unquotes symbol proc))])))

(define current-rewriters (make-parameter (make-rewriters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound rewriter

(define (default-fallback-compound-rewriter l)
  `("(" ,@l ")"))

(define current-fallback-compound-rewriter
  (make-parameter default-fallback-compound-rewriter))

(define (compound-rewrite head args)
  (define compounds (rewriters-compounds (current-rewriters)))
  (define proc (hash-ref compounds (lw-e head) (λ _ #f)))
  (if proc
      (apply proc (list->texexpr args))
      ((current-fallback-compound-rewriter)
       (list->texexpr (cons head args) #:rewrite? #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atomic rewriter

(define (default-fallback-atomic-rewriter l)
  l)

(define current-fallback-atomic-rewriter
  (make-parameter default-fallback-atomic-rewriter))

(define (atomic-rewrite sym)
  (define atomics (rewriters-atomics (current-rewriters)))
  (define proc (hash-ref atomics sym (λ _ #f)))
  (if proc
      (proc sym)
      ((current-fallback-atomic-rewriter)
       (symbol->string sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unquote rewriter

(define (default-fallback-unquote-rewriter l)
  `("(" ,@l ")"))

(define current-fallback-unquote-rewriter
  (make-parameter default-fallback-unquote-rewriter))

(define (unquote-rewrite l)
  (define l* (filter lw? l))
  (match (list->grouped l*)
    [#f (map lw->texexpr l*)]
    [(cons x xt)
     (define unquotes (rewriters-unquotes (current-rewriters)))
     (define proc (hash-ref unquotes (lw-e x) (λ _ #f)))
     (if proc
         (apply proc (list->texexpr xt))
         ((current-fallback-unquote-rewriter)
          (list->texexpr (cons x xt))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lw conversion

(define (lw->texexpr l)
  (match-define (struct* lw ([e e] [unq? unq?])) l)
  (match e
    ["" null]
    [(? string? e) e]
    [(? symbol? e) (atomic-rewrite e)]
    [(? list? e)
     (if unq?
         (unquote-rewrite e)
         (list->texexpr e))]))

(define (list->texexpr l #:rewrite? [rw? #t])
  (define l* (filter lw? l))
  (define lws (list->grouped l*))
  (cond
    [(not lws) (map lw->texexpr l*)]
    [(empty? lws) null]
    [rw? (compound-rewrite (first lws) (rest lws))]
    [else (map lw->texexpr lws)]))

(define (list->grouped l)
  (match l
    [(or (list (struct* lw ([e "("])) lws ... (struct* lw ([e ")"])))
         (list (struct* lw ([e "["])) lws ... (struct* lw ([e "]"])))
         (list (struct* lw ([e "{"])) lws ... (struct* lw ([e "}"]))))
     lws]
    [_ #f]))
