#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide compound-rewriter
         atomic-rewriter
         unquote-rewriter
         make-rewriters
         define-rewriters
         rewriters-union
         current-rewriters
         current-lws
         !
         current-fallback-compound-rewriter
         default-fallback-compound-rewriter
         compound-rewrite
         current-fallback-atomic-rewriter
         default-fallback-atomic-rewriter
         atomic-rewrite
         current-fallback-unquote-rewriter
         default-fallback-unquote-rewriter
         unquote-rewrite
         lw->texexpr
         list->texexpr
         list->grouped)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/string
         racket/hash
         racket/match
         racket/list
         redex/pict
         #;redex/reduction-semantics)

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
;; rewriters functions

(define (rewriters-union . rws)
  (for/fold ([compounds (hash)]
             [atomics (hash)]
             [unquotes (hash)]
             #:result (rewriters compounds atomics unquotes))
            ([rw (in-list rws)])
    (match-define (rewriters compounds* atomics* unquotes*) rw)
    (values (hash-union compounds compounds* #:combine snd)
            (hash-union atomics atomics* #:combine snd)
            (hash-union unquotes unquotes* #:combine snd))))

(define (snd x y) y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewriter macro

(define-syntax (define-rewriters stx)
  (syntax-parse stx
    [(_ ?name
        (~alt (~optional (~seq #:compound (~seq ?ckey:expr ?cval:expr) ...))
              (~optional (~seq #:atomic (~seq ?akey:expr ?aval:expr) ...))
              (~optional (~seq #:unquote (~seq ?ukey:expr ?uval:expr) ...)))
        ...)
     #'(define ?name
         (make-rewriters
          (~? (~@ (compound-rewriter ?ckey ?cval) ...))
          (~? (~@ (atomic-rewriter ?akey ?aval) ...))
          (~? (~@ (unquote-rewriter ?ukey ?uval) ...))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound rewriter

(define (rewriter-pattern x) x)

(define-syntax ! (make-rename-transformer #'side-condition))

(define (default-fallback-compound-rewriter l mf?)
  `(@@ "(" (@ ,@l) ")"))

(define current-fallback-compound-rewriter
  (make-parameter default-fallback-compound-rewriter))

(define current-lws (make-parameter #f))

(define (compound-rewrite head args)
  (match-define (struct* lw ([unq? unq?] [metafunction? mf?])) head)
  (parameterize ([current-lws args])
    (cond
      [(and unq? (not (ignore-unquote?))) (unquote-rewrite head args)]
      [else
       (define compounds (rewriters-compounds (current-rewriters)))
       (define head* (lw-e head))
       (cond
         [(and (eq? head* '!)
               (hash-ref compounds (extract-symbol (second args)) (λ _ #f)))
          =>
          (λ (proc)
            (proc (lw->texexpr (first args))))]
         [(hash-ref compounds head* (λ _ #f))
          =>
          (λ (proc) (apply proc (map lw->texexpr args)))]
         [else
          ((current-fallback-compound-rewriter)
           (list->texexpr (cons head args) #:rewrite? #f)
           mf?)])])))

(define (extract-symbol x)
  (match (lw-e x)
    [(list (struct* lw ([e "'"])) 'spring (struct* lw ([e e]))) e]
    [_ #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atomic rewriter

(define (default-fallback-atomic-rewriter x)
  (if (symbol? x)
      `(texttt ,(symbol->string x))
      `(texttt ,x)))

(define current-fallback-atomic-rewriter
  (make-parameter default-fallback-atomic-rewriter))

(define (atomic-rewrite x)
  (define atomics (rewriters-atomics (current-rewriters)))
  (or (hash-ref atomics x (λ _ #f))
      ((current-fallback-atomic-rewriter) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unquote rewriter

(define ignore-unquote? (make-parameter #f))

(define (default-fallback-unquote-rewriter e)
  `(textsf (detokenize ,(e->string e))))

(define current-fallback-unquote-rewriter
  (make-parameter default-fallback-unquote-rewriter))

(define (unquote-rewrite head args)
  (define unquotes (rewriters-unquotes (current-rewriters)))
  (define proc (hash-ref unquotes (lw-e head) (λ _ #f)))
  (if proc
      (apply proc (parameterize ([ignore-unquote? #t])
                    (list->texexpr args)))
      ((current-fallback-unquote-rewriter)
       (cons head args))))

(define (lw->string l)
  (if (lw? l)
      (e->string (lw-e l))
      ""))

(define (e->string e)
  (match e
    [(? string?) e]
    [(? symbol?) (symbol->string e)]
    [(? list?)
     (define e* (or (list->grouped e) e))
     (format "(~a)" (string-join (map lw->string e*)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lw conversion

(define (lw->texexpr l)
  (match-define (struct* lw ([e e])) l)
  (match e
    ["" null]
    [(or (? string? e) (? symbol? e)) (atomic-rewrite e)]
    [(? list? e) (list->texexpr e)]))

(define (list->texexpr l #:rewrite? [rw? #t])
  (define lws (list->grouped l))
  (cond
    [(not lws) (map lw->texexpr l)]
    [(empty? lws) null]
    [rw? (compound-rewrite (first lws) (rest lws))]
    [else (map lw->texexpr lws)]))

(define (list->grouped l)
  (match l
    [(or (list (struct* lw ([e "("])) lws ... (struct* lw ([e ")"])))
         (list (struct* lw ([e "["])) lws ... (struct* lw ([e "]"])))
         (list (struct* lw ([e "{"])) lws ... (struct* lw ([e "}"]))))
     lws]
    [(list (struct* lw ([e ""])) 'spring (struct* lw ([e (? list? e)])))
     (list->grouped e)]
    [_ #f]))
