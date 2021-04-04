#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide render-lang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/path
                     syntax/parse)
         redex/reduction-semantics
         redex/private/lang-struct
         racket/string
         racket/match
         latex-utils/scribble/unmap
         file/sha1
         "parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; language renderer

(define (render-lang lang-name x descs rhs sets)
  (define forms (make-hash '()))
  (define nts (compiled-lang-lang x))
  (define prods (map (render-nt lang-name forms) nts descs rhs sets))
  (format (current-language-template)
          (string-join (render-provides forms) "\n")
          (string-join prods "\n")))

(define (render-provides forms)
  (for/list ([(key val) (in-hash forms)])
    (format "\\providecommand~a{~a}" key val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nonterminal renderer

(define ((render-nt lang-name forms) cur-nt descs rhs? set)
  (match-define (nt name rhs*) cur-nt)
  (define set* (or ((current-set-procedure) set) ""))
  (cond
    [rhs?
     (define rhs-list
       (for/list ([cur-rhs (in-list rhs*)]
                  [desc (in-list descs)]
                  [k (in-naturals)])
         (match-define (rhs top-pat) cur-rhs)
         ((current-rhs-procedure)
          (zero? k)
          (render-top lang-name forms top-pat)
          desc)))
     (format (current-production-with-rhs-template)
             ((current-nt-procedure) name)
             set*
             (string-join rhs-list))]
    [else
     (format (current-production-no-rhs-template)
             ((current-nt-procedure) name)
             set*)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pattern renderer

(define (render-top lang-name forms x)
  (match x
    [(or (? list?) (? boolean?))
     (forms-set! lang-name forms x (render-pat x))]
    [_ (render-pat x)]))

(define/match (render-pat x)
  [(`(list ,x ...)) ((current-space-procedure) (map render-pat x))]
  [(`(repeat ,x #f #f)) ((current-repeat-procedure) (render-pat x))]
  [(`(nt ,name)) ((current-nt-procedure) name)]
  [(x) ((current-terminal-procedure) x)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `forms-set!`

(define current-hash-count (make-parameter 5))

(define lookup
  (hash #\0 #\g #\1 #\h #\2 #\i #\3 #\j #\4 #\k
        #\5 #\l #\6 #\m #\7 #\n #\8 #\o #\9 #\p))

(define (forms-set! lang-name forms form-datum form-val)
  (define form-name (datum->latex-name form-datum))
  (define form-key (format "\\~a~a" lang-name form-name))
  (hash-set! forms form-key form-val)
  form-key)

(define (datum->latex-name datum)
  (define port (open-input-string (format "~a" datum)))
  (define hash-str (substring (sha1 port) 0 (add1 (current-hash-count))))
  (string-upcase
   (list->string
    (for/list ([hash-char (in-string hash-str)])
      (hash-ref lookup hash-char (λ _ hash-char))))))
