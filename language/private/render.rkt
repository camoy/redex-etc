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

(define (render-lang lang-name lang base-lang descs rhs sets)
  (define forms (make-hash '()))
  (define nts (compiled-lang-lang lang))
  (define base-nts (if base-lang (compiled-lang-lang base-lang) '()))
  (define nts* (nts-combine base-nts nts))
  (define prods (map (render-nt lang-name base-lang forms) nts* descs rhs sets))
  (format (current-language-template)
          (string-join (render-provides forms) "\n")
          (string-join prods "\n")))

(define (nts-combine base-nts nts)
  (define base-hash
    (for/hash ([cur-nt (in-list base-nts)])
      (match-define (nt name rhss) cur-nt)
      (values name rhss)))
  (define nts*
    (for/list ([cur-nt (in-list nts)])
      (match-define (nt name rhss) cur-nt)
      (define base-rhss (hash-ref base-hash name (λ _ '())))
      (define rhss* (filter (λ (x) (not (member x base-rhss))) rhss))
      (and (not (null? rhss*)) (nt name rhss*))))
  (filter values nts*))

(define (render-provides forms)
  (for/list ([(key val) (in-hash forms)])
    (format "\\providecommand~a{~a}" key val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nonterminal renderer

(define ((render-nt lang-name base-lang forms) cur-nt descs rhs? set)
  (match-define (nt name rhs*) cur-nt)
  (define rhs-list
    (for/list ([cur-rhs (in-list rhs*)]
               [desc (in-list descs)]
               [k (in-naturals)])
      (match-define (rhs top-pat) cur-rhs)
      ((current-rhs-procedure)
       base-lang
       (zero? k)
       (render-top lang-name forms top-pat)
       desc)))
  ((current-production-procedure) base-lang rhs? name set rhs-list))

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
