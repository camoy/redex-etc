#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide acmart-style!
         render-language/style
         render-metafunction/style
         render-reduction-relation/style
         current-arrow-hash
         current-serif-font
         current-sans-serif-font
         current-font-size)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (except-in unstable/gui/redex set-cons-rw)
         racket/match
         racket/list
         redex/pict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define (acmart-style!)
  ;; style parameters
  (let ([serif-font (current-serif-font)]
        [sans-serif-font (current-sans-serif-font)]
        [size (current-font-size)])
    (judgment-form-show-rule-names #f)
    #;(rule-pict-style rr-style)
    (white-square-bracket styled-white-square-bracket)
    (label-style serif-font)
    (grammar-style sans-serif-font)
    #;(non-terminal-set nt-set)
    (paren-style sans-serif-font)
    (literal-style sans-serif-font)
    (metafunction-style sans-serif-font)
    (non-terminal-style (cons 'italic serif-font))
    (non-terminal-subscript-style (list* 'italic 'subscript serif-font))
    (non-terminal-superscript-style (list* 'italic 'superscript serif-font))
    (default-style serif-font)
    (label-font-size size)
    (metafunction-font-size size)
    (default-font-size size))

  ;; rewriters
  (apply add-atomic-rewriters! atomic-rewriters)
  (apply add-compound-rewriters! compound-rewriters)
  #;(apply add-unquote-rewriters!
         (let go ([xs unquote-rewriters])
           (match xs
             ['() '()]
             [(list f g ys ...)
              (list* (unquoted-function? f)
                     (λ (l)
                       (struct-copy lw l [e (g (lw-e l))]))
                     (go ys))])))

  ;; set up arrows
  (for ([(arr str) (in-hash (current-arrow-hash))])
    (define (arr->pict)
      ((current-text) str (default-style) (default-font-size)))
    (set-arrow-pict! arr arr->pict))
  )

;; Use normal parentheses instead of denotation brackets for metafunctions
(define (styled-white-square-bracket open?)
  (let ([text (current-text)])
    (text (if open? "(" ")")
          (default-style)
          (default-font-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stylish renderers

(define (render-language/style . args)
  (with-rewriters
    (λ ()
      (apply render-language args))))

(define-syntax-rule (render-metafunction/style args ...)
  (with-rewriters
    (λ ()
      (render-metafunction args ...))))

(define (render-reduction-relation/style . args)
  (with-rewriters
    (λ ()
      (apply render-reduction-relation args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewriters

(define atomic-rewriters
  (list '-> "→"
        'real "ℝ"
        'number "ℂ"
        'integer "ℤ"
        'natural "ℕ"
        'hole "□"
        ))

(define ((substitute-rw arr )lws)
  (match-define (list L _ e x y R) lws)
  (list* "" e ((substitute-pair-rw arr) (list L x y R))))

(define ((substitute*-rw arr) lws)
  (match-define (list L _ e x+y ... R) lws)
  (define pairs (append-map (substitute-pair-...-rw arr) x+y))
  (list* "" e pairs))

(define ((substitute-pair-...-rw arr) lw)
  (define lws (lw-e lw))
  (if (list? lws)
      ((substitute-pair-rw arr) lws)
      '(" …")))

(define ((substitute-pair-rw arr) lws)
  (match-define (list L x y R) lws)
  (list "[" x arr y "]"))

(define (lookup-rw lws)
  (match-define (list L _ f x R) lws)
  (list "" f "(" x ")"))

(define compound-rewriters
  (list '~ (bracket-rw '("⟨" "⟩"))
        '⌢ (binary-rw " ⌢ ")
        'substitute (substitute-rw " → ")
        'substitute* (substitute*-rw " → ")
        'ext (substitute*-rw " ↦ ")
        'lookup lookup-rw))

#;(define ((unquoted-function? f) l)
  (match (lw-e l)
    [(list L g _ ... R)
     (and (lw? g) (eq? (lw-e g) f))]
    [_ #f]))

#;(define (set-cons-rw lws)
  (match-define (list L _ x y R) lws)
  (list "" "{" x "}" " ∪ " y))

#;(define unquote-rewriters
  (list 'set-cons set-cons-rw))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

(define current-arrow-hash
  (make-parameter #hash((:-> . " ↦ "))))

(define current-serif-font
  (make-parameter "Linux Libertine"))

(define current-sans-serif-font
  (make-parameter "Linux Biolinum"))

(define current-font-size
  (make-parameter 22))
