#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide with-style

         render-metafunction/style
         render-metafunctions/style
         render-reduction-relation/style

         default-atomic-rewriters
         default-compound-rewriters
         default-unquote-rewriters

         substitute-rw
         lookup-rw
         macro-rw
         unquote-rw

         current-arrow-hash
         current-serif-font
         current-sans-serif-font
         current-mono-font
         current-font-size)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (except-in unstable/gui/redex set-cons-rw)
         "rule-style.rkt"
         racket/class
         racket/draw
         racket/match
         racket/list
         redex/pict
         pict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define-syntax-rule (with-style ?body ...)
  ;; style parameters
  (let ([serif-font (current-serif-font)]
        [sans-serif-font (current-sans-serif-font)]
        [mono-font (current-mono-font)]
        [size (current-font-size)])
    ;; set up arrows
    (for ([(arr str) (in-hash (current-arrow-hash))])
      (define (arr->pict)
        ((current-text) str (default-style) (default-font-size)))
      (set-arrow-pict! arr arr->pict))

    ;; execute body
    (parameterize
        ([judgment-form-show-rule-names #f]
         [white-square-bracket styled-white-square-bracket]
         [label-style serif-font]
         [grammar-style sans-serif-font]
         [paren-style mono-font]
         [literal-style mono-font]
         [metafunction-style sans-serif-font]
         [non-terminal-style (cons 'italic serif-font)]
         [non-terminal-subscript-style
          (list* 'italic 'subscript serif-font)]
         [non-terminal-superscript-style
          (list* 'italic 'superscript serif-font)]
         [default-style serif-font]
         [label-font-size size]
         [metafunction-font-size size]
         [default-font-size size]
         [rule-pict-style rule-style]
         [current-atomic-rewriters
          (append (default-atomic-rewriters)
                  (current-atomic-rewriters))]
         [current-compound-rewriters
          (append (default-compound-rewriters)
                  (current-compound-rewriters))]
         [current-unquote-rewriters
          (append (plist-map-keys unquoted-function?
                                  (default-unquote-rewriters))
                  (current-unquote-rewriters))])
      (with-rewriters
        (λ () ?body ...)))))

;; Use normal parentheses instead of denotation brackets for metafunctions
(define (styled-white-square-bracket open?)
  (let ([text (current-text)])
    (text (if open? "(" ")")
          (default-style)
          (default-font-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stylish renderers

(define-syntax-rule (render-metafunction/style args ...)
  (with-style
    (render-metafunction args ...)))

(define-syntax-rule (render-metafunctions/style args ...)
  (with-style
    (render-metafunctions args ...)))

(define (render-reduction-relation/style . args)
  (with-style
    (apply render-reduction-relation args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atomic rewriters

(define default-atomic-rewriters
  (make-parameter
   (list '-> "→"
         '->i "→i"
         'real "ℝ"
         'number "ℂ"
         'integer "ℤ"
         'natural "ℕ"
         'hole "□"
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound rewriters

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

(define ((macro-rw name) lws)
  (match-define (list L _ e ...) lws)
  (list* L (text name (current-mono-font)) " " e))

(define default-compound-rewriters
  (make-parameter
   (list '~ (bracket-rw '("⟨" "⟩"))
         '⌢ (binary-rw " ⌢ ")
         'substitute (substitute-rw " → ")
         'substitute* (substitute*-rw " → ")
         'ext (substitute*-rw " ↦ ")
         'lookup lookup-rw
         'mon* (macro-rw "mon")
         'let (macro-rw "let"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unquote rewriters

(define ((unquoted-function? f) l)
  (match (lw-e l)
    [(list L g _ ... R)
     (and (lw? g) (eq? (lw-e g) f))]
    [_ #f]))

(define ((unquote-rw e*) x)
  (struct-copy lw x [e e*] [unq? #f]))

(define (plist-map-keys f plist)
  (match plist
    [(list k v more ...)
     (list* (f k) v (plist-map-keys f more))]
    ['() '()]))

(define default-unquote-rewriters (make-parameter null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameters

(define current-arrow-hash
  (make-parameter #hash((:-> . " ↦ "))))

(define current-serif-font
  (make-parameter "Linux Libertine"))

(define current-sans-serif-font
  (make-parameter "Linux Biolinum"))

(define current-mono-font
  (make-parameter
   (make-object font% 13 "DejaVu Sans Mono" 'default 'normal)))

(define current-font-size
  (make-parameter 22))
