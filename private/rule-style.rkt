#lang racket/base

(provide rule-style
         current-rule-label?
         current-compact-threshold)

(require private-in)
(require (only-in (private-in redex/private/pict)
                  compact-vertical-min-width
                  basic-text)
         (only-in redex/private/core-layout
                  adjust)
         redex/pict
         pict)

(define current-rule-label? (make-parameter #t))
(define current-compact-threshold (make-parameter 450))
(define current-rule-label-padding (make-parameter 4))
(define current-label-margin (make-parameter 10))

(define (rp->pict-label rp)
  (cond [(rule-pict-info-computed-label rp) => bracket]
        [(rule-pict-info-label rp)
         (string->bracketed-label
          (format "~a" (rule-pict-info-label rp)))]
        [else (blank)]))

(define (string->bracketed-label str)
  (define m (regexp-match #rx"^([^_]*)(?:_([^_]*)|)$" str))
  (bracket
   (hbl-append
    ((current-text) (cadr m) (label-style) (label-font-size))
    (if (caddr m)
        ((current-text) (caddr m) `(subscript . ,(label-style)) (label-font-size))
        (blank)))))

(define (bracket label)
  (define w (+ (pict-width label) (* 2 (current-rule-label-padding))))
  (define h (+ (pict-height label) (* 2 (current-rule-label-padding))))
  (cc-superimpose (filled-rectangle w h #:color "WhiteSmoke" #:draw-border? #f)
                  label)
  #;(frame (inset label (current-rule-label-padding))
         #:color "Gray"))

(define (rule-style rps)
  (let* ([max-w (apply max
                       (compact-vertical-min-width)
                       (map pict-width
                            (append
                             (map rule-pict-info-lhs rps)
                             (map rule-pict-info-rhs rps))))]
         [scs (map (lambda (rp)
                     (rule-pict-info->side-condition-pict rp max-w))
                   rps)]
         [labels (map (lambda (rp)
                        (hbl-append (blank (label-space) 0) (rp->pict-label rp)))
                      rps)]
         [total-w (apply max
                         max-w
                         (append (map pict-width scs)
                                 (map (lambda (lbl)
                                        (+ max-w 2 (label-space) (pict-width lbl)))
                                      labels)))]
         [one-line
          (lambda (rp sc label)
            (let ([arrow (hbl-append (arrow->pict (rule-pict-info-arrow rp))
                                     (blank (arrow-space) 0))]
                  [lhs (rule-pict-info-lhs rp)]
                  [rhs (rule-pict-info-rhs rp)]
                  [spc (basic-text " " (default-style))]
                  [sep (blank (compact-vertical-min-width)
                              (reduction-relation-rule-separation))]
                  [add-label (lambda (p label)
                               (if (current-rule-label?)
                                   (htl-append
                                    (current-label-margin)
                                    p
                                    (inset label (- total-w (pict-width p) (pict-width label))
                                           0 0 0))
                                   p))])
              (if ((apply + (map pict-width (list lhs spc arrow spc rhs)))
                   . < .
                   (or (current-compact-threshold) max-w))
                  (list
                   (list (blank) (add-label (hbl-append lhs spc arrow spc rhs) label))
                   (list (blank) sc))
                  (list
                   (list (blank) (add-label lhs label))
                   (list arrow rhs)
                   (list (blank) sc)))))])
    (define rowss
      (map one-line rps scs  labels))
    (define all-cols
      (let ([min-left (blank (compact-vertical-min-width) 0)])
        (for*/fold ([all-cols (list min-left (blank))]) ([rows (in-list rowss)]
                                                         [row (in-list rows)])
          (for/list ([col (in-list all-cols)]
                     [p (in-list row)])
            (ltl-superimpose col (blank (pict-width p) 0))))))
    (apply vl-append
           (+ (reduction-relation-rule-extra-separation)
              (reduction-relation-rule-separation))
           (for/list ([rows (in-list rowss)])
             ((adjust 'reduction-relation-rule)
              (apply vl-append
                     (reduction-relation-rule-line-separation)
                     (for/list ([row (in-list rows)])
                       ((adjust 'reduction-relation-line)
                        (apply ht-append
                               (reduction-relation-rule-line-separation)
                               (for/list ([elem (in-list row)]
                                          [col (in-list all-cols)])
                                 (ltl-superimpose col elem)))))))))))
