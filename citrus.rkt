#lang br/quicklang

(require (for-syntax racket/list))

;(define-namespace-anchor a)
;(define ns (namespace-anchor->namespace a))

(define-syntax (ct-list stx)
  ;; drop the "(" and if applicable ")" from a ct-list, and make it just a list
  (define dtm (syntax->datum stx))
  (datum->syntax stx (if (string? (second dtm))
                         (if (string=? (second dtm) "(")
                             (if (string? (last dtm))
                                 (cons 'list (drop (drop-right dtm 1) 2))
                                 (cons 'list (drop dtm 2)))
                             (cons 'list (second dtm)))
                         (cons 'list (second dtm)))))

(define-syntax (ct-final-list stx)
  ;; drop "|" and if applicable ")" and make it just a list
  (define dtm (syntax->datum stx))
  (datum->syntax stx (cons list (drop (if (string? (last dtm)) ;; must be ")"
                                          (drop-right dtm 1)
                                          dtm) 2))))

(define-syntax (ct-final-id-list stx)
  ;; drop "'", "|" and if applicable ")" and make it just a quoted list
  (define dtm (syntax->datum stx))
  (datum->syntax stx (cons 'quote (list (cons list (drop (if (string? (last dtm)) ;; must be ")"
                                                             (drop-right dtm 1)
                                                             dtm) 3))))))

(define-syntax (ct-id stx)
  ;; remove "'" and make it a quote
  (datum->syntax stx `(quote ,(third (syntax->datum stx)))))

(define-syntax (citrus-program stx)
  (define namespace-name (gensym))
  (datum->syntax stx (cons 'begin (cons `(begin
                                           (define-namespace-anchor a)
                                           (define ,namespace-name (namespace-anchor->namespace a))
                                           (eval '(require citrus/citrus-definitions) ,namespace-name))
                                        (map (λ (expr) `(eval ,expr ,namespace-name)) (drop (syntax->datum stx) 1))))))

(define-syntax (ct-symbol stx)
  (datum->syntax stx `(string->symbol ,(second (syntax->datum stx)))))

(define-syntax (ct-number stx)
  (datum->syntax stx `(string->number ,(second (syntax->datum stx)))))

(provide citrus-program ct-final-list ct-final-id-list ct-list ct-symbol ct-number ct-id) ;; citrus parse tree macros
(provide quote string->symbol require string->number begin eval list define-namespace-anchor namespace-anchor->namespace define) ;; things those macros depend on