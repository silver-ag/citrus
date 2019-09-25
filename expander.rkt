#lang br

(require (for-syntax racket/list))

(define-syntax (ct-module-begin parse-tree)
  (define normalised-parse-tree (ct-special-forms (syntax->datum parse-tree)))
  ;(write normalised-parse-tree)
  #`#,normalised-parse-tree)
(provide (rename-out [ct-module-begin #%module-begin]))
         
(define-for-syntax (ct-special-forms parse-tree)
  (if (list? parse-tree)
      (filter (λ (in) (not (equal? in 'whitespace)))
              (map
               (λ (form) (if (member (if (list? form) (first form) #f) '(ct-program ct-list ct-final-list ct-expression ct-atom ct-symbol ct-number ct-char whitespace))
                             (cond ;; this may seem unecessarily verbose, but case doesn't seem to work
                               [(equal? (first form) 'whitespace) 'whitespace]
                               [(equal? (first form) 'ct-program) (ct-special-forms (ct-program form))]
                               [(equal? (first form) 'ct-list) (ct-special-forms (ct-list form))]
                               [(equal? (first form) 'ct-final-list) (ct-special-forms (ct-final-list form))]
                               [(equal? (first form) 'ct-expression) (ct-special-forms (ct-expression form))]
                               [(equal? (first form) 'ct-atom) (ct-special-forms (ct-atom form))]
                               [(equal? (first form) 'ct-symbol) (ct-special-forms (ct-symbol form))]
                               [(equal? (first form) 'ct-number) (ct-special-forms (ct-number form))]
                               [(equal? (first form) 'ct-char) (ct-special-forms (ct-char form))]
                               [else form])
                             (if (list? form)
                                 (ct-special-forms form)
                                 form)))
               parse-tree))
      parse-tree))

(define-for-syntax (ct-program dtm)
  (ct-special-forms (cons 'begin (drop dtm 1)))) ;; expr1 ... exprn -> (begin expr1 ... exprn)

(define-for-syntax (ct-list dtm)
  (ct-special-forms (if (equal? (second dtm) "(")
                               (drop (drop-right dtm 1) 2) ;; "(" expr1 ... exprn ")" -> (expr1 ... exprn)
                               (cons 'list (second dtm))))) ;; QUOTED-STRING -> (char1 ... charn)

(define-for-syntax (ct-final-list dtm)
  (cond
    [(equal? (second dtm) "'") ;; 'expr -> (quote expr)
     (cons 'quote (list (ct-special-forms (cons (first dtm) (drop dtm 2)))))]
    [(equal? (second dtm) "`") ;; `expr -> (quasiquote expr)
     (cons 'quasiquote (list (ct-special-forms (cons (first dtm) (drop dtm 2)))))]
    [(equal? (second dtm) ",") ;; ,expr -> (unquote expr)
     (cons 'unquote (list (ct-special-forms (cons (first dtm) (drop dtm 2)))))]
    [else (ct-special-forms (drop dtm 2))])) ;; (ct-final-list "|" expr1 ... exprn) -> (expr1 ... exprn)

(define-for-syntax (ct-expression dtm)
  (cond
    [(equal? (last dtm) "]") ;; expr1[expr2] -> (ref expr2 expr1)
     `(ref ,(ct-special-forms (list-ref dtm (- (length dtm) 2)))
           ,(ct-special-forms (drop-right dtm 3)))]
    [(equal? (second dtm) "'") ;; 'expr -> (quote expr)
     (cons 'quote (list (ct-special-forms (cons (first dtm) (drop dtm 2)))))]
    [(equal? (second dtm) "`") ;; `expr -> (quasiquote expr)
     (cons 'quasiquote (list (ct-special-forms (cons (first dtm) (drop dtm 2)))))]
    [(equal? (second dtm) ",") ;; ,expr -> (unquote expr)
     (cons 'unquote (list (ct-special-forms (cons (first dtm) (drop dtm 2)))))]
    [else (ct-special-forms (second dtm))]))

(define-for-syntax (ct-atom dtm)
  (ct-special-forms (second dtm))) ;; (atom expr) -> expr

(define-for-syntax (ct-symbol dtm)
  (string->symbol (second dtm)))

(define-for-syntax (ct-number dtm)
  (string->number (second dtm)))

(define-for-syntax (ct-char dtm)
  (define char-code (substring (second dtm) 1))
  (cond
    [(equal? char-code "space") #\space]
    [(equal? char-code "\\n") #\newline]
    [(equal? char-code "\\t") #\tab]
    [(regexp-match? #rx"[\\]x[0-9][0-9]" char-code) (integer->char (string->number (substring char-code 2) 16))]
    [(eq? (string-length char-code) 1) (string-ref char-code 0)]
    [else (error (format "not a valid character: #~a" char-code))]))


(provide #%top #%app #%datum) ;; rackets #%top-interaction is racket syntax, need to make one that parses to citrus first
(require "definitions.rkt")
(provide (all-from-out "definitions.rkt"))