#lang br

(require (for-syntax racket/list))

(define-syntax (ct-module-begin parse-tree)
  (define normalised-parse-tree (ct-module-special-forms (syntax->datum parse-tree)))
  #`#,normalised-parse-tree)
(provide (rename-out [ct-module-begin #%module-begin]))
         
(define-for-syntax (ct-module-special-forms parse-tree)
  (if (list? parse-tree)
      (filter (λ (in) (not (equal? in 'whitespace)))
              (map
               (λ (form) (if (member (if (list? form) (first form) #f) '(ct-program ct-list ct-final-list ct-expression atom ct-symbol ct-number ct-char whitespace))
                             (cond ;; this may seem unecessarily verbose, but case uses eq? not equal? (partial excuse, I know)
                               [(equal? (first form) 'whitespace) 'whitespace]
                               [(equal? (first form) 'ct-program) (ct-module-special-forms (ct-program form))]
                               [(equal? (first form) 'ct-list) (ct-module-special-forms (ct-list form))]
                               [(equal? (first form) 'ct-final-list) (ct-module-special-forms (ct-final-list form))]
                               [(equal? (first form) 'ct-expression) (ct-module-special-forms (ct-expression form))]
                               [(equal? (first form) 'atom) (ct-module-special-forms (atom form))]
                               [(equal? (first form) 'ct-symbol) (ct-module-special-forms (ct-symbol form))]
                               [(equal? (first form) 'ct-number) (ct-module-special-forms (ct-number form))]
                               [(equal? (first form) 'ct-char) (ct-module-special-forms (ct-char form))]
                               [else form])
                             (if (list? form)
                                 (ct-module-special-forms form)
                                 form)))
               parse-tree))
      parse-tree))

(define-for-syntax (ct-program dtm)
  (ct-module-special-forms (cons 'begin (drop dtm 1))))

(define-for-syntax (ct-list dtm)
  (ct-module-special-forms (if (equal? (second dtm) "(")
                               (drop (drop-right dtm 1) 2) ;; drop brackets
                               (cons 'list (second dtm))))) ;; quoted string

(define-for-syntax (ct-final-list dtm)
  (ct-module-special-forms (drop dtm 2))) ;; drop |

(define-for-syntax (ct-expression dtm)
  (cond
    [(equal? (last dtm) "]") ;; expr1[expr2] -> (ref expr2 expr1)
     `(ref ,(ct-module-special-forms (list-ref dtm (- (length dtm) 2)))
           ,(ct-module-special-forms (drop-right dtm 3)))]
    [(equal? (second dtm) "'")
     `(quote ,(ct-module-special-forms (cons (first dtm) (drop dtm 2))))]
    ;; due to trouble, deal with '`, quoting later
    [else (ct-module-special-forms (second dtm))]))

(define-for-syntax (atom dtm)
  (ct-module-special-forms (second dtm)))

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


(define (ref n l)
  (list-ref l n))

(provide begin list quote #%top #%app #%datum) ;; rackets #%top-interaction is racket syntax, need to make one that parses to citrus first
(provide write quasiquote unquote cons +) ;; note - need to make our own display functions, to deal with strings and to print #char instead of #\char and such
(provide ref)