#lang br
(require brag/support)
(require "parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokeniser port)))
  (define module-datum `(module ct-mod "expander.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define (make-tokeniser port)
  (define (next-token)
    (define ct-lexer
      (lexer
       [(concatenation ";" (repetition 0 +inf.0 (char-complement "\n")) "\n") (next-token)]
       [(concatenation "\"" (repetition 0 +inf.0 (union (char-complement "\"") (concatenation "\\" (union "\"" "\\")))) "\"")
        (token 'QUOTED-STRING (drop (drop-right (string->list lexeme) 1) 1))]
       [(concatenation "{lang"
                       (repetition 0 4 any-char)
                       (complement (concatenation (repetition 0 +inf.0 any-char) "lang}" (repetition 0 +inf.0 any-char)))
                       (repetition 0 4 any-char) "lang}")
        (token 'LANG-BLOCK-CONTENT (list->string (drop (drop-right (string->list lexeme) 5) 5)))]
       [(concatenation (union "-" "") (repetition 1 +inf.0 numeric) (union "" (concatenation "." (union "" (repetition 0 +inf.0 numeric)))))
        (token 'NUMBER lexeme)] ;; note to avoid possible future bugs - if the number case comes after the symbol one, -1 is matched as a symbol
       [(concatenation "#" (repetition 0 +inf.0 (char-complement (char-set " \n\t)]"))))
        (token 'CHAR lexeme)] ;; again, must come after symbol (unless we later decide to disallow # anywhere in a symbol name)
       [(concatenation (repetition 0 +inf.0 (char-complement (union (char-set "`',:|()[]{}\n\"\'\\") #\space)))
                         (char-complement (union (char-set "`',:0123456789|()[]{}\n\"\'\\") #\space))
                         (repetition 0 +inf.0 (char-complement (union (char-set "`',:|()[]{}\n\"\'\\") #\space))))
        (token 'SYMBOL lexeme)]
       [(char-set "\n\t ")
        (token 'WHITESPACE lexeme)]
       [any-char lexeme]))
    (ct-lexer port))  
  next-token)

