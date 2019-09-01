#lang br/quicklang

(require "parser.rkt")
(require brag/support)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokeniser port)))
  (define module-datum `(module citrus citrus/expander ,parse-tree))
  (datum->syntax #f module-datum))

(define (make-tokeniser port)
  (define (next-token)
      (define ct-lexer
        (lexer
         [(concatenation ";" (repetition 0 +inf.0 (char-complement "\n")) "\n") (next-token)]
         [(concatenation (union "-" "") (repetition 1 +inf.0 numeric) (union "" (concatenation "." (union "" (repetition 0 +inf.0 numeric)))))
          (token 'NUMBER lexeme)]
         [(concatenation "\"" (repetition 0 +inf.0 (union (char-complement "\"") (concatenation "\\" (union "\"" "\\")))) "\"")
          (token 'QUOTED-STRING (cons list (drop (drop-right (string->list lexeme) 1) 1)))]
         [(concatenation (repetition 0 +inf.0 (char-complement (union (char-set "|()[]{}\n\"\'\\") #\space)))
                         (char-complement (union (char-set "0123456789|()[]{}\n\"\'\\") #\space))
                         (repetition 0 +inf.0 (char-complement (union (char-set "|()[]{}\n\"\'\\") #\space))))
          (token 'SYMBOL lexeme)]
         [(repetition 1 +inf.0 (union (char-set "\n\t") #\space)) (next-token)]
         [any-char lexeme]))
      (ct-lexer port))
    next-token)

(provide read-syntax)
