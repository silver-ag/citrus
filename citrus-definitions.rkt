#lang racket

(require predicates)

(define ct-display
  (λ args
    (display (list->string (flatten args)))))

(define ct-write
  (λ args
    (map (λ (a) (if ((listof? char?) a)
                    (write (list->string a))
                    (write a))) args)
    (values))) ;; return nothing, like original write

(define (ct-print
  (λ args
    (map (λ (a) (if ((listof? char?) a)
                    (print (list->string a))
                    (print a))) args)
    (values)))


;; redefined functions
(provide (rename-out (ct-display display) (ct-write write) (ct-print print)))
;; otherwise, provide everything from plain racket
(provide (except-out (all-from-out racket) display write print))
