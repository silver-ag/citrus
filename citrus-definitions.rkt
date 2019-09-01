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

;; redefined functions
(provide (rename-out (ct-display display) (ct-write write)))
;; otherwise, provide everything from plain racket
(provide (except-out (all-from-out racket) display write))