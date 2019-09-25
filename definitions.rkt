#lang racket

;; OUTPUT

(define (ct-display #:out (out (current-output-port)) . args)
  ;; takes any number of expressions and displays them. optional #:out argument specifies output port 
  (map (λ (arg)
         (define processed (if ((listof char?) arg)
                               (list->string arg)
                               arg))
         (display processed out)
         processed) ;; should this be (apply values processed)?
       args))

(define (ct-write #:out (out (current-output-port)) . args)
  ;; takes any number of expressions and writes them. optional #:out argument specifies output port 
  (map (λ (arg)
         (define processed (if ((listof char?) arg)
                               (list->string arg)
                               arg))
         (write processed out)
         processed) ;; (apply values processed)?
       args))

(define (ct-print #:out (out (current-output-port)) . args)
  ;; takes any number of expressions and writes them. optional #:out argument specifies output port 
  (map (λ (arg)
         (define processed (if ((listof char?) arg)
                               (list->string arg)
                               arg))
         (print processed out)
         processed) ;; (apply values processed)?
       args))

;; LIST PROCESSING

(define (ref n l)
  (list-ref l n))

;; override racket definitions
(provide (rename-out (ct-display display)
                     (ct-write write)
                     (ct-print print)))
;; things that aren't already defined
(provide ref)       ;; this is used in the normalisation of (1 2 3)[n], be careful changing how it works
;; things provided through from racket
(provide list       ;;|
         begin      ;;|
         quote      ;;|- these are used in normalised forms, any redefinition in citrus must still work sufficiently similarly
         quasiquote ;;|  (or we can change the normalisation)
         unquote    ;;|
         cons
         car
         cdr)