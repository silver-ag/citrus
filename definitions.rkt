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
  ;; takes any number of expressions and prints them. optional #:out argument specifies output port 
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

(define (ct-drop n lst)
  (drop lst n))

(define (ct-take n lst)
  (take lst n))

;; LANGUAGE STUFF

;(define (parse grammar text)
;  (define parse-section (try-parse-rules grammar text)) ;; returns (matching-rule-name substring1 ... substringn) or #f
;  (if parse-section
;      (list (first parse-section)
;            (map (λ)
;      #f)))) ;; couldn't match, backtrack

; (define test-grammar
;  '(((lst (list (list #\( a ... #\) _))
;    ((int #rx"^[0-9]+") (sym #rx"^[-a-zA-Z]+")))
; ;; should do "(hello-world 1 (f 2) 3)" -> (lst (sym "hello-world") (int "1") (lst (sym "f") (int "2")) (int "3"))
;
; general: ((prod1 ... prodn) (term1 ... termn))

;(define (parse grammar text)
;  (define tokenised (tokenise (second grammar) (open-input-string text)))
;  (parse-tokens (first grammar) tokenised))
;

; you can turn greedy into lazy by reversing input and prepending _ ..., but it's probably a bad approach - especially since ... needs special handling
; (match (reverse '(#\( (sym "hello-world") #\space (int "1") #\space #\( (sym "f") #\space (int "2") #\) #\space (int "3") #\) #\( 1 #\)))
;    [(or (list _ ... #\) a ... #\()
;         (list #\( a ... #\)))
;     a])

(define (parse-tokens rules tokens (current-rule '()))
  (if (empty? (first rules))
      #f
      (let* [(productions (first rules)) (current-rule (if (empty? current-rule) (first productions) current-rule)) (terminals (second rules))]
        (cond
          [(symbol? (first current-rule))
           (define term (member (first current-rule) terminals (λ (x y) (equal? x (first y)))))
           (if term
               (if (equal? (first (first tokens)) (first (first term)))
                   (first tokens) ;; stops here, how can we make it continue? do we need to track a current-rule-set and all-rules?
                   #f)
               (if (member (first current-rule) productions (λ (x y) (equal? x (first y))))
                   (write current-rule) ;; figure out what to do here later
                   #f))]
          [(char? (first current-rule))
           (if (eq? (first tokens) (first current-rule))
               (parse-tokens rules (rest tokens) (rest current-rule))
               (parse-tokens (list (rest productions) terminals) tokens))]
          [else (display "shouldn't happen yet")]))))


(parse-tokens '(((#\( int #\))) ;; TESTING
                  ((int whatevs)))
                '(#\( (int 1) #\)))

;; tokeniser works - be aware that order of terminals matters!
(define (tokenise terminals text-port (result '()))
  (if (eq? (peek-char text-port) eof)
      result
      (tokenise terminals text-port (append result (list (next-token terminals text-port))))))

(define (next-token terminals text-port)
  (if (empty? terminals)
      (read-char text-port)
      (let ([attempt (regexp-try-match (second (first terminals)) text-port)])
        (if attempt
            (list (first (first terminals)) (first attempt))
            (next-token (rest terminals) text-port)))))


;; override racket definitions
(provide (rename-out (ct-display display)
                     (ct-write write)
                     (ct-print print)

                     (ct-drop drop)))
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