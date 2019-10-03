#lang racket

(require (for-syntax racket/list)) ;; for grammar macro

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

;; parse is done, next up is apply-lang with arbitrary expanders (we can make a nice (rule-expander ...) macro later, but need to support more complex forms so sublanguages can do macros and stuff).
;; it may be helpful to know that eval with current namespace works in and for citrus:
;(define-namespace-anchor a)
;(define ns |namespace-anchor->namespace a)
;(eval '(display "hello " |cons 1 '|2 3) ns)
; -> 'hello (1 2 3)'
;; also for later, a grammar should be able to have an 'ignore-whitespace flag that makes parse remove all whitespace from the lexed source before continuing, so you don't have to deal with it in your grammar by hand.

;; apply-lang
(define-syntax (apply-lang stx)
  (define dtm (syntax->datum stx))
  (define lang (second dtm))
  (define text (third dtm))
  (write lang)
  ;(write
  (datum->syntax
    stx
;     (let ([ns (make-base-namespace)])
;     (eval
;      `(module lang-run citrus/expander ;; citrus/expander requires definitions.rkt, creating a loop?
         `(,(second lang) (parse ,(first lang) ,text))) )
;      ns))))
     ;(eval '(require 'lang-run) ns))));)


;; parse and supporting functions
(define (parse grammar text)
  (define tokens (tokenise (second grammar) (open-input-string (list->string text))))
  (let-values ([(parse-result empty-tokens) (parse-next grammar tokens grammar)])
    parse-result))

(define (parse-next current-rules tokens all-rules (result '()))
  (cond
    [(empty? (first current-rules))
      (values #f #f)]
    [(empty? tokens) (values result '())] ;; finished parse 
    [else
     (let-values ([(attempt-item attempt-tokens) (parse-with-rule (first (first current-rules)) tokens all-rules)])
       (define-values (backtrack-item backtrack-tokens)
         (if attempt-item
             (parse-next all-rules attempt-tokens all-rules (append result (list attempt-item)))
             (parse-next (list (rest (first current-rules)) (second current-rules)) tokens all-rules result)))
       (if backtrack-item
           (values backtrack-item backtrack-tokens)
           (parse-next (list (rest (first current-rules)) (second current-rules)) tokens all-rules result)))]))

(define (parse-with-rule rule tokens rules (result '()))
  (if (empty? (second rule))
      (values ;; pass back lazy parse and remaining tokens
       (cons (first rule) result)
       tokens)
      (cond
        [(empty? tokens)
         (if (and (list? (first (second rule))) (equal? (first (first (second rule))) '?))
             (parse-with-rule (list (first rule) (rest (second rule))) tokens rules result)
             (values #f #f))]
        [(symbol? (first (second rule)))
         (cond
           [(member (first (second rule)) (second rules) (λ (x y) (equal? x (first y)))) ;; terminal
            (if (and (list? (first tokens)) (equal? (first (first tokens)) (first (second rule))))
                (parse-with-rule (list (first rule) (rest (second rule))) (rest tokens) rules (append result (list (first tokens))))
                (values #f #f))]
           [(member (first (second rule)) (first rules) (λ (x y) (equal? x (first y)))) ;; production
            (define-values (item-next tokens-next) (parse-with-rule (first (member (first (second rule)) (first rules) (λ (x y) (equal? x (first y))))) tokens rules))
            ;(parse-next rules tokens rules))
            (cond
              [(not item-next) (values #f #f)]
              [(equal? (first (second rule)) (first item-next))
               (parse-with-rule (list (first rule) (rest (second rule))) tokens-next rules (append result (list item-next)))] )]
;              [else (values #f #f)])]
           [else (error (format "parse error: unrecognised rule '~a': ~a" (first (second rule)) rule))])]
        [(char? (first (second rule)))
         (if (eq? (first (second rule)) (first tokens))
             (parse-with-rule (list (first rule) (rest (second rule))) (rest tokens) rules (append result (list (first tokens))))
             (values #f #f))]
        [(list? (first (second rule))) ;; special rule (* ...) or (or ...)
         (case (first (first (second rule)))
           [(*) ;; the strategy here is, given ((* a) b c), to try (a b c) and then if that fails (* a a) b c) 
            (if (> (- (length (first (second rule))) 1) (length tokens)) ;; -1 to count the '*'
                (values #f #f)
                (let-values ([(attempt-item attempt-tokens)
                              (parse-with-rule (list (first rule) (append (rest (first (second rule))) (rest (second rule)))) tokens rules result)])
                  (if attempt-item
                      (let-values ([(rest-item rest-tokens) 
                                    (parse-with-rule (list (first rule) (rest (second rule))) attempt-tokens rules (rest attempt-item))])
                        (if rest-item
                            (values rest-item rest-tokens)
                            (parse-with-rule (list (first rule) (cons (append (first (second rule)) (list (second (first (second rule))))) (rest (second rule)))) tokens rules result)))
                      (parse-with-rule (list (first rule) (cons (append (first (second rule)) (list (second (first (second rule))))) (rest (second rule)))) tokens rules result))))]
           [(or) ;; similarly, given ((or a b c) d) we try (a d) then (or b c) d)
            (if (eq? (length (first (second rule))) 1)
                (values #f #f) ;; no more options
                (let-values ([(attempt-item attempt-tokens)
                              (parse-with-rule (list (first rule) (cons (second (first (second rule))) (rest (second rule)))) tokens rules result)])
                  (if attempt-item
                      (values attempt-item attempt-tokens)
                      (parse-with-rule (list (first rule) (cons (cons 'or (drop (first (second rule)) 2)) (rest (second rule)))) tokens rules result))))]
           [(?) ;; ((? a b) c) -> (a b c), then (c)
            (let-values ([(attempt-item attempt-tokens)
                          (parse-with-rule (list (first rule) (rest (second rule))) tokens rules result)])
              (if attempt-item
                  (values attempt-item attempt-tokens)
                  (parse-with-rule (list (first rule) (append (rest (first (second rule))) (rest (second rule)))) tokens rules result)))]
           [(quote) ;; literal list, given ((quote (a b)) c) try (a b c) only. for use in things like ((or '(a b) c) d)
            (parse-with-rule (list (first rule) (append (second (first (second rule))) (rest (second rule)))) tokens rules result)]
           [else (display (format "no such special grammar form: '~a' (accepts '*', 'or' and '?'): ~a" (first (first (second rule))) (second rule)))])]
        [else (display "shouldn't happen")])))

(define (parse-with-rule-* rule tokens rules (result '()))
  (define-values (attempt-item attempt-tokens)
    (parse-with-rule rule tokens rules result))
  (if attempt-item
      (let-values ([(cont-item cont-tokens) (parse-with-rule rule attempt-tokens rules (append result (list attempt-item)))])
        (if cont-item
            (values cont-item cont-tokens)
            (parse-with-rule-* rule attempt-tokens rules (append result (list attempt-item)))))
      (values #f #f)))

;; tokeniser - be aware that order of terminals matters!
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

;; grammar generating macro
(define-syntax (grammar stx)
  (define dtm (syntax->datum stx))
  (datum->syntax
   stx
   `(quote
     ,(foldl
       (λ (rule g) (case (first rule)
                     [(production) (list (cons `(,(second rule) ,(drop rule 2)) (first g)) (second g))] ;; (production name part1 ... partn) -> (name (part1 ... partn)
                     [(terminal) (list (first g) (cons `(,(second rule) ,(regexp (string-append "^" (list->string (rest (third rule)))))) (second g)))] ;; (terminal name "regex-string") -> (name #rx"^regex-string")
                     [else (error (format "unrecognised rule type '~a' (must be terminal or production)" (first rule)))]))
       '(()())
       (rest dtm)))))


; TEST WORKS:
;(define test-grammar
;  (grammar
;   (production lst (? #\space) #\( (? #\space) (* '(elem (? #\space))) #\) (? #\space))
;   (production elem (or int sym lst))
;   (terminal int "[0-9]+")
;   (terminal sym "[-a-zA-Z]+")))
;(parse test-grammar "(hello-world 1 (f 2) 3)")

;; there may be a lingering bug in the parser
;; I fixed one where * rules failed by matching lazily and then not trying again if the rest failed, but I haven't checked for analogous errors in other special rules
;(apply-lang ((grammar (production lst (* (or int lst))) (terminal int (#\[ #\0 #\- #\9 #\] #\+))) (λ (ast) (write ast)))
 ;           '(#\a #\1 #\3 #\a #\8))

;; override racket definitions
(provide (rename-out (ct-display display)
                     (ct-write write)
                     (ct-print print)

                     (ct-drop drop)))
;; things that aren't already defined
(provide ref       ;; this is used in the normalisation of (1 2 3)[n], be careful changing how it works
         apply-lang
         parse
         grammar
         tokenise        ;- this stuff shouldn't be directly available, but has to be available to the parsed module. put in a module of their own?
         next-token      ;
         parse-next      ;
         parse-with-rule);
;; things provided through from racket
(provide list       ;;|
         begin      ;;|
         quote      ;;|- these are used in normalised forms, any redefinition in citrus must still work sufficiently similarly
         quasiquote ;;|  (or we can change the normalisation)
         unquote    ;;|
         cons
         car
         cdr)