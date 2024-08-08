#lang racket

(provide

 all-equal? 

 #; {[Setof X] [X -> X] -> (U False [Setof X])}
 set-member
 set-such-that

 =>
 when*
 unless*
 and*

 random-pick

 #; {-> Void} 
 with-error-to-string

 #; (dev/null e0 e ...)
 dev/null 

 #; {[Cons X [Listof X]] -> [Listof X]}
 all-but-last

 #; {[Cons X [Listof X]] -> (values [Listof X] X)}
 split-off-last)

;; TODO make them more like the real thing  

;; ---------------------------------------------------------------------------------------------------
(require syntax/parse/define (for-syntax syntax/parse))
(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define-syntax => (lambda (stx) (raise-syntax-error '=> "used out of context")))

(define-simple-macro
  (when* condition:expr (~literal =>) body:expr)
  (let ([it condition]) (when it (body it))))

(define-simple-macro
  (unless* condition:expr (~literal =>) body:expr)
  (let ([it condition]) (unless it (body it))))

(define-syntax (and* stx)
  (syntax-parse stx
    [(_) #'(and)]
    [(_ e1:expr) #'(and e1)]
    [(_ e1:expr (~literal =>) e-next:expr e2:expr ...)
     #'(let ([it e1]) (and* it (e-next it) e2 ...))]
    [(_ e1:expr e2:expr ...) #'(and e1 (and* e2 ...))]))

(module+ test
  (check-equal? (when* (sin (/ pi 2)) => (λ (it) (- it 1.0))) 0.0) ;; ok ok
  (check-equal? (unless* (sin (/ pi 2)) => (λ (it) (- it 1.0))) (void))

  (check-true (and*))
  (check-true (and* #t))
  (check-true (and* (+ 1 1) => (λ (it) (> 3 it))))
  (check-false (and* (+ 1 1) => (λ (it) (> 3 it)) #f)))

;; ---------------------------------------------------------------------------------------------------
(define (set-member s prop)
  (for/set ((x (in-set s)) #:when (prop x)) x))

(define set-such-that set-member)

(module+ test
  (check-equal? (set-member (set 1 2 3) (λ (x) (> x 1))) (set 3 2)))
  
;; ---------------------------------------------------------------------------------------------------
(define-syntax-rule (dev/null e0 e ...)
  (parameterize ([current-output-port (open-output-string)]
		 [current-error-port (open-output-string)])
    e0 e ...))

;; ---------------------------------------------------------------------------------------------------
(define (with-error-to-string proc)
  (call-with-output-string
   (lambda (p)
     (parameterize ([current-error-port p])
       (proc)))))

(module+ test
  (check-equal? (with-error-to-string (λ () (displayln "test" (current-error-port)))) "test\n"))

;; ---------------------------------------------------------------------------------------------------
(define (all-but-last l)
  (if (and (pair? l) (list? l))
      (let loop ([l l] [x (cdr l)])
        (if (pair? x)
            (cons (car l) (loop x (cdr x)))
            '()))
      (raise-argument-error 'last "(and/c list? (not/c empty?))" l)))

(define (split-off-last l)
  (define lst (gensym))
  (if (and (pair? l) (list? l))
      (values 
       (let loop ([l l] [x (cdr l)])
         (if (pair? x)
             (cons (car l) (loop x (cdr x)))
             (begin (set! lst (car l)) '())))
       lst)
      (raise-argument-error 'split-off-last "(and/c list? (not/c empty?))" l)))

(module+ test
  (check-equal? (all-but-last '(a b c)) '(a b))
  (check-equal? (let-values (([all-but last] (split-off-last '(a b c)))) `(,all-but ,last))
                '((a b) c)))

;; -----------------------------------------------------------------------------

(define (random-pick lox)
  (list-ref lox (random (length lox))))

;; ---------------------------------------------------------------------------------------------------
;; an iterator for going over the suffixes of a list in order (except for the empty one)
;; belongs into racket/collects/racket/private/for 

(provide in-suffixes)

(#%require '#%unsafe)

(begin-for-syntax
  (define lst-sym (string->uninterned-symbol "lst"))
  (define rest-sym (string->uninterned-symbol "rest")))

(define-syntax-rule (unless-unsafe e)
  (unless (variable-reference-from-unsafe? (#%variable-reference)) e))

(define (check-list l)
  (unless (list? l) (raise-argument-error 'in-list "list?" l)))

(define-sequence-syntax in-suffixes
  (lambda () #'in-list)
  (lambda (stx)
    (syntax-case stx (list)
      [[(id) (_ (list expr))] #'[(id) (:do-in ([(id) expr]) (void) () #t () #t #f ())]]
      [[(id) (_ lst-expr)]
       (with-syntax ([lst lst-sym]
                     [rest rest-sym])
         #'[(id)
            (:do-in
             ;;outer bindings
             ([(lst) lst-expr])
             ;; outer check
             (unless-unsafe (check-list lst))
             ;; loop bindings
             ([lst lst])
             ;; pos check
             (pair? lst)
             ;; inner bindings
             ([(id) lst]
              [(rest) (unsafe-cdr lst)]) ; so `lst` is not necessarily retained during body
             ;; pre guard
             #t
             ;; post guard
             #t
             ;; loop args
             (rest))])]
      [_ #f])))

;; ---------------------------------------------------------------------------------------------------
(define (all-equal? l)
  (cond
    [(empty? (rest l)) #true]
    [else (and (equal? (first l) (second l)) (all-equal? (rest l)))]))
