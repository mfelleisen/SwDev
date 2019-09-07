#lang racket

(provide

 #; {[Setof X] [X -> X] -> (U False [Setof X])}
 set-member
 set-such-that

 =>
 when*
 unless*
 and*

 #; {-> Void} 
 with-error-to-string)

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
(define (with-error-to-string proc)
  (call-with-output-string
    (lambda (p)
      (parameterize ([current-error-port p])
	(proc)))))
