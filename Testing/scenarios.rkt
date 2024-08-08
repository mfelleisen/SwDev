#lang racket

(provide
 #; SYNTAX 
 #;(setup-scenarios scenario+ name:id ...)
 ;; sets up tests scenarios `name` ... and a macro for adding a test scenario 
 #; (scenario+ name arguments-as-a-list expected-value msg:str)
 ;; to one of them 
 setup-scenarios)

(module+ test
  (require rackunit))

(module server racket
  (provide setup-scenarios)
  (require (for-syntax syntax/parse))
  (define-syntax (setup-scenarios stx)
    (syntax-parse stx 
      [(_ scenario+ name:id ...)
       #:with (n ...) (map syntax-e (syntax->list #'(name ...)))
       #'(begin
           (define-for-syntax all-names (list 'n ...))
           (define-syntax (scenario+ stx)
             (syntax-parse stx 
               [(_ kind:id args expected msg:str)
                #:fail-unless (member (syntax-e #'kind) all-names) "name not defined"
                #'(set! kind (append kind (list [list args expected msg])))]))
           (define name '[])
           ...)])))
(require 'server)

(module client racket
  (provide Tests/)
  (require (submod ".." server))
  (setup-scenarios scenario+ Tests/)
  (scenario+ Tests/ (list 1 2) (list 'a 'b) "testing") )

(require 'client)

(module+ test
  (check-equal? Tests/ {list [list (list 1 2) (list 'a 'b) "testing"]}))