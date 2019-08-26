#lang racket

(provide
 #;{Any Any -> (U False [List Any Any])}
 ;; if there is a 
 diff)

(require "spy.rkt")

(module+ test
  (require rackunit))

(define (diff tree1 tree2)

  (define (package x y Pth)
    (list x y (reverse Pth)))
  
  (define (simple string=? x y path)
    (if (string=? x y) #f (package x y path)))

  (define (complex vector-length vector->list x y path)
    (or (if (= (vector-length x) (vector-length y)) #f (package x y path))
        (for/or ((a (vector->list x)) (b (vector->list y)) (i (in-naturals)))
          (diff a b (cons i path)))))

  (define (diff [tree1 tree1][tree2 tree2][path '()])
    (match* (tree1 tree2)
      [('() '()) #f]
      [((? number? x) (? number? y))   (simple = x y path)]
      [((? symbol? x) (? symbol? y))   (simple eq? x y path)]
      [((? string? x) (? string? y))   (simple string=? x y path)]
      [((? char? x)   (? char? y))     (simple eq? x y path)]
      [((? boolean? x) (? boolean? y)) (simple eq? x y path)]
      [((? vector? x) (? vector? y))   (complex vector-length vector->list x y (cons 'vector path))]
      [((? list? x)   (? list? y))     (complex length values x y (cons 'list path))]
      [((? struct? x) (? struct? y))
       (define xv (struct->vector x))
       (define yv (struct->vector y))
       (define x-tag (vector-ref xv 0))
       (define y-tag (vector-ref yv 0))
       (if (eq? x-tag y-tag) (diff xv yv (cons x-tag path)) (package x y path))]
      ;; let struct equality kick in first 
      [((? procedure? x)   (? procedure? y)) (simple eq? x y path)]
      [(_   _) (list tree1 tree2)]))

  (diff))

(module+ test

  (check-false  (diff 1 1))
  (check-equal? (diff 1 2) (list 1 2 '()))

  (struct in  () #:transparent)
  (check-false (diff (in) (in)))

  (struct out ())
  (define ss (list (out) (out)))
  (check-equal? (apply diff ss) ss))