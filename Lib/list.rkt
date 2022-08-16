#lang racket

(provide

 #; {[Listof X] Y -> (Cons X ... Y)}
 snoc
 
 #; {[Cons X ... Y] -> [Listof X]}
 rdc

 #; {[NEListof X] -> [NEListof X]} 
 list-rotate 

 #; {[Cons X [Listof X]] -> (values [Listof X] X)}
 split-off-last

 #; {[Listof X] N -> [Listof [Listof X]]}
 split-list)

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
#; {[NEListof X] -> [NEListof X]}
(define (list-rotate lox)
  (snoc (rest lox) (first lox)))
  
(define (snoc l x) (append l (list x)))

(define (rdc l)
  (if (not (pair? l))
      (raise-argument-error 'last "(and/c list? (not/c empty?))" l)
      (let loop ([l l] [x (cdr l)])
        (if (pair? x)
            (cons (car l) (loop x (cdr x)))
            '()))))

(define (split-off-last l)
  (define lst (gensym))
  (if (not (pair? l))
      (raise-argument-error 'split-off-last "(and/c list? (not/c empty?))" l)
      (values 
       (let loop ([l l] [x (cdr l)])
         (if (pair? x)
             (cons (car l) (loop x (cdr x)))
             (begin (set! lst (car l)) '())))
       lst)))

#; {[Listof X] N -> [Listof [Listof X]]}
(define (split-list tiles0 rows#)
  (let L ([t* tiles0])
    (cond
      [(empty? t*) '()]
      [else
       (define row1 (take t* rows#))
       (cons row1 (L (drop t* rows#)))])))


;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (rdc '(a b c)) '(a b))
  (check-equal? (let-values (([all-but last] (split-off-last '(a b c)))) `(,all-but ,last))
                '((a b) c)))

;; ===================================================================================================

(provide

 matrix?

 #; {[Listof X] *-> [Matrix X] || all lists in the argument are the same length}
 (contract-out
  [matrix (->* [] #:rest (and/c (listof any/c) (λ (x) (apply = (map length x)))) matrix?)])

 #; {[Matrix X] N N -> X}
 #; (matrix-ref m row col)
 matrix-ref

 matrix-#rows
 matrix-#columns

 matrix-transpose)

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod "..")))

(define (matrix? x)
  (and (cons? x)
       (list? x)
       (cons? (first x))
       (andmap list? x) 
       (apply = (map length x))))

(module+ test
  (check-equal? (matrix '[1 2 3] '[a b c]) '[[1 2 3] [a b c]]))

(define (matrix-ref rb row column)
  (list-ref (list-ref rb row) column))

(define (matrix . x) x)

(define (build-matrix f rows# columns#) 0)

(define (matrix-#rows M) (length M))

(define (matrix-#columns M) (length (first M)))

#; {RawBoard -> RawBoard}
(module+ test
  (define silly-board-2
    '([A C E]
      [B D F]))

  (define silly-board-2-transposed
    `([A B]
      [C D]
      [E F]))
  (check-equal? (matrix-transpose silly-board-2) silly-board-2-transposed)
  (check-equal? (matrix-transpose (matrix-transpose silly-board-2)) silly-board-2))
(define (matrix-transpose rb) (apply map list rb))
