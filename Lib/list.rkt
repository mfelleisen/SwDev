#lang racket

(provide

 #; {[Listof X] Y -> (Cons X ... Y)}
 snoc
 
 #; {[Cons X ... Y] -> [Listof X]}
 rdc

 #; {[Cons X [Listof X]] -> (values [Listof X] X)}
 split-off-last)

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (rdc '(a b c)) '(a b))
  (check-equal? (let-values (([all-but last] (split-off-last '(a b c)))) `(,all-but ,last))
                '((a b) c)))

;; ===================================================================================================

(provide
 matrix

 matrix-#rows
 matrix-#columns

 matrix-transpose)

(define (matrix x) x)

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