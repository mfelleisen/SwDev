#lang racket

;; a matrix representation with cached transpose 

;; ---------------------------------------------------------------------------------------------------
;; exports

(define (same-length? M) (apply = (map length M)))
(define (row/c m) (flat-named-contract "row index" (and/c natural? (</c (matrix-#rows m)))))
(define (col/c m) (flat-named-contract "column index" (and/c natural? (</c (matrix-#columns m)))))

(provide
 matrix?
 direction?
 
 (contract-out
  [matrix           (->* () #:rest (and/c (listof (listof any/c)) cons? same-length?) matrix?)]
  [matrix-#rows     (-> matrix? natural?)]
  [matrix-#columns  (-> matrix? natural?)]
  [matrix-transpose (-> matrix? matrix?)]
  [matrix-ref       (->i ([m matrix?] [r (m) (row/c m)] [c (m) (col/c m)]) (x any/c))]
  [matrix-set       (->i ([m matrix?] [r (m) (row/c m)] [c (m) (col/c m)] [n any/c]) (y any/c))]

  [matrix-slide-row    (->i ([m matrix?] [d left-right/c] [r (m) (row/c m)] [x any/c]) (y matrix?))]
  [matrix-slide-column (->i ([m matrix?] [d up-down/c] [c (m) (col/c m)] [x any/c]) (y matrix?))]  

  [matrix-left      direction?]
  [matrix-right     direction?]
  [matrix-up        direction?]
  [matrix-down      direction?]))
  
;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require rackunit))

(define (rdc l)
  (if (not (pair? l))
      (raise-argument-error 'last "(and/c list? (not/c empty?))" l)
      (let loop ([l l] [x (cdr l)])
        (if (pair? x)
            (cons (car l) (loop x (cdr x)))
            '()))))

;; ---------------------------------------------------------------------------------------------------
;; data definition 

(struct inner [rectangle {transpose #:mutable} row# col#] #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc self other rec) (rec (inner-rectangle self) (inner-rectangle other)))
   (define (hash-proc self rec) (+ (eq-hash-code struct:inner) (rec (inner-rectangle self))))
   (define (hash2-proc self rec) (+ (eq-hash-code struct:inner) (rec (inner-rectangle self))))])

#; {type [Matrix X]    = [Inner x]}
#; {type [Inner X]     = [inner [Rectangle X] (U False [Rectangle X]) N N]}
#; {type [Rectangle X] = [NEListof [Listof X]] || the inner lists are all of the same length}

;; ---------------------------------------------------------------------------------------------------
;; basic constructors, predicates, selectors, setter 

(define matrix? inner?)

(define (matrix . t-rows)
  (define x (apply (λ x x) t-rows))
  (inner x #f (length x) (length (first x))))

(define matrix-#rows inner-row#)

(define matrix-#columns inner-col#)

(define (matrix-ref M r c)
  (list-ref (list-ref (inner-rectangle M) r) c))

(define (matrix-set M r c x0)
  (define R (inner-rectangle M))
  (define x (list-set (list-ref R r) c x0))
  (inner (rectangle-replace-row R r x identity) #false (inner-row# M) (inner-col# M)))

;; ---------------------------------------------------------------------------------------------------
;; transpose 

#; {∀ X: [Matrix X] -> [Matrix X]}
(define (matrix-transpose m)
  (or (and (inner-transpose m) (flip m))
      (let* ([R (inner-rectangle m)]
             [T (apply map list R)])
        (set-inner-transpose! m T)
        (flip m))))

#; {∀ X: [Inner X] -> [Inner X]}
(define (flip i)
  (match-define [inner M M^t r c] i)
  (inner M^t M c r))

;; ---------------------------------------------------------------------------------------------------
;; slide rows and columns 

#; {∀ X: [Matrix X] Direction Natural X -> [Matrix X]}
#; {type RPose = [Rectangle X] -> [Rectangle X]}

(struct direction [slide add])
#; {type Direction = [direction [(Listof X) -> X] [X [Listof X] -> [Listof X]]]}

(define matrix-left  [direction rest (λ (nu row) (reverse (cons nu (reverse row))))])
(define matrix-right [direction rdc cons])
(define matrix-up    [direction rest (λ (nu row) (reverse (cons nu (reverse row))))])
(define matrix-down  [direction rdc cons])

(define left-right/c
  (flat-named-contract "left-right" (or/c (curry eq? matrix-left) (curry eq? matrix-right))))

(define up-down/c
  (flat-named-contract "up-down" (or/c (curry eq? matrix-down) (curry eq? matrix-up))))

(define (matrix-slide-column M d c nu)
  (inner (slide (inner-rectangle M) d c nu rectangle-transpose) #false (inner-row# M) (inner-col# M)))

(define (matrix-slide-row M d r nu)
  (inner (slide (inner-rectangle M) d r nu identity) #false (inner-row# M) (inner-col# M)))

;; ---------------------------------------------------------------------------------------------------
;; rectangle auxiliaries

#; {∀ X: [Rectangle X] Direction Natural X RPose -> [Rectangle X]}
(define (slide R0 direction r nu opt-transpose)
  (let* ([R (opt-transpose R0)]
         [S ([direction-add direction] nu [(direction-slide direction) (list-ref R r)])])
    (rectangle-replace-row R r S opt-transpose)))

#; {∀ X: [Rectangle X] Natural X Natural Natural Rpose-> [Rectangle X]}
(define (rectangle-replace-row R r nu opt-transpose)
  (opt-transpose (append (take R (max 0 r)) (list nu) (drop R (+ r 1)))))

#; {∀ X: [Rectangle X] -> [Rectangle X]}
(define (rectangle-transpose R)
  (apply map list R))

;; ---------------------------------------------------------------------------------------------------
;; positive tests 

(module+ test
  (define M1
    (matrix
     '[A C E]
     '[B D F]))

  (define M1-set-1-1-X
    (matrix
     '[A C E]
     '[B X F]))

  (define M1-slide-row1-left-X
    (matrix
     '[A C E]
     '[D F X]))
  
  (define M1-slide-column2-up-X
    (matrix
     '[A C F]
     '[B D X]))
    
  (define M1-transposed
    (matrix
     '[A B]
     '[C D]
     '[E F]))
  
  (check-equal? (matrix-#rows M1) 2  "#rows")
  (check-equal? (matrix-#columns M1) 3 "#columns")

  (check-true (hash? (hash M1 0)) "hash code")

  (check-equal? (matrix-ref M1 0 1) 'C "ref")
  (check-equal? (matrix-set M1 1 1 'X) M1-set-1-1-X "set")

  (check-equal? (matrix-slide-row M1 matrix-left 1 'X) M1-slide-row1-left-X "slide 1 left X")
  (check-equal? (matrix-slide-row M1-slide-row1-left-X matrix-right 1 'B) M1 "slide 1 right back")
  (check-equal? (matrix-slide-column M1 matrix-up 2 'X) M1-slide-column2-up-X "slide 2 up X")
  (check-equal? (matrix-slide-column M1-slide-column2-up-X matrix-down 2 'E) M1 "slide 2 down back")
  
  (check-equal? (matrix-transpose M1) M1-transposed "transpose basic")
  (check-equal? (matrix-transpose (matrix-transpose M1)) M1 "transpose o transpose"))

;; ---------------------------------------------------------------------------------------------------
;; negative tests

(module+ test

  (check-exn #px"expected: pair?" matrix)
  (check-exn #px"expected: list?" (λ () (matrix (cons 1 2))))
  (check-exn #px"same-length" (λ () (matrix '[A B] '[C])))

  (check-exn #px"row index" (λ () (matrix-ref M1 3 9)))
  (check-exn #px"column index" (λ () (matrix-ref M1 0 9)))

  (check-exn #px"left-right" (λ () (matrix-slide-row M1 matrix-down 0 'x)))
  (check-exn #px"row index" (λ () (matrix-slide-row M1 matrix-left 9 'x)))

  (check-exn #px"up-down" (λ () (matrix-slide-column M1 matrix-left 0 'x)))
  (check-exn #px"column index" (λ () (matrix-slide-column M1 matrix-up 9 'x))))
  