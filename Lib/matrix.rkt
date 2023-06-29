#lang racket

;; a matrix representation with cached transpose 

;; ---------------------------------------------------------------------------------------------------
;; exports

(define (square? l) (and/c cons? (natural? (sqrt (length l)))))
(define (same-length? M) (apply = (map length M)))
(define rectangle? (and/c (listof (listof any/c)) cons? same-length?))
(define (row/c m) (flat-named-contract "row index" (and/c natural? (</c (matrix-#rows m)))))
(define (col/c m) (flat-named-contract "column index" (and/c natural? (</c (matrix-#columns m)))))

(define [(divisible-by n) l] (= (remainder (length l) n) 0))

(provide
 matrix?
 direction?
 matrix-undo
 
 (contract-out
  [group-by-length (->i ([l (n) (and/c list? (divisible-by n))] [n natural?]) [r (listof list?)])])
 
 (contract-out
  [matrix           (->* () #:rest rectangle? matrix?)]
  [make-matrix      (-> rectangle? matrix?)]
  [matrix-from-list (-> (and/c (listof any/c) square?) matrix?)]
  [matrix-copy      (-> matrix? matrix?)]
  
  [matrix-#rows     (-> matrix? natural?)]
  [matrix-#columns  (-> matrix? natural?)]
  [matrix-diagonal  (-> matrix? list?)]
  [matrix-rows      (-> matrix? list?)]
  [matrix-columns   (-> matrix? list?)]

  [matrix-transpose (-> matrix? matrix?)]
  [matrix-ref       (->i ([m matrix?] [r (m) (row/c m)] [c (m) (col/c m)]) (x any/c))]
  [matrix-ref*      (->i ([m matrix?] [r integer?] [c integer?]) (#:default (d any/c)) (y any/c))]
  [matrix-set       (->i ([m matrix?] [r (m) (row/c m)] [c (m) (col/c m)] [n any/c]) (y any/c))]

  [matrix-rectangle (-> matrix? (listof (listof any/c)))]
  
  [matrix-pad
   (->i ([m matrix?] [lox [listof any/c]] #:nuwidth (nuw natural?) #:nuheight (nuh natural?))
        #:pre/name (m nuw) "new width >= to old width expected" (>= nuw (matrix-#columns m))
        #:pre/name (m nuh) "new height >= to old height expected" (>= nuh (matrix-#rows m))
        #:pre/name (m nuh nuw lox) "sufficient number of extras expected"
        (>= (length lox) (* (- (matrix-#columns m) nuw) (- (matrix-#rows m) nuh)))
        (r matrix?)
        #:post/name (nuw r) "expected width" (= (matrix-#columns r) nuw)
        #:post/name (nuh r) "expected height" (= (matrix-#rows r) nuh))]

  [matrix-frame
   (->i ([m matrix?] [lox [listof any/c]]) (r matrix?))]
        

  [matrix-slide-row    (->i ([m matrix?] [d left-right/c] [r (m) (row/c m)] [x any/c]) (y matrix+))]
  [matrix-slide-column (->i ([m matrix?] [d up-down/c] [c (m) (col/c m)] [x any/c]) (y matrix+))]  

  [matrix-left      direction?]
  [matrix-right     direction?]
  [matrix-up        direction?]
  [matrix-down      direction?]))

;; occasionally needed for extracted rectangle 
(provide
 rectangle?
 rectangle-#rows
 rectangle-#columns)
  
;; ---------------------------------------------------------------------------------------------------
(require "list-private.rkt")

(module+ test
  (require (submod ".."))
  (require rackunit))

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
(define matrix+ (list/c matrix? any/c))

;; ===================================================================================================
(define (group-by-length l n)
  (let loop ([l l])
    (cond
      [(empty? l) '()]
      [else (cons (take l n) (loop (drop l n)))])))
;; ===================================================================================================

(define (matrix-copy m) m)

(define (make-matrix t-rows)
  (apply matrix t-rows))

;; make a square matrix from a list of objects 
(module+ test
  (check-equal? (matrix-from-list '[1 2 3 4 5 6 7 8 9]) (matrix '[1 2 3] '[4 5 6] '[7 8 9])))
(define (matrix-from-list l)
  (define n (sqrt (length l)))
  (define r 
    (let loop ([l l])
      (cond
        [(empty? l) '()]
        [else (cons (take l n) (loop (drop l n)))])))
  (apply matrix r))

(define (matrix . t-rows)
  (define x (apply (λ x x) t-rows))
  (inner x #f (rectangle-#rows x) (rectangle-#columns x)))

(define matrix-rectangle inner-rectangle)

(define matrix-#rows inner-row#)

(define matrix-#columns inner-col#)

;; for iterating over matrix elements 
(module+ test
  (check-equal? (matrix-diagonal (matrix '[1 2 3] '[4 5 6] '[7 8 9])) '[1 5 9]))
(define (matrix-diagonal m)
  (for/list ([i (matrix-#rows m)]) (matrix-ref m i i)))

(define (matrix-rows m) (matrix-rectangle m))

(define (matrix-columns m) (matrix-rectangle (matrix-transpose m)))

#; {[Matrix X] N N [#:default D] -> (U X D)}
(define (matrix-ref* M row col #:default (default 0))
    (if (and (<= 0 row (- (matrix-#columns M) 1))
             (<= 0 col (- (matrix-#rows M) 1)))
        (matrix-ref M row col) 
        default))

(define (matrix-ref M r c)
  (list-ref (list-ref (inner-rectangle M) r) c))

(define (matrix-set M r c x0)
  (define R (inner-rectangle M))
  (define x (list-set (list-ref R r) c x0))
  (inner (rectangle-replace-row R r x identity) #false (inner-row# M) (inner-col# M)))

(define (matrix-undo M) M)

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

(struct direction [slide out add])
#; {type Direction = [direction [(Listof X) -> X] [X [Listof X] -> [Listof X]]]}

(define matrix-left  [direction rest first (λ (nu row) (reverse (cons nu (reverse row))))])
(define matrix-right [direction rdc last cons])
(define matrix-up    [direction rest first (λ (nu row) (reverse (cons nu (reverse row))))])
(define matrix-down  [direction rdc last cons])

(define left-right/c
  (flat-named-contract "left-right" (or/c (curry eq? matrix-left) (curry eq? matrix-right))))

(define up-down/c
  (flat-named-contract "up-down" (or/c (curry eq? matrix-down) (curry eq? matrix-up))))

(define (matrix-slide-column M d c nu)
  (match-define [list M*t out] (matrix-slide-row (matrix-transpose M) d c nu))
  (list (matrix-transpose M*t) out))

(define (matrix-slide-row M d r nu)
  (match-define [list R out] (slide (inner-rectangle M) d r nu identity))
  (list (inner R #false (inner-row# M) (inner-col# M)) out))

;; ---------------------------------------------------------------------------------------------------
;; padding a matrix 
(define (matrix-pad M extras #:nuwidth (nuw 7) #:nuheight (nuh 7))
  (define R (matrix-rectangle M))
  (define P (rectangle-col-fill R extras nuw nuh))
  (make-matrix P))

(define (matrix-frame M extras)
  (define R (matrix-rectangle M))
  (define r (matrix-#rows M))
  (define c (matrix-#columns M))
  (let* ([P R]
         [X (drop extras (+ c 2))]
         [P (rectangle-col-fill P X (+ r 1) (+ c 1))]
         [P (rectangle-col-fill P X (+ r 2) (+ c 1) #:append (λ (row fill) (append fill row)))]
         [P (cons (take extras (+ c 2)) P)])
    (make-matrix P)))

;; ---------------------------------------------------------------------------------------------------
;; padding a rectangle 
#; {∀ X: [Rectable X] [Listof X] N N -> [Rectangle X]}
(define (rectangle-col-fill R extras nuw nuh #:append (append append))
  (let rectangle-col-fill ([R R] [extras extras] [result '()])
    (cond
      [(empty? R) (rectangle-row-fill (reverse result) extras nuw nuh)]
      [else
       (define row (first R))
       (define nnn (- nuw (length row)))
       (define pad (append row (take extras nnn)))
       (rectangle-col-fill (rest R) (drop extras nnn) (cons pad result))])))

#; {∀ X: [Rectable X] [Listof X] N N -> [Rectangle X]}
(define (rectangle-row-fill R extras nuw nuh)
  (define height (length R))
  (let rectangle-row-fill ([todo (- nuh height)] [extras extras] [result (reverse R)])
    (cond
      [(or (zero? todo) (empty? extras)) (reverse result)]
      [else
       (define next-row (take extras nuw))
       (rectangle-row-fill (sub1 todo) (drop extras nuw) (cons next-row result))])))

;; ---------------------------------------------------------------------------------------------------
;; rectangle auxiliaries

#; {∀ X: [Rectangle X] -> Natural}
(define (rectangle-#rows x) (length x))

(define (rectangle-#columns x) (length (first x)))

#; {∀ X: [Rectangle X] Direction Natural X RPose -> [Rectangle X]}
(define (slide R0 direction r nu opt-transpose)
  (let* ([R (opt-transpose R0)]
         [row (list-ref R r)]
         [S ([direction-add direction] nu [(direction-slide direction) row])])
    (list (rectangle-replace-row R r S opt-transpose) [(direction-out direction) row])))

#; {∀ X: [Rectangle X] Natural X Natural Natural Rpose-> [Rectangle X]}
(define (rectangle-replace-row R r nu opt-transpose)
  (opt-transpose (append (take R (max 0 r)) (list nu) (drop R (+ r 1)))))

#; {∀ X: [Rectangle X] -> [Rectangle X]}
(define (rectangle-transpose R)
  (apply map list R))

;; ---------------------------------------------------------------------------------------------------
;; positive tests 

(module+ test
  (define M1-at-3-x-4
    (matrix
     '[A C E a]
     '[B D F b]
     '[c d e f]))

  (define M1
    (matrix
     '[A C E]
     '[B D F]))

  (define M1-make
    (make-matrix
     '[[A C E]
       [B D F]]))

  (define M1-set-1-1-X
    (matrix
     '[A C E]
     '[B X F]))

  (define M1-slide-row1-left-X
    (list
     (matrix
      '[A C E]
      '[D F X])
     'B))
  
  (define M1-slide-column2-up-X
    (list
     (matrix
      '[A C F]
      '[B D X])
     'E))
    
  (define M1-transposed
    (matrix
     '[A B]
     '[C D]
     '[E F]))

  (check-equal? M1 M1-make "make")
  
  (check-equal? (matrix-#rows M1) 2  "#rows")
  (check-equal? (matrix-#columns M1) 3 "#columns")

  (check-true (hash? (hash M1 0)) "hash code")

  (check-equal? (matrix-ref M1 0 1) 'C "ref")
  (check-equal? (matrix-set M1 1 1 'X) M1-set-1-1-X "set")

  (check-equal? (matrix-slide-row M1 matrix-left 1 'X) M1-slide-row1-left-X "slide 1 left X")
  (check-equal? (matrix-slide-row (first M1-slide-row1-left-X) matrix-right 1 'B) [list M1 'X] "1b")
  (check-equal? (matrix-slide-column M1 matrix-up 2 'X) M1-slide-column2-up-X "slide 2 up X")
  (check-equal? (matrix-slide-column (first M1-slide-column2-up-X) matrix-down 2 'E) [list M1 'X] "2")
  
  (check-equal? (matrix-transpose M1) M1-transposed "transpose basic")
  (check-equal? (matrix-transpose (matrix-transpose M1)) M1 "transpose o transpose")

  (check-equal? (matrix-pad M1 '[a b c d e f] #:nuwidth 4 #:nuheight 3) M1-at-3-x-4 "padding")

  (module+ test
    (check-equal? 
     (matrix-frame
      (matrix
       '[A B]
       '[C D])
      '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
     (matrix
      '[0 0 0 0]
      '[0 A B 0]
      '[0 C D 0]
      '[0 0 0 0]) "matrix-frame"))) 

(module+ test
  (define R1 (make-matrix (build-list 11 (λ (row) (build-list 11 (λ (col) [list row col]))))))

  (time (for ([i 100090]) (matrix-slide-column R1 matrix-down 1 'anyany) (matrix-undo R1) (void))))

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
