#lang racket

(define (same-length? M) (apply = (map length M)))
(define rectangle? (and/c (listof (listof any/c)) cons? same-length?))
(define (row/c m) (flat-named-contract "row index" (and/c natural? (</c (matrix-#rows m)))))
(define (col/c m) (flat-named-contract "column index" (and/c natural? (</c (matrix-#columns m)))))
(define matrix+ (list/c matrix? any/c))


(provide
 matrix?
 direction?
 
 (contract-out
  [matrix           (->* () #:rest rectangle? matrix?)]
  [make-matrix      (-> rectangle? matrix?)]
  
  [matrix-#rows     (-> matrix? natural?)]
  [matrix-#columns  (-> matrix? natural?)]
  [matrix-transpose (-> matrix? matrix?)]
  [matrix-ref       (->i ([m matrix?] [r (m) (row/c m)] [c (m) (col/c m)]) (x any/c))]
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

  [matrix-slide-row    (->i ([m matrix?] [d left-right/c] [r (m) (row/c m)] [x any/c]) (y matrix+))]
  [matrix-slide-column (->i ([m matrix?] [d up-down/c] [c (m) (col/c m)] [x any/c]) (y matrix+))]  

  [matrix-left      direction?]
  [matrix-right     direction?]
  [matrix-up        direction?]
  [matrix-down      direction?]))

;                                                                          
;                                                                          
;        ;                                       ;            ;            
;        ;            ;                          ;            ;            
;        ;            ;                          ;            ;            
;    ;;; ;    ;;;   ;;;;;;    ;;;            ;;; ;   ;;;;   ;;;;;          
;   ;;  ;;   ;   ;    ;      ;   ;          ;;  ;;  ;    ;    ;            
;   ;    ;       ;    ;          ;          ;    ;  ;;;;;;    ;            
;   ;    ;   ;;;;;    ;      ;;;;;          ;    ;  ;         ;            
;   ;    ;  ;    ;    ;     ;    ;          ;    ;  ;         ;       ;;   
;   ;;  ;;  ;   ;;    ;     ;   ;;          ;;  ;;  ;;   ;    ;       ;;   
;    ;;; ;   ;;; ;     ;;;   ;;; ;           ;;; ;   ;;;;;    ;       ;;   
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(struct inner [rectangle {transpose #:mutable} row# col#] #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc self other rec) (rec (inner-rectangle self) (inner-rectangle other)))
   (define (hash-proc self rec) (+ (eq-hash-code struct:inner) (rec (inner-rectangle self))))
   (define (hash2-proc self rec) (+ (eq-hash-code struct:inner) (rec (inner-rectangle self))))])

(define matrix? inner?)

[define matrix 0]
[define make-matrix 1]
  
[define matrix-#rows 2]
[define matrix-#columns 3]

;                                                                                  
;                                                                                  
;        ;     ;                                       ;                           
;        ;                                    ;                                    
;        ;                                    ;                                    
;    ;;; ;   ;;;     ;;;;    ;;;;     ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;    ;;;;  
;   ;;  ;;     ;     ;;  ;  ;    ;   ;   ;    ;        ;    ;;  ;;  ;;   ;  ;    ; 
;   ;    ;     ;     ;      ;;;;;;  ;         ;        ;    ;    ;  ;    ;  ;      
;   ;    ;     ;     ;      ;       ;         ;        ;    ;    ;  ;    ;   ;;;;  
;   ;    ;     ;     ;      ;       ;         ;        ;    ;    ;  ;    ;       ; 
;   ;;  ;;     ;     ;      ;;   ;   ;   ;    ;        ;    ;;  ;;  ;    ;  ;    ; 
;    ;;; ;   ;;;;;   ;       ;;;;;    ;;;      ;;;   ;;;;;   ;;;;   ;    ;   ;;;;  
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  

(struct direction [slide out add])
#; {type Direction = [direction [(Listof X) -> X] [X [Listof X] -> [Listof X]]]}

(define left-right/c
  (flat-named-contract "left-right" (or/c (curry eq? matrix-left) (curry eq? matrix-right))))

(define up-down/c
  (flat-named-contract "up-down" (or/c (curry eq? matrix-down) (curry eq? matrix-up))))

;                          
;                          
;                          
;     ;;;                  
;    ;   ;                 
;   ;     ; ; ;;;    ;;;;  
;   ;     ; ;;  ;;  ;    ; 
;   ;     ; ;    ;  ;      
;   ;     ; ;    ;   ;;;;  
;   ;     ; ;    ;       ; 
;    ;   ;  ;;  ;;  ;    ; 
;     ;;;   ; ;;;    ;;;;  
;           ;              
;           ;              
;           ;              
;                          

[define matrix-transpose 4]
[define matrix-ref 5]
[define matrix-set 6]

[define matrix-rectangle 7]
  
[define matrix-pad 8]

[define matrix-slide-row 9]
[define matrix-slide-column 10]  

[define matrix-left 11]
[define matrix-right 12]
[define matrix-up 13]
[define matrix-down 14]