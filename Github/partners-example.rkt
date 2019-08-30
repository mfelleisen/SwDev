#lang racket

(provide (all-defined-out))
(require "human-resources.rkt")

(define SamCaldwell
  (person "Sam" "Caldwell" "samc@ccs.neu.edu" 1234 "samc"))
(define BenGreenman
  (person "Ben" "Greenman" "types@ccs.neu.edu" 5678 "types"))
(define t1
  (team SuzanneBecker ZachWalsh 3048))
(define all-teams
  (list t1))

(define using-javascript
  (list t1))

(define using-python3.6
  (list))

(define using-python3.7
  (list))

(define using-python3
  (list))

(define using-haskell
  (list))

(define using-java8
  (list))

(define using-java
  (list))

(define using-go
  (list))

(define using-rust
  (list))

(define using-c++
  (list))
