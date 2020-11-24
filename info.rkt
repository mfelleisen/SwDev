#lang info
(define collection "SwDev")

(define scribblings '(("scribblings/SwDev.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(matthias))

(define compile-omit-paths
  '("Ideas"))

(define deps
  '("base"
     "gui-lib"
     "scribble-lib"
     "pict-lib"
     "rackunit-lib"))

(define build-deps
  '("gui-lib"
     "pict-lib"
     "at-exp-lib"
     "rackunit-lib"
     "scribble-lib"
     "racket-doc"
     "rackunit-lib"))
