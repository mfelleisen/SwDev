#lang info
(define collection "cs4500-f18-fest")
(define deps '("base" "rackunit-lib" "lang-file" "glob" "sandbox-lib" "gregor-lib"))
(define build-deps '("scribble-lib" "racket-doc" "gregor-doc"))
(define pkg-desc "CS4500 testfest harness")
(define version "0.1")
(define pkg-authors '(ben samc))
(define scribblings '(("scribblings/cs4500-f18-fest.scrbl" () (tool-library))))
(define raco-commands '(("cs4500-f18-fest" (submod cs4500-f18-fest main) "Run a test fest based on a manifest (or config file)" #f)))
