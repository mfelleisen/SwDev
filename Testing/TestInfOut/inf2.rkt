#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" -- ${1+"$@"}
|#
#lang racket/base 

(provide main) 

(require json)

(define (main [count 0])
  (read-json) ;; may produce EOF, keep going anyway
  (write-json count) (newline) (flush-output)
  (main (add1 count)))

