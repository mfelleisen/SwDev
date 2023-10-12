#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" -- ${1+"$@"}
|#
#lang racket/base 

(provide main) 

(require json)

(define (main [count 0])
  (define last (read-json)) ;; may produce EOF, keep going anyway
  (write-json (format "last ~a, count ~a" last count)) (newline) (flush-output)
  (main (add1 count)))

