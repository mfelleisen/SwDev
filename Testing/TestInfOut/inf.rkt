#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" -- ${1+"$@"}
|#
#lang racket/base 

(provide main) 

(require json)

(define (main [count 0] [read-more #true])
  (define next (and read-more (read-json)))
  (write-json count) (newline) (flush-output)
  (main (add1 count) (and read-more (not (eof-object? next)))))

