#lang racket

(provide 
 LOCALHOST
 REMOTE-PORT
 ACCEPT-TIMEOUT
 
 unset-time-out
 TIMEOUT)

(define LOCALHOST "127.0.0.1")
(define REMOTE-PORT 45678)
(define ACCEPT-TIMEOUT 5) ;; seconds. See (server).
(define TIMEOUT 5) ;; seconds. See read-json-safely/timeout.

(define (unset-time-out)
  (set! TIMEOUT 1000000000))
