#lang racket

(provide 
 LOCALHOST
 REMOTE-PORT
 ACCEPT-TIMEOUT
 )

(define LOCALHOST "127.0.0.1")
(define REMOTE-PORT 45678)
(define ACCEPT-TIMEOUT 5) ;; seconds. See (server).
