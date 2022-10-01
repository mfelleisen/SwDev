#lang racket

;; provide some basic TCP constants and a utility for rotating thru ports 

(provide 
 LOCALHOST
 REMOTE-PORT
 ACCEPT-TIMEOUT
 RETRY-COUNT

 (contract-out
  ;; consult a PORT-STARTER-FILE and get the next one 
  (get-starter-port (-> port-number?))))

; ---------------------------------------------------------------------------------------------------
(define LOCALHOST "127.0.0.1")
(define REMOTE-PORT 45678)
(define ACCEPT-TIMEOUT 5) ;; seconds. See (server).
(define RETRY-COUNT 10) ;; with a retry every .5 sec. See (client).

(define BASE 12345)
(define PORT-STARTER-FILE "port-starter-file.rktd")
(define (get-starter-port)
  (define p 
    (cond
      [(file-exists? PORT-STARTER-FILE) (with-input-from-file PORT-STARTER-FILE read)]
      [else BASE]))
  (with-output-to-file PORT-STARTER-FILE (λ () (writeln (add1 p))) #:exists 'replace)
  p)