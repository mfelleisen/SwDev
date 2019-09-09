#lang racket

(provide
 #; {IP-Address PortNumber [Natural] -> (values [JSexpr -> JSexpr] Custodian)}
 #; (connect-to-server ip p [n])
 ;; attempts to connect to IP address ip at port p n times;
 ;; success: returns a call-service that conducts "remote calls" and a custodian for the connection
 ;; failure: exn:fail:network 
 connect-to-server)

;; ---------------------------------------------------------------------------------------------------
(require SwDev/Testing/communication)

;; ---------------------------------------------------------------------------------------------------
(define TCP-TRIES 10)

(define (connect-to-server server port (tries TCP-TRIES))
  (define custodian (make-custodian))
  (define-values (in out)
    (parameterize ((current-custodian custodian))
      (let tcp ([n TCP-TRIES])
        (with-handlers ([exn:fail:network? (λ (xn)
                                             (sleep 1)
                                             (if (<= n 0) (raise xn) (tcp (- n 1))))])
          (tcp-connect server port)))))
  (define (call-server j)
    (parameterize ((current-output-port out) (current-input-port in))
      (send-message j)
      (flush-output)
      (newline)
      (read-message)))
  (values call-server custodian))