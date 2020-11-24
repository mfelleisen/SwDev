#lang racket

(require (only-in json jsexpr?))

(define port/c (and/c natural-number/c (</c 60000) (>/c 10000)))
(define tries/c natural-number/c)

(provide
 port/c

 #; {IP-Address PortNumber [Natural] [-> Void] -> (values [JSexpr -> JSexpr] Custodian)}
 #; (connect-to-server ip p [n] [#:init init])
 ;; attempts to connect to IP address ip at port p n times;
 ;; returns a CALL-SERVICE that conducts "remote calls" and a custodian for the connection
 ;; failure: exn:fail:network 
 connect-to-server-as-caller
 ;; for backwards compatibility 
 (rename-out [connect-to-server-as-caller connect-to-server])


 #; {IP-Address PortNumber [Natural] -> (values [[JSexpr -> JSexpr] -> JSexpr] Custodian)}
 ;; attempts to connect to IP address ip at port p n times;
 ;; returns a receive-service that turns some given function into the receiver for remote calls
 ;; when connected, it runs (init ip)
 ;; failure: exn:fail:network
 (contract-out
  [connect-to-server-as-receiver
   (->* (string? port/c) (tries/c #:init (-> output-port? any))
        (values (-> (-> (or/c eof-object? jsexpr?) jsexpr?) any)
                custodian?))]))

;; ---------------------------------------------------------------------------------------------------
(require SwDev/Testing/communication)

;; ---------------------------------------------------------------------------------------------------
(define TCP-TRIES 10)

(define (connect-to-server-as-caller server port (tries TCP-TRIES))
  (define-values (custodian in out) (connect server port tries))
  #; {JSexpr -> JSexpr}
  (define (call-server j)
    (send-message j out)
    (read-message in))
  (values call-server custodian))

(define (connect-to-server-as-receiver server port (tries TCP-TRIES) #:init (init void))
  (define-values (custodian in out) (connect server port tries))
  (init out)
  #; {[JSexpr -> JSexpr] -> Void}
  (define (receive-from-server f)
    (define input (read-message in))
    (define result (f input))
    (send-message result out))
  (values receive-from-server custodian))

#; {IP-Address Port N -> (values Custodian Input-Port Output-Port)}
(define (connect server port tries)
  (define custodian (make-custodian))
  (define-values (in out)
    (parameterize ((current-custodian custodian))
      (let tcp ([n TCP-TRIES])
        (with-handlers ([exn:fail:network? (λ (xn) (sleep 1) (if (<= n 0) (raise xn) (tcp (- n 1))))])
          (tcp-connect server port)))))
  (values custodian in out))