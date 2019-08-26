#lang racket 

;; ---------------------------------------------------------------------------------------------------
(provide
 ;; String -> Void
 ;; effect: writes message to current output port, adds newline and flushes
 pretty-print-output?
 trickle-output?
 trailing-newline?
 encode-all-unicode?

 send-message
 read-message
 terminal-value?)

;; ---------------------------------------------------------------------------------------------------
(require SwDev/Testing/tcp)
(require SwDev/Lib/json-pretty)
(require json)

;; ---------------------------------------------------------------------------------------------------
(define pretty-print-output? (make-parameter #f))
(define trickle-output? (make-parameter #f))
(define trailing-newline? (make-parameter #t))
(define encode-all-unicode? (make-parameter #f))

(define (send-message i)
  (with-handlers ([exn:fail:network? (lambda (e) #f)])
    (define output-bytes
      (cond
        [(pretty-print-output?)
         (let ((o (open-output-string)))
           (write-json/pretty i o #:indent-maps? #t #:indent-lists? #t)
           (get-output-bytes o))]
        [else (jsexpr->bytes i #:encode (if (encode-all-unicode?) 'all 'control))]))
    (define output-length (bytes-length output-bytes))
    (define chunk-size (max (quotient output-length 100) 10))
    (if (trickle-output?)
        (for [(offset (in-range 0 output-length chunk-size))]
          (define chunk (subbytes output-bytes offset (min output-length (+ offset chunk-size))))
          (write-bytes chunk)
          (flush-output)
          (sleep 0.005))
        (write-bytes output-bytes))
    (if (trailing-newline?) (newline) (write-byte 32))
    (flush-output)
    #t))

;; Read a blob of JSON, treating any network error as EOF, and only waiting for TIMEOUT seconds.
;; (Because tcp-read gets RST from linux servers from time to time.)
(define (read-message)
  (with-handlers ([exn:fail:network? (lambda (_exn) eof)])
    (read-json/timeout TIMEOUT TIMEOUT)))

;; Detects values that mean the end of the session with the remote party.
(define (terminal-value? blob)
  (or (eq? blob 'error)
      (eq? blob 'timeout-1)
      (eq? blob 'timeout-2)))

;; Read a blob of JSON with a timeout for the first byte of input to appear
;; and a second timeout by which the entirety of the blob should have appeared.
(define (read-json/timeout start-timeout-sec response-duration-timeout-sec)
  (define control-ch (make-channel))
  (define reply-ch (make-channel))
  (define read-thread
    (thread
     (lambda ()
       (cond
         [(sync/timeout start-timeout-sec (current-input-port))
          (channel-put control-ch 'response-started)
          (with-handlers [(values (lambda (e) (channel-put reply-ch (list 'exn e))))]
            (channel-put reply-ch (list 'ok (read-json))))]
         [else (channel-put control-ch 'response-not-started)]))))
  (match (channel-get control-ch)
    ['response-not-started
     (log-info "Timed out waiting for reading to start.")
     'timeout-1]
    ['response-started
     (match (sync/timeout response-duration-timeout-sec reply-ch)
       [(list 'ok blob)
        blob]
       [(list 'exn (? exn:fail:network?))
        eof]
       [(list 'exn e)
        (local-require racket/exn)
        (log-info "Error reading message:\n~a" (exn->string e))
        `(error ,(exn-message e))]
       [#f
        (log-info "Timed out waiting for reading to complete.")
        'timeout-2])]))

