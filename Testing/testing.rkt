#lang racket

;; a simple facility for writing tests to two files:
;; <n>-in.json for the test input
;; <n>-out.json for the test output 

(provide
 #; {(U Path-String #false) -> Void}
 ;; path-string -> write the test files into this directory; #false means no output 
 recording 

 #; {[-> Void] [Listof JSexpr] [Listof JSexpr] String -> Void}
 ;; convert inputs and expected to JSON, run main on converted inputs, compare with expected
 ;; IF recording is set, also record the specified test cases as pairs of files
 r-check-equal?

 #; {[-> Void] (U String [Listof JSexpr]) String String -> Void}
 ;; convert inputs to JSON, run main on converted inputs, catch exn, compare with expected
 ;; IF recording is set, also record the specified test cases as pairs of files
 r-check-exn)

(require SwDev/Testing/communication)
(require rackunit)
(require json)

(require SwDev/Debugging/spy)

;; ---------------------------------------------------------------------------------------------------
(define recording (make-parameter #false))

(define (r-check-equal? main inputs expected msg)
  (define in:str (prepare inputs))
  (define ex:str (prepare expected))

  (define actual (gensym))
  (define (tee x) (set! actual x) x)

  (check-equal? (tee (post (with-output-to-bytes (lambda () (with-input-from-bytes in:str main)))))
                expected
                msg)
  
  (record inputs actual))

(define (r-check-exn main inputs expected msg)
  (define in:str (prepare inputs))
  (define rx (curry regexp-match (pregexp expected)))
  
  (define actual (gensym))
  (define (tee x) (set! actual x) x)

  (check-pred rx (tee (first (post (with-output-to-bytes (λ () (with-input-from-bytes in:str main)))))) msg)

  (if (string? inputs) 
      (record (list inputs) (list actual) #:write-inputs displayln)
      (record inputs (list actual))))

#;[[Listof Jsexpr] [Listof Jsexpr] -> Void]
;; write test input and test output to next pair of test files in (recording) directory, if any 
(define *file-no -1)
(define (record input output #:write-inputs (wi write-json #; send-message))
  (define base (recording))
  (when base
    (unless (directory-exists? base) (make-directory base))
    (set! *file-no (+ *file-no 1))
    (define n (~a *file-no))
    (define -in.json  (build-path base (format "~a-in.json" n)))
    (define -out.json (build-path base (format "~a-out.json" n)))
    (write-to -in.json input wi)
    (write-to -out.json output (lambda (x) (send-message x)))))

;; [X] [PathString [Listof X] [X -> Void] -> Void]
;; write and optionally replace file 
(define (write-to file-name input writer)
  (with-output-to-file file-name (lambda () (for ((x input)) (writer x))) #:exists 'replace))

#; {(U String [Listof JSexpr]) -> Bytes}
(define (prepare x)
  (cond
    [(string? x) (string->bytes/utf-8 x)]
    [else (bytes-append (bytes-join (map jsexpr->bytes x) #" "))]))

#;{Bytes -> [Listof JSexpr]}
(define (post x)
  (port->list read-json (open-input-bytes x)))
  
