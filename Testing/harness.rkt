#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" ${1+"$@"}
|#
#lang racket

;; Use as:
;;    $ xtest PathToTests PathToExecutable 
;; 
;; This test harness pipes test input files into an executable either via
;; STDIN or LOCALHOST TCP and waits for results on STDOUT or LOCALHOST TCP,
;; respectively. The test directories must contain pairs of files: one for
;; the input, and one for the expected output. The files are determined by
;; the following functions: 

(define (recognize-input-filename fn)
  (regexp-match #px"^(.*/)?([^/]+-)?([0-9]+)-in.json$" fn))

(define (matching-output-file-name prefix numberstr)
  (format "~a~a-out.json" (or prefix "") numberstr))

;; TODO:
;; -- security for student file acces (shill?) 

(provide

 ;; [Parameter Boolean]
 test-plain-and-pretty-json?              ;; preset by default 
 test-fast-and-slow-delivery?             ;; the remaining are unset 
 test-with-and-without-trailing-newline? 
 test-with-and-without-escaped-unicode?
 test-with-batch-mode?
 
 (contract-out
  [client
   ;; run _to-be-tested_ on tests in _test-directory_
   ;; the directory contains pairs of files:
   ;; -- recognize-input-file-name ( <n>-in.json for a natural <n> )
   ;; -- matching-output-file-name ( <n>-out.json for <n>-in.json )
   ;; see above for the two functions
   ;; #:tcp   determines the TCP port for LOCALHOST testing, if any
   ;; #:check can be used to determine the validity of the JSON input
   ;;         there is also a well-formedness check (but it is diabled)
   #; (-> [List FileName [Listof JSexpr] FileName [Listof JSexpr]] Boolean)
   (->i (#:check [valid-json (-> any/c any)])
        (#:tcp   (tcp-on (or/c #false (and/c (>/c 1024) (</c 65000)))))
        (r (->i ([tests-directory path-string?] [to-be-tested path-string?]) [r any/c])))]
  
  [server
   ;; like client, but plays role of TCP server if tcp is #t
   (->i (#:check [valid-json (-> any/c any)])
        (#:tcp   (tcp-on (or/c #false (and/c (>/c 1024) (</c 65000)))))
        (r (->i ([tests-directory path-string?] [to-be-tested path-string?]) [r any/c])))]

  [client/no-tests
   ;; run _to-be-tested_ on test inputs from STDIN 
   ;; #:tcp determines whether the communication uses TCP; STDIN/STDOUT is default 
   ;; #:check can be used to determine the validity of the JSON input
   ;;         there is also a well-formedness check (but it is diabled)
   #; (-> [List FileName [Listof JSexpr] FileName [Listof JSexpr]] Boolean)
   (->i (#:check [valid-json (-> any/c any)])
        (#:tcp   (tcp-on (or/c #false (and/c (>/c 1024) (</c 65000)))))
        (r (->i ([to-be-tested path-string?]) [r any/c])))]))

;                                                                                      
;       ;                                  ;                                           
;       ;                                  ;                          ;                
;       ;                                  ;                                           
;    ;;;;   ;;;   ;;;;    ;;;   ; ;;    ;;;;   ;;;   ; ;;    ;;;    ;;;    ;;;    ;;;  
;   ;; ;;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;;  ;     ;   ;;  ;  ;   ; 
;   ;   ;  ;   ;; ;   ;  ;   ;; ;   ;  ;   ;  ;   ;; ;   ;  ;         ;   ;   ;; ;     
;   ;   ;  ;;;;;; ;   ;  ;;;;;; ;   ;  ;   ;  ;;;;;; ;   ;  ;         ;   ;;;;;;  ;;;  
;   ;   ;  ;      ;   ;  ;      ;   ;  ;   ;  ;      ;   ;  ;         ;   ;          ; 
;   ;; ;;  ;      ;; ;;  ;      ;   ;  ;; ;;  ;      ;   ;  ;;        ;   ;      ;   ; 
;    ;;;;   ;;;;  ;;;;    ;;;;  ;   ;   ;;;;   ;;;;  ;   ;   ;;;;   ;;;;;  ;;;;   ;;;  
;                 ;                                                                    
;                 ;                                                                    
;                 ;                                                                    

(require SwDev/Testing/communication)
(require SwDev/Testing/tcp)
(require json)
(require (for-syntax racket/syntax))

(module+ test
  (require rackunit))

;                                                                        
;                                                                        
;                                               ;                        
;                                               ;                        
;   ;;;;   ;;;;    ;;;;  ;;;;  ;;;;;;   ;;;   ;;;;;   ;;;    ;;;;   ;;;  
;   ;; ;;      ;   ;;  ;     ; ;  ;  ; ;;  ;    ;    ;;  ;   ;;  ; ;   ; 
;   ;   ;      ;   ;         ; ;  ;  ; ;   ;;   ;    ;   ;;  ;     ;     
;   ;   ;   ;;;;   ;      ;;;; ;  ;  ; ;;;;;;   ;    ;;;;;;  ;      ;;;  
;   ;   ;  ;   ;   ;     ;   ; ;  ;  ; ;        ;    ;       ;         ; 
;   ;; ;;  ;   ;   ;     ;   ; ;  ;  ; ;        ;    ;       ;     ;   ; 
;   ;;;;    ;;;;   ;      ;;;; ;  ;  ;  ;;;;    ;;;   ;;;;   ;      ;;;  
;   ;                                                                    
;   ;                                                                    
;   ;                                                                    

#; {type Setup = [->* InputPort OutputPort [-> Void]]}

(define test-plain-and-pretty-json?             (make-parameter #t))
(define test-fast-and-slow-delivery?            (make-parameter #f))
(define test-with-and-without-trailing-newline? (make-parameter #f))
(define test-with-and-without-escaped-unicode?  (make-parameter #f))
(define test-with-batch-mode?                   (make-parameter #f))

;; This is to make the external parameter names consistent with Tony's implementation 
(define-syntax (def/setting stx)
  (syntax-case stx ()
    [(_ para pos neg)
     (with-syntax ([test-para (format-id stx "test-~a" #'para)])
       #'(define para (make-parameter (lambda () (if (test-para) pos neg)))))]))

(def/setting plain-and-pretty-json?             '(#f #t) '(#f))
(def/setting fast-and-slow-delivery?            '(#f #t) '(#f))
(def/setting with-and-without-trailing-newline? '(#t #f) '(#t))
(def/setting with-and-without-escaped-unicode?  '(#f #t) '(#f))

;                                                                                                    
;                                                                                                    
;                                                                  ;;;       ;                   ;   
;                                                                    ;                           ;   
;    ;;;    ;;;    ;;;;  ;   ;   ;;;    ;;;;                 ;;;     ;     ;;;    ;;;   ; ;;   ;;;;; 
;   ;   ;  ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ;               ;;  ;    ;       ;   ;;  ;  ;;  ;    ;   
;   ;      ;   ;;  ;      ; ;   ;   ;;  ;                   ;        ;       ;   ;   ;; ;   ;    ;   
;    ;;;   ;;;;;;  ;      ; ;   ;;;;;;  ;                   ;        ;       ;   ;;;;;; ;   ;    ;   
;       ;  ;       ;      ; ;   ;       ;                   ;        ;       ;   ;      ;   ;    ;   
;   ;   ;  ;       ;       ;    ;       ;       ;;          ;;       ;       ;   ;      ;   ;    ;   
;    ;;;    ;;;;   ;       ;     ;;;;   ;       ;;           ;;;;     ;;   ;;;;;  ;;;;  ;   ;    ;;; 
;                                               ;                                                    
;                                              ;;                                                    
;                                                                                                    

(define ((server #:check valid-json #:tcp (tcp #false)) tests-directory-name program-to-be-tested)
  (define setup
    (make-setup
     program-to-be-tested
     (lambda (stdout stdin)
       (define listener (tcp-listen (or tcp REMOTE-PORT) 30 #true))
       (cond
         [(not tcp) (values stdout stdin)]
         [(sync/timeout ACCEPT-TIMEOUT listener) => tcp-accept]
         [else (displayln `(failed to accept a connection within ,ACCEPT-TIMEOUT seconds))
               (values #f #f)]))))
  (work-horse setup program-to-be-tested tests-directory-name valid-json))

(define ((client #:check valid-json #:tcp (tcp #false)) tests-directory-name program-to-be-tested)
  (define setup
    (make-setup
     program-to-be-tested
     (lambda (stdout stdin)
       (if tcp (try-to-connect-to-times 10 tcp) (values stdout stdin)))))
  (work-horse setup program-to-be-tested tests-directory-name valid-json))

(define ((client/no-tests #:check valid-json #:tcp (tcp #f) #:stdin (stdin #f)) program-to-be-tested)
  (define (connect stdout stdin) (if tcp (try-to-connect-to-times 10 tcp) (values stdout stdin)))
  (define setup (make-setup program-to-be-tested connect #:stdin stdin))
  (work-horse/no-tests setup program-to-be-tested valid-json))

#; ([InputPort OutputPort -> (values InputPort OutputPort)] -> Setup)
(define (make-setup program-to-be-tested f #:stdin (config #f))
  (define (setup)
    (define custodian (make-custodian))
    (parameterize ((current-custodian custodian)
                   (current-subprocess-custodian-mode 'kill))
      (define-values (stdout stdin pid _stderr query) (spawn-process program-to-be-tested))

      (when config
        (with-input-from-file config
          (lambda ()
            (define lines (port->lines))
            (display-lines lines stdin)
            (flush-output stdin))))
      
      (define (tear-down)
        (custodian-shutdown-all custodian)
        (kill-process query))
      
      (define-values (in out) (f stdout stdin))
      (values in out tear-down)))
  setup)

#; (N Port -> (values InputPort OutputPort))
(define (try-to-connect-to-times retry-limit tcp)          
  (define-values (in out)
    (let retry ((count 1))
      (cond
        [(> count retry-limit) (die `(no connection after ,retry-limit tries))]
        [else 
         (sleep 0.5)
         (with-handlers ((exn:fail:network? (lambda (x) (retry (+ 1 count)))))
           (tcp-connect LOCALHOST tcp))])))
  (values in out))

#; (Any -> never returns)
(define (die any)
  (log-error "~v" any)
  (exit 1))

#; (PathString [Any ...] -> Void)
(define (spawn-process command . args)
  (apply values (apply process*/ports #f #f (current-error-port) command args)))

;; ProcessId -> Void 
(define (kill-process query)
  (query 'kill))

;                                                                 
;                        ;      ;                                 
;                        ;      ;                                 
;                        ;      ;                                 
;  ;     ;  ;;;    ;;;;  ;  ;   ; ;;    ;;;    ;;;;   ;;;    ;;;  
;  ;     ; ;; ;;   ;;  ; ;  ;   ;;  ;  ;; ;;   ;;  ; ;   ;  ;;  ; 
;   ; ; ;  ;   ;   ;     ; ;    ;   ;  ;   ;   ;     ;      ;   ;;
;   ; ; ;  ;   ;   ;     ;;;    ;   ;  ;   ;   ;      ;;;   ;;;;;;
;   ;; ;;  ;   ;   ;     ; ;    ;   ;  ;   ;   ;         ;  ;     
;   ;; ;;  ;; ;;   ;     ;  ;   ;   ;  ;; ;;   ;     ;   ;  ;     
;    ; ;    ;;;    ;     ;   ;  ;   ;   ;;;    ;      ;;;    ;;;; 
;                                                                 
;                                                                 
;                                                                 

#; (Setup String JSONCheck -> Void)
(define (work-horse/no-tests setup program-to-be-tested valid-json?)
  (displayln `(running ,program-to-be-tested))
  (for* ([pretty     ([plain-and-pretty-json?])]
         [trickle    ((fast-and-slow-delivery?))]
         [terminated ([with-and-without-trailing-newline?])]
         [escaped    ((with-and-without-escaped-unicode?))])
    (match-define `(,n ,config) (test-one/no-tests pretty trickle terminated escaped setup))
    (displayln `(ran ,program-to-be-tested successfully (,config) and received ,n JSON values))))

#;(Boolean Boolean Boolean Boolean [-> Setup] -> (List N Configuration))
(define (test-one/no-tests pretty trickle terminated escaped setup)
  (define pretty-sym (if pretty 'pretty 'plain))
  (define trickle-sym (if trickle 'slow 'fast))
  (define terminated-sym (if terminated 'with-newline 'with-space))
  (define escaped-sym (if escaped 'escaped-unicode 'plain-unicode))
  (define classification (list pretty-sym trickle-sym terminated-sym escaped-sym))
  (log-info " ... ~v" classification)
  (define-values (in out tear-down) (setup))
  (define actual 
    (parameterize ((pretty-print-output? pretty)
                   (trickle-output?      trickle)
                   (trailing-newline?    terminated)
                   (encode-all-unicode?  escaped)
                   (current-input-port   in)
                   (current-output-port  out))
      (read-rest)))
  (tear-down)
  (log-info "received ~v" actual)
  (list (length actual) classification))

;; ---------------------------------------------------------------------------------------------------
#; (Setup String Path JSONCheck -> Void)
(define (work-horse setup program-to-be-tested tests-directory-name valid-json?)
  (parameterize ()
    (displayln `(testing ,program-to-be-tested))
    
    (define file*      (json-test-files (in-directory tests-directory-name (lambda (_path) #f))))
    (define test*      (retrieve-all-tests file*))
    (define all-tests  (eliminate-bad-tests valid-json? test*))
    (define passed#    (test-them setup all-tests))
    (define all-tests# (length all-tests))
    
    (displayln `(tested ,program-to-be-tested successfully on ,passed# cases out of ,all-tests#))))

#;(Setup [Listof TestSpec] -> {0,1})
(define (test-them setup all-tests)
  (for/sum ((t all-tests) (i-th-test (in-naturals)))
    (match-define `(,in-fname ,input* ,out-fname ,expected-out) t)
    (displayln `(testing ,in-fname ,out-fname) (current-error-port))
    (define actual-output
      (for*/list ([pretty     ([plain-and-pretty-json?])]
                  [trickle    ((fast-and-slow-delivery?))]
                  [terminated ([with-and-without-trailing-newline?])]
                  [escaped    ((with-and-without-escaped-unicode?))])
        (test-one pretty trickle terminated escaped setup input*)))
    (define comparison-result (compare input* expected-out actual-output))
    (define judgement         (make-judgment comparison-result))
    (if (eq? comparison-result 'ok) 1 0)))

#; (Boolean Boolean Boolean Boolean [-> Setup] [Listof JSexpr]
            ->
            [List [List Symbol Symbol Symbol Symbol] JSexpr])
;; run a single test in the specified context 
(define (test-one pretty trickle terminated escaped setup input*)
  (define pretty-sym (if pretty 'pretty 'plain))
  (define trickle-sym (if trickle 'slow 'fast))
  (define terminated-sym (if terminated 'with-newline 'with-space))
  (define escaped-sym (if escaped 'escaped-unicode 'plain-unicode))
  (define classification (list pretty-sym trickle-sym terminated-sym escaped-sym))
  (log-info " ... ~v" classification)
  (define-values (in out tear-down) (setup))
  (define actual 
    (parameterize ((pretty-print-output? pretty)
                   (trickle-output?      trickle)
                   (trailing-newline?    terminated)
                   (encode-all-unicode?  escaped))
      (if (and in out) (feed-and-receive in out input*) 'failed-to-establish-connection)))
  (tear-down)
  (list classification actual))

#;{ type TestSpec = [List FileName JSexpr FileName JSexpr]}

#;([Listof TestSpec] [JSexpr -> Boolean] -> [Listof TestSpec])
(define (eliminate-bad-tests valid-json? test*)
  (filter (make-exn-safe valid-json?) (filter wff-json? test*)))

#; ([Listof [List FileName FileName]] -> [Listof TestSpec])
(define (retrieve-all-tests file*)
  (for/list ((x file*))
    (match-define `(,in-fname ,out-fname) x)
    (match-define `(,input ,output) (list (file->json in-fname) (file->json out-fname)))
    (list in-fname input out-fname output)))

;; Symbol -> Symbol 
(define (make-judgment comparison-result)
  (match comparison-result
    ['fail    'FAILED]
    ['ok      'PASSED]
    ['partial 'MOSTLY-FAILED]))

;; ---------------------------------------------------------------------------------------------------
#; (InputPort OutputPort [Listof JSexpr] -> [Listof JSexpr])
(define (feed-and-receive in out input*)
  
  (define batch? (test-with-batch-mode?))

  ;; [Listof JSexpr] -> [Listof JSexpr]
  ;; EFFECT
  ;;  (1) send all inputs to the input port
  ;;  (2) if interactive, read response for each input
  ;;  (3) in any case, when all inputs are sent, close port and read all responses 
  (define (write-and-read remaining-inputs)
    (match remaining-inputs
      ['() (close-output-port (current-output-port))
           (read-rest)]
      [(cons i rest)
       (send-message i)
       (if batch?
           (write-and-read rest)
           (match (read-message)
             [(? eof-object?) '()]
             [(? terminal-value? v) (list v)]
             [v (cons v (write-and-read rest))]))]))
  
  (parameterize ((current-input-port in)
                 (current-output-port out))
    (write-and-read input*)))

;; -> [Listof JSexpr]
;; EFFECT keep reading until current output port is closed 
(define (read-rest)
  (define next (read-message))
  (match next
    [(? eof-object?) '()]
    [(? terminal-value? v) (list v)]
    [v (cons v (read-rest))]))

;; ---------------------------------------------------------------------------------------------------
(define (make-exn-safe test-pred)
  (lambda (t)
    (match-define `(,in-fname ,_ ,_ ,_) t)
    (define ok?
      (with-handlers [(exn? (lambda (e)
                              (local-require racket/exn)
                              (define str (exn->string e))
                              (log-error "Test validation for ~e failed:\n~a" in-fname str)
                              #f))]
        (test-pred t)))
    (when (not ok?) (displayln `(INVALID-TEST ,in-fname)))
    ok?))

(define (wff-json? t)
  #true
  ;; why did Tony test list-ness of each input and outpu? 
  #;
  (match t
    [(list _in-fname (? list?) _out-fname (? list?)) #t]
    [_ #f]))

;                                                   
;                                                   
;                                                   
;                                                   
;    ;;;    ;;;  ;;;;;;  ;;;;   ;;;;    ;;;;   ;;;  
;   ;;  ;  ;; ;; ;  ;  ; ;; ;;      ;   ;;  ; ;;  ; 
;   ;      ;   ; ;  ;  ; ;   ;      ;   ;     ;   ;;
;   ;      ;   ; ;  ;  ; ;   ;   ;;;;   ;     ;;;;;;
;   ;      ;   ; ;  ;  ; ;   ;  ;   ;   ;     ;     
;   ;;     ;; ;; ;  ;  ; ;; ;;  ;   ;   ;     ;     
;    ;;;;   ;;;  ;  ;  ; ;;;;    ;;;;   ;      ;;;; 
;                        ;                          
;                        ;                          
;                        ;                          

;; [Listof JSexpr] [Listof JSexpr] [Listof (List Symbol (Listof JSexpr))] -> Symbol
;; compare: 'ok for the expected equals actual; 'partial for some passes; 'fail otherwise
;; effect: write out diff for failed test
(define (compare input* expected-out actual-outputs)
  (define number-outputs (length actual-outputs))
  (define partial-score  (/ number-outputs))
  (define score (for/sum [(entry actual-outputs)]
                  (match-define (list _classification actual-out) entry)
                  (if (compare-expected-actual expected-out actual-out) partial-score 0)))
  (cond
    [(= score 1) 'ok]
    [else
     (displayln '---------------------------------)
     (displayln `(*** score ,score))
     (displayln `(*** on))
     (pretty-print input*)
     (displayln '(*** expected))
     (pretty-print expected-out)
     (displayln `(*** but received))
     (pretty-print actual-outputs)
     (if (zero? score)
         'fail
         'partial)]))

#; {[Listof JSExpr] [Listof JSExpr] -> Booleaan}
(define (compare-expected-actual expected-out actual-out)
  (or (equal? (spy expected-out) (spy actual-out))
      (spy (match* (expected-out actual-out)
        [((list(? string? expected-single)) (list (? string? actual-single)))
         (regexp-match expected-single actual-single)]
        [(_ _) #false]))))

(require SwDev/Debugging/spy)

;; -----------------------------------------------------------------------------
;; String -> [Maybe [Listof JSexpr]]
;; read f as JSON file if possible 
;; effect: display its name if it is not
(define (file->json f)
  (with-input-from-file f (all-json-expressions f)))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  (check-equal? (with-input-from-string "1" (all-json-expressions "nonsense"))
                '(1)
                "true good file")
  
  (check-equal? (with-output-to-string
                  (lambda ()
                    (with-input-from-string "1" (all-json-expressions "nonsense"))))
                ""
                "frame condition: no effect for good file")
  
  (check-equal? (with-output-to-string
                  (lambda ()
                    (with-input-from-string "()" (all-json-expressions "nonsense"))))
                "(nonsense is not a JSON file)\n"
                "write for bad file")
  
  (void
   (with-output-to-string
     (lambda ()
       (check-equal? (with-input-from-string "()" (all-json-expressions "nonsense"))
                     '()
                    "false for bad file")))))

(define ((all-json-expressions f))
  (with-handlers ((exn:fail:read?
                   (lambda (xn)
                     (displayln `(,f is not a JSON file))
                     '())))
    (let all-json-lines ()
      (define next (read-message))
      (cond
        [(eof-object? next) '()]
        [else (cons next (all-json-lines))]))))

;; -----------------------------------------------------------------------------
;; [Listof Path] -> [Listof [List String String]]
;; Find all pairs of the form x-i-in.json and x-i-out.json or
;; i-in.json and i-out.json for i in Nat+ and for all x in file name
;; prefixes.

(module+ test
  
  (check-equal? (json-test-files '()) '())
  
  (define path0 (list (build-path "1-in.json") (build-path "1-out.json")))
  (define strg0 (list (map path->string path0)))
  (check-equal? (json-test-files path0) strg0)
  
  (define path1 (list (build-path "1-in.json") (build-path "2-out.json")))
  (check-equal? (json-test-files path1) '())

  (define path2 (list (build-path "/foo/bar/1-in.json")
                      (build-path "/foo/bar/1-out.json")
                      (build-path "/foo/bar/zot-1-in.json")
                      (build-path "/foo/bar/zot-1-out.json")
                      (build-path "/foo/bar/foo-bar-1-in.json")
                      (build-path "/foo/bar/foo-bar-1-out.json")
                      (build-path "/foo/bar/quux-2-in.json")
                      (build-path "/foo/bar/quux-3-out.json")
                      (build-path "/foo/bar/foo-bar-2-in.json")
                      (build-path "/foo/bar/foo-bar-3-out.json")
                      (build-path "/foo/bar/2-out.json")
                      (build-path "/foo/bar/3-in.json")))
  (check-equal? (json-test-files path2)
                (list (list "/foo/bar/1-in.json" "/foo/bar/1-out.json")
                      (list "/foo/bar/foo-bar-1-in.json" "/foo/bar/foo-bar-1-out.json")
                      (list "/foo/bar/zot-1-in.json" "/foo/bar/zot-1-out.json")))
  
  (check-equal? (json-test-files (append path0 path1)) strg0))

(define (json-test-files d)
  (define all-json-files
    (for/list ((f d)  #:when #px"^(.*/)?([^/]+-)?[0-9]+-[^/]*.json$")
      (path->string f)))

  (define (path-with-file-named x)
    (findf (lambda (f) (and (>= (string-length f) (string-length x))
                            (equal? (substring f
                                               (- (string-length f) (string-length x))
                                               (string-length f))
                                    x)
                            (or (= (string-length f) (string-length x))
                                (char=? (string-ref f (- (string-length f) (string-length x) 1))
                                        #\/))))
           all-json-files))

  (sort
   (remove-duplicates
    (filter-map
     (lambda (input-filename)
       (cond
         [(recognize-input-filename input-filename)
          =>
          (match-lambda
            [(list _entire _dir prefix numberstr)
             (define matched-name    (matching-output-file-name prefix numberstr))
             (define output-filename (path-with-file-named matched-name))
             (and output-filename (list input-filename output-filename))])]
         [else #f]))
     all-json-files))
   string<?
   #:key car))

#|

When you use `system` or `process`, the immediate new process runs a
shell. The shell process then starts another one to run the command
that you give it.

I recommend using `process*` to avoid the shell process and to avoid
encoding issues when passing arguments:

(define racket (find-executable-path "racket"))
... (process* racket "sleeper.rkt") ...

Another approach is to create a fresh process group for the shell
process, and then `((fifth pl) 'kill)` kills the whole group:

 (parameterize ([subprocess-group-enabled #t])
   (process "racket sleeper.rkt"))

Finally, you could tell the shell to not create a subprocess and
instead replace itself with the other program"

 (process "exec racket sleeper.rkt")

|#