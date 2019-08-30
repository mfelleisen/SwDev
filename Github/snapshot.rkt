#lang racket

(require "github-utils.rkt")
(require racket/system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fetch, sync repos with GitHub
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dir : Path, directory to put the repos in
(define (create-snapshot gh dir)
  (let/ec esc
    (define r (list-org-repos gh ORG))
    (ensure-success! r esc)
    (for ([repo-data (in-list (github-response-data r))])
      (define clone-url (hash-ref repo-data 'ssh_url))
      (define repo-name (hash-ref repo-data 'name))
      (define clone-path (build-path dir repo-name))
      (or (git-clone clone-url clone-path)
          (esc repo-data)))))

;; assume all the repos exist, checkout current origin/master
(define (sync-repos dir)
  (for ([repo-name (in-list (directory-list dir))])
    (parameterize ([current-directory (build-path dir repo-name)])
      (define r
        (and (system "git fetch origin > /dev/null 2> /dev/null")
             (system "git merge --ff-only origin/master > /dev/null 2> /dev/null")))
      (unless r
        (printf "issue with ~a\n" repo-name)))))

;; time format: "YYYY-DD-DD H{H}:MM"
;; e.g. "2018-09-10 1:00"
(define (sync-repos-to-time dir date-time)
  (for ([repo-name (in-list (directory-list dir))])
    (parameterize ([current-directory (build-path dir repo-name)])
      (and (system "git fetch origin")
           (system (~a "git checkout `git rev-list -n 1 --before='" date-time "' origin/master`"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; URLString Path ErrorKont -> 
(define (git-clone url path)
  (define cmd (~a "git clone " url " " path))
  (system cmd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; usage
;; > racket snapshot.rkt sync-master directory
;;     for each repo inside directory, checkout origin/master
;; > racket snapshot.rkt snapshot directory time
;;     clone each teams repo into directory, then checkout the most recent commit before `time`
;;     `time` = YYYY-MM-DD {H}H:MM
(define (main)
  (match (current-command-line-arguments)
    [(vector "sync-master" (? path-string? p))
     (sync-repos p)]
    [(vector "create" (? path-string? p))
     (define d (build-path (current-directory) p))
     (unless (directory-exists? d)
       (make-directory d))
     (create-snapshot api-handle d)]
    [(vector "snapshot" (? path-string? p) (and time (pregexp #px"\\d{4}-\\d{2}-\\d{2} \\d?\\d:\\d\\d")))
     (define d (build-path (current-directory) p))
     (unless (directory-exists? d)
       (make-directory d))
     (create-snapshot api-handle d)
     (sync-repos-to-time d time)
     ]
    [_ #f]))

(module+ main
  (main))
