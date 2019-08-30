#lang racket

(require "github-utils.rkt")
(require (prefix-in mf: "partners-mf-section.rkt"))
(require (prefix-in jh: "partners-jh-section.rkt"))

(module+ test
  (require rackunit))

;; a LanguageCommunity is a (List Symbol Team Team (Listof Team))
;; where the Symbol identifies the language, and
;; the first two Teams correspond to the A and B directorys, respectively
(define javascripts
  (list 'javascript
        mf:t6
        mf:t1
        (append mf:using-javascript
                jh:using-javascript
                ;; INCLUDING TYPESCRIPT
                jh:using-typescript)))
(define python-3.6s
  (list 'python-3.6
        mf:t2
        mf:t13
        (append mf:using-python3.6
                mf:using-python3.7
                jh:using-python3.6
                jh:using-python3.7)))
(define python-3s
  (list 'python-3
        jh:t6
        jh:t20
        (append mf:using-python3
                jh:using-python3)))
(define java-8s
  (list 'java-8
        mf:t8
        mf:t14
        (append mf:using-java8
                jh:using-java8)))
(define javas
  (list 'java
        jh:t11
        jh:t15
        (append mf:using-java
                jh:using-java)))
(define gos
  (list 'go
        mf:t15
        jh:t23
        (append mf:using-go
                jh:using-go)))

(define all-lcs (list javascripts
                      python-3.6s
                      python-3s
                      java-8s
                      javas
                      gos))

;; LanguageCommunity -> (Values (List "A" (Listof RepoName)) (List "B" (Listof RepoName))
;; assign each team in a lang community one of the two repos, and swap the two teams that are
;; providing the repos
;; (the lists are those teams that GET directory A, teams that GET directory B)
(define (assign lc)
  (match-define (list _ tm-a tm-b tms) lc)
  (define undecided-teams (remove* (list tm-a tm-b) tms))
  (define-values (as bs) (split-at undecided-teams
                                   (exact-floor (/ (length undecided-teams) 2))))
  (values (list "A" (map team-repo-name (cons tm-b as)))
          (list "B" (map team-repo-name (cons tm-a bs)))))

(module+ test
  (test-case
   "go"
   (define-values (a b) (assign gos))
   (check-equal? a (list "A" (list (team-repo-name jh:t23))))
   (check-equal? b (list "B" (list (team-repo-name mf:t15))))))

(define (make-assignments out-file)
  (define assignments
    (for/list ([lc (in-list all-lcs)])
       (define-values (as bs) (assign lc))
       (list (first lc) as bs)))
  (with-output-to-file out-file #:exists 'replace
    (thunk
     (pretty-write assignments))))

(module+ main
  (define out-file "week9-swap.rktd")
  ;; un-comment to generate a new assignment
  #;(make-assignments out-file))