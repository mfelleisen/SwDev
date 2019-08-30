#lang racket

(provide (all-defined-out)
         (all-from-out github-api)
         (all-from-out json)
         (all-from-out "human-resources.rkt"))

(require github-api)
(require json)
(require "github-identity.rkt")
(require "human-resources.rkt")

(define api-handle
  (github-api personal-token-id
              #:endpoint "github.ccs.neu.edu"))

;; REPLACE WITH YOUR ORG
(define ORG "CS4500-F18")
(define TEST-ORG "sw-dev-test")

(define API-URI-PREFIX "/api/v3/")
(define (make-uri . args)
  (apply string-append API-URI-PREFIX args))

(define (ensure-success! resp k)
  (unless (success-response? resp)
    (k resp)))

;; below, gh should usually be the api-handle above
;; note these aren't using the routines provided by `github-api` becuase there appears to be a bug in
;; how the package constructs uris

(define (success-status? s)
  (<= 200 s 299))

(define (success-response? r)
  (success-status? (github-response-code r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GitHub API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add the username to the organization
(define (add-to-org gh org username #:role [role "member"])
  (define uri (make-uri "orgs/" org "/memberships/" username))
  (define data (jsexpr->string (hasheq 'role role)))
  (gh uri #:method "PUT" #:data data))

;; create a team
(define (create-team gh org name)
  (define uri (make-uri "orgs/" org "/teams"))
  (define data (jsexpr->string (hasheq 'name name
                                       'privacy "secret")))
  (gh uri #:method "POST" #:data data))

;; add member to team -- should also add member to team's organization
(define (add-member-to-team gh team-id username #:role [role "member"])
  (define uri (make-uri "teams/" (~a team-id) "/memberships/" username))
  (define data (jsexpr->string (hasheq 'role role)))
  (gh uri #:method "PUT" #:data data))

;; assign a team to a repo
(define (add-team-to-repo gh team-id owner repo #:permission [permission "push"])
  (define uri (make-uri "teams/" (~a team-id) "/repos/" owner "/" repo))
  (define data (jsexpr->string (hasheq 'permission permission)))
  (gh uri #:method "PUT" #:data data))

;; check team's relationship with repo
(define (check-team-repo gh team-id owner repo)
  (define uri (make-uri "teams/" (~a team-id) "/repos/" owner "/" repo))
  (gh uri #:method "GET"))

;; create an organization repo
(define (create-org-repo gh org repo #:private [private? #true])
  (define uri (make-uri "orgs/" org "/repos"))
  (define data (jsexpr->string (hasheq 'name repo
                                       'private private?)))
  (gh uri #:method "POST" #:data data))

;; get all teams
(define (list-org-teams gh org)
  (define uri (make-uri "orgs/" org "/teams?per_page=100"))
  (gh uri #:method "GET"))

;; 100 is the upper limit, I believe, on per_page, so this should be fine unless
;; we have more than 100 repos
(define (list-org-repos gh org [page 1])
  (define uri (make-uri "orgs/" org "/repos?page=" (~a page) "&per_page=100"))
  (gh uri #:method "GET"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -> (U GitHubResponse #;failure
;;       #f #;success,not-found
;;       JSValue #;success,found)
(define (find-team-by-name gh org tm)
  (let/ec esc
    (define r (list-org-teams api-handle org))
    (ensure-success! r esc)
    (define tm-nm (team-name tm))
    (findf (lambda (r) (equal? (hash-ref r 'name) tm-nm))
           (github-response-data r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ASSUME current directory is in the repository
(define (checkout-repo-to-master!)
  (define tmp-branch-name (gensym 'tmp980u903u4))
  (system (~a "git fetch origin; git checkout -b " tmp-branch-name "; git branch -D master; "
              "git checkout origin/master; git branch -D " tmp-branch-name "; "
              "git checkout -b master")))
