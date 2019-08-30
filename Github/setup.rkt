#lang racket

(provide setup-team
         sync-teams)

(require "github-utils.rkt")
(require "human-resources.rkt")

;; -> (U #f (List ID TeamName)
(define (setup-team gh tm)
  (match-define (team a b id) tm)
  (cond
    [(and (false? id)
          (person? a)
          (person? b)
          (string? (person-username a))
          (string? (person-username b)))
     (define team-name (team-initials/uc tm "-"))
     (define repo (team-initials/dc tm "-"))
     (let/ec esc
       (define r (create-team gh ORG team-name))
       (ensure-success! r esc)
       (define new-id (hash-ref (github-response-data r) 'id))
       (define r2 (add-member-to-team gh new-id (person-username a)))
       (ensure-success! r2 esc)
       (define r3 (add-member-to-team gh new-id (person-username b)))
       (ensure-success! r3 esc)
       (define r4 (create-org-repo gh ORG repo #:private #true))
       (ensure-success! r4 esc)
       (define r5 (add-team-to-repo gh new-id ORG repo #:permission "push"))
       (ensure-success! r5 esc)
       (list new-id team-name))]
    [else #f]))

(define (sync-teams tms)
  (match tms
    [(cons t more-tms)
     (match (setup-team api-handle t)
       [#f
        ;; already setup or don't have usernames
        (cons (list t 'no-op)
              (sync-teams more-tms))]
       [(? github-response? r)
        ;; error
        (list r t)]
       [(list (? number? new-id) (? string? team-name))
        (cons (list t new-id team-name)
              (sync-teams more-tms))])]
    [_ '()]))

(define (no-op? result)
  (match result
    [(list _ 'no-op) #t]
    [_ #f]))
