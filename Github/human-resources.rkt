#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; People, Teams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct person (last first email last4 username) #:prefab)
(struct team (a b id) #:prefab)

(define (person-initials p)
  (string-append (substring (person-first p) 0 2)
                 (substring (person-last p) 0 2)))

(define (person-name p)
  (string-append (person-first p) " " (person-last p)))

(define (person-initials/uc p)
  (string-upcase (person-initials p)))

(define (person-initials/dc p)
  (string-downcase (person-initials p)))

(define (team-initials t [sep ""])
  (string-append (person-initials (team-a t))
                 sep
                 (person-initials (team-b t))))

(define (team-initials/uc t [sep ""])
  (string-upcase (team-initials t sep)))

(define (team-initials/dc t [sep ""])
  (string-downcase (team-initials t sep)))

(define (team-name tm)
  (cond
    [(equal? (team-id tm) 3048)
     "SBZW"]
    [(equal? (team-id tm) 3064)
     "FIJA-DIPE"]
    [else
     (team-initials/uc tm "-")]))

(define (team-repo-name tm)
  (cond
    [(equal? (team-id tm) 3048)
     "sbzw"]
    [(equal? (team-id tm) 3064)
     "fija-dipe"]
    [else
     (team-initials/dc tm "-")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Looking up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-in-section repo-name tms)
  (findf (lambda (t) (equal? repo-name (team-repo-name t)))
         tms))