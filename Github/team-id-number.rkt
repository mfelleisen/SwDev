#lang racket/base

(require
  racket/set
  racket/runtime-path
  racket/string
  racket/file)

(module+ test
  (require rackunit))

;; -----------------------------------------------------------------------------

(define kv* '())

(module+ test
  (test-case "unique-pins"
    (check-equal? (length kv*) (set-count (for/set ((x (in-list kv*))) (cadr x))))))

(struct presentation-record (team-record*) #:prefab)
(struct mf-record presentation-record () #:prefab)
(struct jh-record presentation-record () #:prefab)
(struct team-record (person-record*) #:prefab)
(struct person-record (full-name email missing-dates present-dates head-dates assistant-dates scribe-dates) #:prefab)
(struct date-record (day*) #:prefab)
(struct planned-absences date-record () #:prefab)
(struct presenter date-record () #:prefab)
(struct head date-record () #:prefab)
(struct assistant date-record () #:prefab)
(struct scribe date-record () #:prefab)

(define-runtime-path presentation-record.rktd "presentation-schedule/presentation-record.rktd")

(define all-teams
  (let ((v (file->value presentation-record.rktd)))
    (apply append (map presentation-record-team-record* v))))

(define (person-record->initials pr)
  (define fn (person-record-full-name pr))
  (define str* (string-split fn " "))
  (string-append (two-alphas (car str*))
                 (two-alphas (cadr str*))))

(define (two-alphas str)
  (define got (box 0))
  (apply string
    (for/list ((c (in-string str))
               #:when (char-alphabetic? c)
               #:break (= 2 (unbox got)))
      (set-box! got (+ 1 (unbox got)))
      (char-downcase c))))

(module+ test
  (test-case "person-record->initials"
    (check-equal? (person-record->initials (person-record "Anon Stein" "anon@anon.anon" #f #f #f #f #f))
                  "anst")))

(define (make-team-matcher id)
  (case id
    ((sbzw)
     (make-team-matcher 'sube-zawa))
    ((alzi-zama)
     (make-team-matcher 'alzi))
    (else
      (define id* (string-split (symbol->string id) "-"))
      (lambda (tr)
        (define n* (map person-record->initials (team-record-person-record* tr)))
        (or (equal? id* n*)
            #;(equal? id* (reverse n*)))))))

(define (id->emails id)
  (define team-match? (make-team-matcher id))
  (define team* (filter team-match? all-teams))
  (when (or (null? team*) (not (null? (cdr team*))))
    (raise-arguments-error 'id->emails "ambiguous team" "id" id "team*" team*))
  (map person-record-email (team-record-person-record* (car team*))))

(module+ test
  (test-case "all-teams-matched"
    (check-not-exn
      (lambda () (for ((kv (in-list kv*))) (id->emails (car kv)))))))

(module+ main
  (require mutt)
  (parameterize (#;(*mutt-exe-path* #f))
    (for ((kv (in-list kv*)))
      (define e* (id->emails (car kv)))
      (define pin (cadr kv))
      (define msg (format "Greetings,~n~nYour CS4500 team identification number is:~n~a~n~nYou will need this number to view your test results, once the results are published.~n" pin))
      (mutt msg #:to (car e*) #:subject "cs4500 team identification number" #:cc (cdr e*)))))
