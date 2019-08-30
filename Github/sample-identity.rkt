#lang racket

(provide personal-token-id)

(require github-api)

(define personal-token "personal token here")
(define username "username here")
(define personal-token-id (github-identity 'personal-token (list username personal-token)))
