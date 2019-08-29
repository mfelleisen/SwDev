#lang racket/base

(require racket/runtime-path)

(define-runtime-path bad-0 "./manifest/bad-0.rkt")
(define-runtime-path good-0 "./manifest/good-0.rkt")

(module+ test
  (require
    rackunit
    cs4500-f18-fest/private/config)

  (test-case "bad-manifest"
    (check-exn exn:fail:contract?
      (lambda () (manifest->config bad-0))))

  (test-case "good-manifest"
    ;; TODO why labeled as 2 tests?
    (check-not-exn
      (lambda () (manifest->config good-0)))))
