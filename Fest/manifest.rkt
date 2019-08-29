#lang racket/base

(provide
  #%datum
  #%top
  #%app
  quote
  (rename-out [-find-system-path find-system-path])
  (rename-out [-#%module-begin #%module-begin]))

(require
  (for-syntax
    (only-in cs4500-f18-fest/private/config CS4500-CONFIG-ID)
    racket/base
    syntax/parse))

;; =============================================================================

(define (-find-system-path x)
  (path->string (find-system-path x)))

(define-syntax (-#%module-begin stx)
  (syntax-parse stx #:datum-literals (define)
   ((_ (define key*:id val*:expr) ...)
    #`(#%plain-module-begin
        (require racket/contract cs4500-f18-fest/private/config)
        (provide (contract-out [#,CS4500-CONFIG-ID fest-config/c]))
        (define #,CS4500-CONFIG-ID
          (for/fold ((acc cs4500-default#))
                    ((k (in-list '(key* ...)))
                     (v (in-list (list val* ...))))
            (hash-set acc k v)))))))

(module* reader syntax/module-reader
  cs4500-f18-fest/manifest
  #:read read
  #:read-syntax read-syntax)
