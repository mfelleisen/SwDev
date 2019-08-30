#lang racket

(require "github-utils.rkt")

;; takes list of teams
;; (Listof Team) -> (Listof (List Team Team))
(define (pair-teams lang-teams)
  (define shufd (shuffle lang-teams))
  (let loop ([rem shufd])
    (match rem
      ['() '()]
      [(list tm)
       (list (list tm (first shufd)))]
      [(list-rest tm1 tm2 rst)
       (cons (list tm1 tm2)
             (loop (cons tm2 rst)))])))

;; ASSUME all repos are in current directory
(define (shuffle-specs lop)
  (for ([p (in-list lop)])
    (match-define (list (and t-from (team _ _ from-id))
                        (and t-to (team _ _ to-id)))
      p)
    (define from-repo-name (team-repo-name t-from))
    (define to-repo-name (team-repo-name t-to))
    (define dest (build-path to-repo-name "3"))
    (unless (directory-exists? dest)
      (make-directory dest))
    (define src (build-path from-repo-name "2" "spread.pdf"))
    (define dest-file-name "given-2spread.pdf")
    (define dest-file (build-path dest dest-file-name))
    (cond
      [(file-exists? dest-file)
       (printf "skipping team, memo already exists\n")]
      [(file-exists? src)
       (copy-file src dest-file)
       (parameterize [(current-directory to-repo-name)]
         (unless
             (and (checkout-repo-to-master!)
                  (system (~a "git add " (build-path "3" dest-file-name)
                              "; "
                              "git commit -m 'welcome to codemanistan'"
                              "; "
                              "git push origin master")))
           (printf "\n\nError during git-foo for pair ~a\n\n" p)))]
      [else
       (printf "\n\nunable to locate source file for team ~a\n\n" t-from)])))

(define (return-impls lop)
  (for ([p (in-list lop)])
    (match-define (list (and t-to (team _ _ to-id))
                        (and t-from (team _ _ from-id)))
      p)
    (define from-repo-name (team-repo-name t-from))
    (define to-repo-name (team-repo-name t-to))
    (define dest (build-path to-repo-name "3"))
    (unless (directory-exists? dest)
      (make-directory dest))
    (define src-dir (build-path from-repo-name "3"))
    (match (find-server-impl src-dir)
      [#f
       (printf "\n\nUnable to find implementation in repo ~a, team ~a\n\n" from-repo-name t-from)]
      [(? (lambda (f) (directory-exists? (build-path src-dir f))))
       (printf "\n\n skipping from ~a because server is a directory\n\n" from-repo-name)]
      [server-file
       (define src-file (build-path src-dir server-file))
       (define ext? (regexp-match #px".*(\\..*)" server-file))
       (define ext (or (and ext?
                            (second ext?))
                       "PP"))
       (define dest-file-name (path-add-extension "server-for-your-2spread" ext))
       (define dest-file (build-path dest dest-file-name))
       (if (file-exists? dest-file)
           (printf "skipping server copy from ~a\n" from-repo-name)
           (copy-file src-file dest-file))
       (define aux (find-aux-dir src-dir))
       (if (or (false? aux) (directory-exists? (build-path dest "given-Aux")))
           (printf "skipping aux copy from ~a\n" from-repo-name)
           (copy-directory/files (build-path src-dir aux) (build-path dest "given-Aux")))
       (parameterize [(current-directory to-repo-name)]
         (unless
             (and (checkout-repo-to-master!)
                  (system (~a "git add " (build-path "3" dest-file-name)
                              "; "
                              (if aux
                                  (~a "git add " (build-path "3" "given-Aux; "))
                                  "")
                              "git commit -m 'delivery from codemanistan'"
                              "; "
                              "git push origin master")))
           (printf "\n\nError during git-foo for pair ~a\n\n" p)))])))

(define (find-server-impl dir)
  (for/or ([f (directory-list dir)])
    (and (regexp-match #px"for-given-2spread" f)
         f)))

(define (find-aux-dir dir)
  (for/or ([f (directory-list dir)]
           #:when (directory-exists? (build-path dir f)))
    (and (regexp-match #px"^(a|A)ux" f)
         f)))

;; (List LanguageName (Listof Team)) -> (Listof (List LanguageName (Listof (List Team Team))))
(define (create-pairings lols)
  (for/list ([l (in-list lols)])
    (match-define (list lang-name teams) l)
    (define paird (pair-teams teams))
    (list lang-name paird)))

(define (make-pairings lols out-file)
  (define paird (create-pairings lols))
  (with-output-to-file out-file
    (thunk (pretty-write paird))
    #:mode 'text))

(define (read-pairings in-file)
  (with-input-from-file in-file read #:mode 'text))

;; Significant entry point
(define (perform-shuffling in-file)
  (define lang-pairs (read-pairings in-file))
  (define team-pairs (apply append (map second lang-pairs)))
  (shuffle-specs team-pairs))

#|
(require (prefix-in mf: "partners-mf-section.rkt"))
(require (prefix-in jh: "partners-jh-section.rkt"))
(define (make-pairs-from-actual-teams out-file)
  ;; Leaving out Haskell, taking care of manually
  (define javascripts
    (list 'javascript
          (append mf:using-javascript
                  jh:using-javascript
                  ;; INCLUDING TYPESCRIPT
                  jh:using-typescript)))
  (define python-3.6s
    (list 'python-3.6
          (append mf:using-python3.6
                  mf:using-python3.7
                  jh:using-python3.6
                  jh:using-python3.7)))
  (define python-3s
    (list 'python-3
          (append mf:using-python3
                  jh:using-python3)))
  (define java-8s
    (list 'java-8
          (append mf:using-java8
                  jh:using-java8)))
  (define javas
    (list 'java
          (append mf:using-java
                  jh:using-java)))
  (define gos
    (list 'go
          (append mf:using-go
                  jh:using-go)))
  (define c++s
    (list 'c++/rust
          (append mf:using-c++
                  jh:using-c++
                  mf:using-rust
                  jh:using-rust)))
  (define lols (list javascripts
                     python-3.6s
                     python-3s
                     java-8s
                     javas
                     gos
                     c++s))
  (make-pairings lols out-file))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deposit particular files into the repos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deposit-files repo-dir server-spec tests-dir)
  #;(define server-spec "server-spread.md")
  (define server-dest-file "given-server-spread.md")
  #;(define tests-dir "spread-tests")
  (for ([repo-name (directory-list repo-dir)])
    (define dest-dir (build-path repo-dir repo-name "4"))
    (unless (directory-exists? dest-dir)
      (make-directory dest-dir))
    (unless (directory-exists? (build-path repo-dir repo-name "3"))
      (make-directory (build-path repo-dir repo-name "3")))
    (copy-file server-spec (build-path dest-dir server-dest-file) #t)
    (copy-directory/files tests-dir (build-path repo-dir repo-name "3" tests-dir))
    (parameterize ([current-directory (build-path repo-dir repo-name)])
      (unless
             (and (checkout-repo-to-master!)
                  (system (~a "git add " (build-path "4" server-dest-file)
                              "; "
                              "git add " (build-path "3" tests-dir)
                              "; "
                              "git commit -m 'import files'"
                              "; "
                              "git push origin master")))
           (printf "\n\nError during git-foo for repo ~a\n\n" repo-name)))))

(define (fix-spec repo-dir server-spec)
  (define server-dest-file "given-server-spread.md")
  (for ([repo-name (directory-list repo-dir)])
    (define dest-dir (build-path repo-dir repo-name "4"))
    (unless (directory-exists? dest-dir)
      (make-directory dest-dir))
    (unless (directory-exists? (build-path repo-dir repo-name "3"))
      (make-directory (build-path repo-dir repo-name "3")))
    (copy-file server-spec (build-path dest-dir server-dest-file) #t)
    (parameterize ([current-directory (build-path repo-dir repo-name)])
      (unless
             (and (checkout-repo-to-master!)
                  (system (~a "git add " (build-path "4" server-dest-file)
                              "; "
                              "git commit -m 'FIX SERVER SPEC'"
                              "; "
                              "git push origin master")))
           (printf "\n\nError during git-foo for repo ~a\n\n" repo-name)))))

(define (deposit-client-tests repo-dir tests-dir)
  (define dest-dir "given-client-tests")
  (for ([repo-name (directory-list repo-dir)])
    (define 4dir (build-path repo-dir repo-name "4"))
    (unless (directory-exists? 4dir)
      (make-directory dest-dir))
    (define dest (build-path 4dir dest-dir))
    (cond
      [(directory-exists? dest)
       (printf "skipping copy to repo ~a, directory exists\n" repo-name)]
      [else
       (copy-directory/files tests-dir dest)])
    (parameterize ([current-directory (build-path repo-dir repo-name)])
      (unless
             (and (checkout-repo-to-master!)
                  (system (~a "git add " (build-path "4" dest-dir)
                              "; "
                              "git commit -m 'import tests'"
                              "; "
                              "git push origin master")))
           (printf "\n\nError during git-foo for repo ~a\n\n" repo-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main)
  (match (current-command-line-arguments)
    ;; Command Line Argument: File with pair data
    [(vector "shuffle" (? path-string? f))
     (perform-shuffling f)]
    [(vector "pair" (? path-string? f))
     (make-pairs-from-actual-teams f)]
    [(vector "return" (? path-string? f))
     (define lang-pairs (read-pairings f))
     (define team-pairs (apply append (map second lang-pairs)))
     (return-impls team-pairs)
     #f]
    ;; USAGE
    ;; repo-dir is a path to the directory containing (only) the repositories to transfer the files to
    ;; spec-file in current directory
    ;; tests-dir in current directory
    [(vector "deposit" (? path-string? repo-dir) (? path-string? spec-file) (? path-string? tests-dir))
     (deposit-files repo-dir spec-file tests-dir)]
    [(vector "fix-spec" (? path-string? repo-dir) (? path-string? spec-file))
     (fix-spec repo-dir spec-file)]
    [(vector "client-tests" (? path-string? repo-dir) (? path-string? tests-dir))
     (deposit-client-tests repo-dir tests-dir)]
    [_ #f]))

(module+ main
  (main))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define SC
  (person "Caldwell" "Sam" #f #f "samc"))
(define MF
  (person "Felleisen" "Matthias" #f #f "matthias"))
(define BG
  (person "Greenman" "Ben" #f #f "types"))

(define tt1
  (team SC MF 3199))
(define tt2
  (team MF BG 3200))
(define tt3
  (team SC BG 3201))
(define test-teams
  (list tt1 tt2 tt3))

(define test-pythons
  (list "python" (list tt1 tt2 tt3)))
