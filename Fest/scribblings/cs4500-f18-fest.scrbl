#lang scribble/manual

@require[
  scribble/example
  (for-label
    (only-in gregor)
    racket/base
    racket/contract
    cs4500-f18-fest/config
    cs4500-f18-fest)]

@(define (make-config-eval) (make-base-eval '(require cs4500-f18-fest/config)))

@; =============================================================================

@title{CS4500 F18 Test Fest Harness}

@defmodule[cs4500-f18-fest]{
  Test fest harness
}

@section[#:tag "cs4500-quickstart"]{How to Run a Test Fest}

@itemlist[
  @item{
    Create a @tech{cs4500 manifest} (see @secref{cs4500-manifest})
  }
  @item{
    Run @exec|{PLTSTDERR="error info@cs4500-f18" raco cs4500-f18-fest <FILE>.cs4500}|
    or (from the package top-level directory) @exec|{./xfest <FILE>.cs4500}|.
  }
  @item{
    Once the harness is running, it:
    @itemlist[
      @item{
        inspects student git repositories,
         pulls the @tt{master} branch,
         checks out the latest commit before the deadline,
         and creates a new branch with the same name as the assignment;
      }
      @item{
        creates a new folder for results --- see @secref{cs4500-result} for details;
      }
      @item{
        runs each team's executable on the staff tests;
      }
      @item{
        validates each team's first @racket[MAX-NUM-TESTS] against the staff
        executable;
      }
      @item{
        and runs all valid student executables on all valid student tests.
      }
    ]
  }
]

If a step does not go well, the harness may
 (1) print a message to the console and
 (2) wait for a user to enter a newline.
After a newline the harness may retry the previous step, but more likely it
 will carry on assuming the user double-checked whatever the harness was
 concerned about.

If the harness raises an exception, then it probably has a bug.

Feel free to kill the harness in the middle of running.
When restarted (from the beginning), the harness checks the student git
 repos and the results folder to see what work is left.
If you want something re-done, then delete or move the thing.


@section[#:tag "cs4500-manifest"]{CS4500 Manifest}

@defmodulelang[cs4500-f18-fest/manifest]{
  A @deftech{cs4500 manifest} defines the parameters to a test fest.
  Running a manifest checks that all files/directories in the manifest exist.
}

Example:
@codeblock{
#lang cs4500-f18-fest/manifest

(define harness-exe-path "/home/cs4500/staff/harness/xmain")
(define staff-exe-path "/home/cs4500/staff/xcomponent")
(define staff-tests-path "/home/cs4500/staff/xtest")
(define student-exe-name "./1/xmain")
(define student-test-name "./1/component-tests")
(define student-root "/home/cs4500/all-student-repos/")
(define student-deadline "2018-10-01T01:00:00")
(define assignment-name 'week1)
(define max-seconds 20)
(define team-name* '(aaaa-bbbb cccc-dddd eeee-ffff))}

@subsection[#:tag "cs4500-config"]{CS4500 Configuration}

@defmodule[cs4500-f18-fest/config]{
  CS4500 contracts, constants, and helper functions.
}

@defthing[#:kind "flat-contract?" fest-config/c (-> any/c boolean?)]{
  Contract for a @tech{cs4500 manifest} value
  (i.e., the result of evaluating a manifest file).

  Similar to @racket[(hash/dc [k fest-config-key/c] [v (k) (fest-config-value/c k)] #:immutable #true :kind 'flat)],
   except that it can give better error messages.
  (We should probably improve @racket[hash/dc].)
}

@defthing[#:kind "flat-contract?" fest-config-key/c (-> any/c boolean?)
          #:value (or/c 'harness-exe-path
                        'staff-exe-path
                        'staff-tests-path
                        'student-exe-name
                        'student-test-name
                        'student-root
                        'student-deadline
                        'assignment-name
                        'max-seconds
                        'team-name*)]{
  Valid keys for a @tech{cs4500 manifest}.
}

@defthing[#:kind "procedure" fest-config-value/c (-> fest-config-key/c flat-contract?)
          #:value (lambda (k)
                    (case k
                      ((harness-exe-path)
                       complete-path-to-file/c)
                      ((staff-exe-path)
                       complete-path-to-file/c)
                      ((staff-tests-path)
                       complete-path-to-directory/c)
                      ((student-exe-name)
                       (and/c path-string? relative-path?))
                      ((student-test-name)
                       (and/c path-string? relative-path?))
                      ((student-root)
                       complete-path-to-directory/c)
                      ((student-deadline)
                       iso8601-string?)
                      ((assignment-name)
                       symbol?)
                      ((max-seconds)
                       (or/c #f exact-nonnegative-integer?))
                      ((team-name*)
                       (listof symbol?))))]{
  Given a @tech{cs4500 manifest} key, returns a contract for the matching value.
}

@defproc[(complete-path-to-file/c [x any/c]) boolean?]{
  Predicate for an absolute path to a file that exists (@racket[file-exists?]) on the current filesystem.
}

@defproc[(complete-path-to-directory/c [x any/c]) boolean?]{
  Predicate for an absolute path to a directory that exists (@racket[directory-exists?]) on the current filesystem.
}

@defproc[(iso8601-string? [x any/c]) boolean?]{
  Predicate for a string that the @racketmodname[gregor] library can parse.

  @examples[#:eval (make-config-eval)
    (iso8601-string? "2018-10-02T01:00:00")
    (iso8601-string? "October 2, 2018 at 1:00am")]
}

@defproc[(cs4500-manifest-file? [ps path-string?]) boolean?]{
  Checks the @hash-lang[] of the given file.
}

@defproc[(manifest->config [ps (and/c path-string? file-exists?)]) fest-config/c]{
  Parse a configuration from a @tech{cs4500 manifest} file.
}

@defthing[#:kind "value" MAX-NUM-TESTS exact-nonnegative-integer?]{
  Limits the number of tests that a student may submit to the test fest.
}

@defthing[#:kind "value" MAX-EXE-SECONDS exact-nonnegative-integer?]{
  Default time limit for running a student executable on staff tests.
}

@defthing[#:kind "value" MAX-TEST-SECONDS exact-nonnegative-integer?]{
  Default time limit for running the staff executable on student tests.
}

@defthing[#:kind "value" MAX-MB exact-nonnegative-integer?]{
  Memory limit for running staff and/or student executables.
}

@defthing[#:kind "value" MAX-FILE-BYTES exact-nonnegative-integer?]{
  Size limit for student tests.
}

@defthing[#:kind "value" MF.txt path-string? #:value "MF.txt"]{
  Filename for results of staff tests on student exe.
}

@defthing[#:kind "value" AUDIT.txt path-string? #:value "audit.txt"]{
  Filename for results of student tests on staff exe.
}

@defthing[#:kind "value" cs4500-f18-logger logger?]{
  Logs events to the topic @racket['cs4500-f18].
}


@section[#:tag "cs4500-result"]{CS4500 Results Format}

For an assignment named @litchar{A1} where the student root folder is named
 @litchar{all-repos}, the harness saves results to the folder @litchar{all-repos/A1}.

The results folder contains one directory for each team.
When the harness finishes, each team directory contains:
@itemlist[
  @item{
    a file named @racket[MF.txt] with the results of running the student
    executable on the staff tests;
  }
  @item{
    a file named @racket[AUDIT.txt] with the results of validating the student
    tests against the staff executable;
  }
  @item{
    a folder containing valid tests (if any);
  }
  @item{
    and --- if the team submitted a working executable --- one file for each
    team that submitted valid tests (containing the results of running this
    team's executable against that team's tests).
  }
]

