#lang cs4500-f18-fest/manifest

(define harness-exe-path (find-system-path 'pref-file))
(define staff-exe-path (find-system-path 'pref-file))
(define staff-tests-path (find-system-path 'temp-dir))
(define student-exe-name "./main")
(define student-test-name "./the/tests")
(define student-root (find-system-path 'temp-dir))
(define student-deadline "2018-10-01T01:00:00")
(define team-name '(aaaa-bbbb cccc-dddd eeee-ffff))

(define assignment-name 420)
