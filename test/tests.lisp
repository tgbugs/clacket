(in-package :racket-reader-test)

#lang not actually racket

#; (pprint "this will not print") (defvar *test-datum-comments* "yes datum comments has been imported")

(define *test-value* "looks like define->defvar works")

(unless (eql #t t) (error "#t not t"))
(unless (eql #f nil) (error "#f not nil"))

'[don't you love square brackets?]

#'(read me some syntax boys)

(define *six* [+ 1 2 3])
(define *seven* {- 10 3})

'(package:names break:things)

(defun run-all-tests ()
  (princ "running tests, but if you got here then this was a success")
  (princ (format nil "~%~%got test value ~s~%" *test-datum-comments*))
  (princ (format nil "~%got test value ~s~%" *test-value*))
  (princ (format nil "~%got test value ~s~%" *six*))
  (princ (format nil "~%got test value ~s~%" *seven*))
  t)
