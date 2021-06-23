(in-package :clacket/test)

#lang not actually racket

#; (pprint "this will not print") (defvar *test-datum-comments* "yes datum comments has been imported")

#;
(not-a-package-name:why 'does-this-fail)

(define *test-value* "looks like define->defvar works")

(unless (eql #t t) (error "#t not t"))
(unless (eql #f nil) (error "#f not nil"))

'[don't you love square brackets?]

#'(read me some syntax boys)

(define *six* [+ 1 2 3])
(define *seven* {- 10 3})

#+()
;; the cl reader looks for the package package: and can't find it
;; and for whatever reason the datum comment doesn't comment it out
;; in time to protect from the issue
'(package:names break:things)

(defun run-all-tests ()
  (princ "running tests, but if you got here then this was a success")
  (princ (format nil "~%~%got test value ~s~%" *test-datum-comments*))
  (princ (format nil "~%got test value ~s~%" *test-value*))
  (princ (format nil "~%got test value ~s~%" *six*))
  (princ (format nil "~%got test value ~s~%" *seven*))
  t)
