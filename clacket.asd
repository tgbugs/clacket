(in-package :cl-user)

(defpackage :clacket-asd
  (:use :cl :asdf))

(in-package :clacket-asd)

(defsystem :clacket
  :version "0.0.1"
  :author "Tom Gillespie <tgbugs@gmail.com>"
  :license "Public Domain (Unlicense)"
  :description "Read racket programs in common lisp"
  :serial t  ; needed so that packages loads before the others
  :depends-on (:datum-comments)
  :components ((:file "packages")
               (:file "read")
               (:file "enable"))
  :in-order-to ((test-op (test-op :clacket/test))))

(defsystem :clacket/test
  :depends-on (:clacket)
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "tests"))))
  :perform (test-op (o c) (uiop:symbol-call :clacket/test :run-all-tests)))
