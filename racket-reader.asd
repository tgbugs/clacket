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
               (:file "enable")))

(defsystem :clacket-test
  :depends-on (:clacket)
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "tests")))))

;; tell the system where to find
(defmethod perform ((o test-op) (c (eql (find-system :clacket))))
  (operate 'load-op :clacket-test)
  (funcall (intern (symbol-name :run-all-tests) (find-package :clacket-test))))
