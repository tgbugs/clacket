(in-package :cl-user)

(defpackage :racket-reader-asd
  (:use :cl :asdf))

(in-package :racket-reader-asd)

(defsystem :racket-reader
  :version "0.0.1"
  :author "Tom Gillespie <tgbugs@gmail.com>"
  :license "Public Domain (Unlicense)"
  :description "Read racket programs in common lisp"
  :serial t  ; needed so that packages loads before the others
  :depends-on (:datum-comments)
  :components ((:file "packages")
               (:file "read")
               (:file "enable")))

(defsystem :racket-reader-test
  :depends-on (:racket-reader)
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "tests")))))

;; tell the system where to find
(defmethod perform ((o test-op) (c (eql (find-system :racket-reader))))
  (operate 'load-op :racket-reader-test)
  (funcall (intern (symbol-name :run-all-tests) (find-package :racket-reader-test))))
