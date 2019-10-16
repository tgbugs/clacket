(in-package :cl-user)
(defpackage :racket-reader
  (:use :cl)
  (:nicknames :rkt)
  (:export
   #:define
   #:read-rkt
   #:read-rkt-file))
