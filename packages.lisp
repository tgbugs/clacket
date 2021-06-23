(in-package :cl-user)
(defpackage :clacket
  (:use :cl)
  (:nicknames :rkt)
  (:export
   #:define
   #:read-rkt
   #:read-rkt-file
   #:enable-clacket))
