(in-package :clacket)

;; lang line
(defun racket-lang-line (stream char arg)
  ; can't just read an reset, but can peak I think?
  (declare (ignore arg))
  #+()
  (invoke-debugger (make-condition 'simple-error))
  (let* ((l char)
         (a (read-char stream))
         (n (read-char stream))
         (g (read-char stream))
         (space (read-char stream)))
    ;(format t "char: ~s~%" char)
    (when (not (and
                #+(or sbcl ccl)
                (eql l #\L) ; I think there is a bug in SBCL where setting readtable-case fails?
                #-(or sbcl ccl)
                (eql l #\l)  ; use this reader case preserving? except that somehow still #\L in ccl?
                    (eql a #\a)
                    (eql n #\n)
                    (eql g #\g)
                    (eql space #\space)))
      (error (format nil "this macro should always be #lang ? but is ~a instead"
                     (uiop:strcat l a n g space)))))
  ; FIXME technically this becomes the module reader
  ; TODO should also set the package that we intern into probably since modules are conflated with files
  (let* ((ll (read-line stream t nil t))
         (is (make-string-input-stream ll))
         (lr (symbol-name (read is)))
         (losing-it (string= lr "at-exp")))
    #+()
    (format t "lang-line-value: ~s ~s ~s~%" ll lr losing-it #+()(string= lr "at-exp"))
    (if losing-it ;(string= lr "at-exp") ; for reasons of symbol intering I link just (read is) won't work
        (progn
          #+()
          (format t "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA~%")
        ;; FIXME apparently if a signal is not caught stuff just keeps running ? kind of owrrying
               ;; NOTE use error instead of signal so that it will fail if there is nothing listening for the signal?
          (error 'lat))
        (values))))

(defun racket-dis-h (stream char arg)
  (declare (ignore arg))
  #+()
  (invoke-debugger (make-condition 'simple-error))
  (let ((hash-type (let* ((h char)
                          (a (read-char stream))
                          (s (read-char stream))
                          (h2 (read-char stream))
                          (e (peek-char nil stream)))
                                        ;(format t "char: ~s~%" char)
                     (when (not (and
                                 #+(or sbcl ccl)
                                 (eql h #\H) ; I think there is a bug in SBCL where setting readtable-case fails?
                                 #-(or sbcl ccl)
                                 (eql h #\h)  ; use this reader case preserving? except that somehow still #\L in ccl?
                                 (eql a #\a)
                                 (eql s #\s)
                                 (eql h2 #\h)))
                       (error (format nil "this macro should always be #hash ? but is ~a instead"
                                      (uiop:strcat h a s h2))))
                     (if (eq e #\e)
                         (let* ((e (read-char stream))
                                (q (read-char stream))
                                (v (peek-char nil stream)))
                           (when (not (eq q #\q))
                             (error (format nil "q is not #\\q it is ~a instead" q)))
                           (if (eq v #\v)
                               (let ((v (read-char stream)))
                                 '|#%hasheqv|
                                 )
                               '|#%hasheq|))
                         '|#%hash|))))
    ;; technically incorrect since #hash () isn't currently allowed
    ;; FIXME this should somehow convert to reading a hash table
    (list hash-type (read stream))))

(defun racket-dis-p (stream char arg)
  (declare (ignore arg))
  (let* ((p char)
         (x (peek-char nil stream)))
    (when (not (and
                #+(or sbcl ccl)
                (eql p #\P) ; upcasing reader?
                #-(or sbcl ccl)
                (eql p #\p) ; upcasing reader?
                    (eql x #\x)))
      (error (format nil "oops ~s~%" x))))
  (read-char stream) ; eat the x
  ;; sort out the error handling later, racket uses a contract, also #px#"byte string" is a thing
  `(pregexp ,(read stream)))

(defun racket-dis-r (stream char arg)
  (declare (ignore arg))
  (let* ((r char)
         (x (peek-char nil stream)))
    (when (not (and
                #+(or sbcl ccl)
                (eql r #\R) ; upcasing reader
                #-(or sbcl ccl)
                (eql r #\r) ; upcasing reader
                    (eql x #\x)))
      (error (format nil "oops ~s~%" x))))
  (read-char stream) ; eat the x
  ;; sort out the error handling later, racket uses a contract, also #px#"byte string" is a thing
  `(regexp ,(read stream)))

;; wow ... not kidding it is easy
;; https://stackoverflow.com/questions/1988/how-far-can-lisp-macros-go
(defun right-square-bracket-reader (s c)
  (declare (ignore c))
  ;;(format t "really we are here already")
  #-ecl
  (read-delimited-list #\] s t)
  #+ecl
  (let ((out (read-delimited-list #\] s t)))
    (format t "ecl wat: ~s~%" out)
    out))

(defun right-curly-bracket-reader (s c)
  (declare (ignore c))
  (read-delimited-list #\} s t))

(defstruct syntax-object
  lexical-information ; scope-sets
  source-location
  (tamper-status nil)
  (properties '()))

;; syntax
(defun read-syntax (s c a)
  (declare (ignore a))
  ; FIXME not quite right need to swap quote with syntax
  `(|syntax| ,(funcall (get-macro-character #\') s c)))

(defparameter *syntax-stack* '()) ; see backquote.lisp `ccl::*backquote-stack*'

(defun read-quasisyntax (s c a)
  (declare (ignore a))
  ; FIXME not quite right need to swap quote with syntax
  `(|quasisyntax| ,
    (funcall (get-macro-character #\`) s c)
    #+() ; FIXME this works but interactions between syntax and quote are a problem
    (let ((ccl::*backquote-stack* *syntax-stack*))
      (funcall (get-macro-character #\`) s c))))

(defun read-unsyntax (s c a) ; also reads unsyntax splicing
  (declare (ignore a))
  ; FIXME not quite right need to swap quote with syntax
  (let* ((at? (peek-char nil s))
         (name (if (eq at? #\@) ; in ccl splicing is handled as part of |, reader|
                   '|unsyntax-splicing|
                   '|unsyntax|)))
    (list name
          (funcall (get-macro-character #\,) s c)
          #+()
          (let ((ccl::*backquote-stack* *syntax-stack*))
            (funcall (get-macro-character #\,) s c)))))

(defun read-box (s c a)
  (declare (ignore c a))
  `(|box| ,(read s)))

(defun read-bytes (s c a)
  ; FIXME this is totally incorrect
  (declare (ignore a))
  `(|bytes| ,(ccl::read-string s c)))

(defun read-exact (s c a)
  ; FIXME FIXME obviously this is incorrect
  (declare (ignore c a))
  `(|exact| ,(read s)))

(defun read-inexact (s c a)
  ; FIXME FIXME obviously this is incorrect
  (declare (ignore c a))
  `(|inexact| ,(read s)))

;; comments
(defun datum-comment (stream char arg)
  (declare (ignore char arg))
  (let ((*read-suppress* t))
    (read-preserving-whitespace stream t nil t))
  (values))

;; handle packages XXX TODO check cloture for how it deals with this
#+sbcl
(defun handle-rkt-pkg (condition)
  (format nil "error: ~a" condition)
  (invoke-restart 'unintern))

#+ccl
(defun handle-rkt-pkg (condition)
  (let* ((package-name (slot-value condition 'package))
         (package-keyword (when package-name (intern package-name 'keyword))))
    (if package-name
        (progn
          (eval `(defpackage ,package-keyword (:use :cl))) ; XXX evil
          (invoke-restart 'use-value package-name))
        (signal 'error))))

(defun handle-simple-error (condition)
  (let* ((fa (slot-value condition 'ccl::format-arguments))
         (fc (slot-value condition 'ccl::format-control))
         (name (car fa))
         (package (cadr fa)))
    ; TODO if string prefix Reader error:
    ; illegal symbol error will hit us with symbol: because the symbol name is empty
    (cond ((string= fc "Reader error: Illegal symbol syntax.")
           ;;(invoke-restart 'cl-user::please-continue)
           (signal 'iss) ; so the ccl error has no way to recover here because nothing is included in the error
           #+()
           (make-symbol (concatenate 'string (package-name package) ":")))
          ((string= (subseq fc 0 4) "oops") (signal 'sigh))
          ((string= (subseq fc (- (length fc) 9) (length fc)) "backquote") (signal 'cnib))
          ((packagep package)
           (make-symbol (concatenate 'string (package-name package) ":" (if name name "")))
           ;; FIXME return value?
           (invoke-restart 'continue))
          (t #+()(format t "handle-simple-error package: ~s ~s~%" package condition) (signal 'sigh)))))

(define-condition lat (condition) ())
(define-condition iss (condition) ())
(define-condition cnib (condition) ())
(define-condition sigh (condition) ())
(define-condition yeah-yeah-yeah (error) ())

;; reader
(defun read-rkt (stream &optional (eof-value nil))
  (restart-case
      (handler-bind
          #+sbcl
        ((sb-int:simple-reader-package-error #'handle-rkt-pkg))
        #+ecl
        ((simple-error (lambda (c) (format nil "I have no idea: ~a" c) (values))))
        #+ccl
        ( ; this doesn't work due to an interposing handler in ccl
         (ccl::no-such-package #'handle-rkt-pkg)
         (ccl::simple-reader-error (lambda (c) c (signal 'yeah-yeah-yeah)))
         (ccl::cant-construct-arglist (lambda (c) ; probably lang-at exp which we can handle w/ another readable but only once we implement the readline
                                        c (signal 'sigh)))
         (simple-error #'handle-simple-error #+()(lambda (c) c (signal 'error)))
         ;;(ccl::inactive-restart ())
         ;;(ccl::simple-reader-error (lambda (c) (throw c)))
         #+()
         (simple-error (lambda (c) (declare (ignore c))
                         ;; FIXME this is a dirty hack with too many false positives
                         ;; after using cl-user it won't find a symbol so we have to intern it
                         (invoke-restart 'ccl::continue)))
         )
                                        ;((ccl::simple-program-error #'handle-rkt-pkg))
        (read stream nil eof-value))
    #+ccl
    (ccl-unintern (&optional value) (format t "ccl-value: ~s~%" value)
      (let ((v (read stream nil eof-value))) ; not quite right could have name ending in colon
        (make-symbol (concatenate 'string value ":" (symbol-name v)))))
    #+ccl
    (ccl-wat (&optional value) (format t "ccl-value: ~s~%" value))
    ))

;;; XXX TODO
(defun read-rkt-all (&optional (stream *standard-input*)
                               (eof-error-p t)
                               (eof-value nil)
                               (recursive-p nil))
  (handler-bind
      #+sbcl
      ((sb-int:simple-reader-package-error #'handle-rkt-pkg))
      #+ecl
      ((simple-error (lambda (c) (format nil "I have no idea: ~a" c))))
      #+ccl
      ((ccl::no-such-package #'handle-rkt-pkg))
    (read stream eof-error-p eof-value recursive-p)))

(defun read-rkt-file (p)
  (with-open-file (in p)
    :external-format :utf-8
    #+()
    (let ((eof-sym (gensym)))
      (loop for expr = (read-rkt in eof-sym)
            do (pprint expr)
            collect expr into exprs
            until (eq expr eof-sym)
            return exprs))
    (let ((out nil) (eof-sym (gensym))) ; FIXME ICK
      (do ((expr (read-rkt in eof-sym)
                 (read-rkt in eof-sym)))
          ((eq expr eof-sym))
        (setf out (cons expr out))
        #+debug ; for debug
        (progn
          (pprint expr)
          (format t "~%"))
        #+()
        (case (car expr)
          (define (let ((next (cadr expr)))
                    (if (listp next)
                        (print "TODO defun ...")
                        (progn
                          (eval expr)))))
          (otherwise (values))))
      (reverse out))))

;; TODO named-readtables
#|
(defreadtable clacket
  ;(:case :preserve)
  (:dispatch-macro-char #\# #\l #'racket-lang-line)
  (:dispatch-macro-char #\# #\t (lambda (s c a) (declare (ignore s c a)) t))  ; FIXME
  (:dispatch-macro-char #\# #\t (lambda (s c a) (declare (ignore s c a)) nil))  ; FIXME
  (:syntax-from :standard #\( #\[)
  (:syntax-from :standard #\) #\])
  (:syntax-from :standard #\( #\{)
  (:syntax-from :standard #\) #\})
  )

(defmacro with-racket-reader ((&key) &body body)
  (with-thunk (body)
    `(call/racket-reader ,body)))

(defun call/racket-reader (function)
  (let ((*readtable* (find-readtable 'clacket)))
    (funcall fn)))
(defun read-racket (stream
                    &key (eof-error-p t)
                         eof-value
                         recursive)
  (with-racket-reader ()
    (read stream eof-error-p eof-value recursive)))
|#

;;; XXX FIXME yeah ... so we basically have to reimplement all of read-list

(defvar |...| 'elipsis-zero-or-more)

(defvar |...+| 'elipsis-one-or-more)

(defun read-elip (s c)
  ;; FIXME this fails because there isn't a way to override the list reader so it won't gobble the first dot >_<
  ;; so if you hit a space after elip like (a ... b) you are toast
  (let ((out (list (string c))))
    #+()
    (format t "START: ~s ~s~%" (file-position s) out)
    (do ((nc nil ;(peek-char nil s)
             (peek-char nil s)))
        ((not (or (eq nc #\.) (eq nc #\+) (not nc)))
         #+()
         (format t "EXIT: ~s ~s~%" (file-position s) nc))
      (when nc
        (setf out (cons (string (read-char s)) out)))
      #+()
      (format t "SIGH: ~s ~s ~s~%" (file-position s) nc out))
    (let ((out (intern (apply #'concatenate 'string out))))
      #+()
      (format t "OUT: ~s ~s~%" (file-position s) out)
      out))
  #+()
  (let ((nc (peek-char t s)))
    (if (eq nc #\.)
        (concatenate 'string (string (read-char s)) (string c))
        (string c))
    ))

(defun read-symbol (s c a)
  (declare (ignore a))
  (intern
   (concatenate 'string "#" (string c) (symbol-name (read s)))))

(defun read-kw (s c a)
  (declare (ignore c a))
  (let* ((v (read s))
         (rv (if (numberp v)
                 (make-symbol (format nil "~s" v)) ; FIXME a horrible hack ...
                 v)))
    (intern (symbol-name rv) 'keyword)))

(defun read-struct (s c a)
  (declare (ignore c a))
  `(|#%struct| ,(read s)))

;; enable
(defun enable-clacket ()
  ;; case
  (setf (readtable-case *readtable*) :preserve) ; FIXME it seems that this does not stick in sbcl?

  ;; chars
  (ccl::register-character-name "vtab" #\PageUp) ; apparently this is #\u0B aka #\u+0B aka line tabulation

  ;; #lang -> (values)
  (set-dispatch-macro-character #\# #\l #'racket-lang-line)
  ;; #hash -> ?????????????
  (set-dispatch-macro-character #\# #\h #'racket-dis-h)
  ;; [] -> ()
  (set-macro-character #\[ #'right-square-bracket-reader)
  (set-syntax-from-char #\] #\))
  ;; {} -> ()
  (set-macro-character #\{ #'right-curly-bracket-reader)
  (set-syntax-from-char #\} #\))
  ;; : -> hrm FIXME this totally does not work in ccl due to the way that package names are parsed
  ;;(set-macro-character #\: (lambda (s c) (declare (ignore c)) (intern (symbol-name (read s)) 'keyword)) t)
  ;; ...
  #+() ; bad
  (set-macro-character #\. (lambda (s c) (intern (concatenate 'string (string c) (sb-impl::read-token s c)))))
  (set-macro-character #\. #'read-elip) ; FIXME doesn't seem to catch does in lists?
  ;; #; -> #+() effectively
  (set-dispatch-macro-character #\# #\; #'datum-comment)
  ;; #" -> bytes
  (set-dispatch-macro-character #\# #\" #'read-bytes)
  ;; #' -> '
  (set-dispatch-macro-character #\# #\' #'read-syntax)
  ;; #` -> `
  (set-dispatch-macro-character #\# #\` #'read-quasisyntax)
  ;; #, -> ,
  (set-dispatch-macro-character #\# #\, #'read-unsyntax)
  ;; #: -> :
  (set-dispatch-macro-character #\# #\: #'read-kw)
  ;; #t -> t
  (set-dispatch-macro-character #\# #\t (lambda (s c a) (declare (ignore s c a)) t))
  ;; #f -> nil
  (set-dispatch-macro-character #\# #\f (lambda (s c a) (declare (ignore s c a)) nil))
  ;; #& -> box
  (set-dispatch-macro-character #\# #\& #'read-box)
  ;; #+ -> symbol
  (set-dispatch-macro-character #\# #\+ #'read-symbol)
  ;; #- -> symbol
  (set-dispatch-macro-character #\# #\- #'read-symbol)
  ;; #% -> symbol
  (set-dispatch-macro-character #\# #\% #'read-symbol)
  ;; #e -> exact
  (set-dispatch-macro-character #\# #\e #'read-exact)
  ;; #i -> inexact
  (set-dispatch-macro-character #\# #\i #'read-inexact)
  ;; #s -> struct
  (set-dispatch-macro-character #\# #\s #'read-struct)

  (set-dispatch-macro-character #\# #\[ (get-dispatch-macro-character #\# #\()) ; FIXME not right? I mean technically yes ...
  (set-dispatch-macro-character #\# #\{ (get-dispatch-macro-character #\# #\()) ; FIXME not right? I mean technically yes ...
  (set-dispatch-macro-character #\# #\p #'racket-dis-p)
  (set-dispatch-macro-character #\# #\r #'racket-dis-r)
  ;; define -> defvar
  (setf (macro-function 'define) (macro-function 'defvar))
  ;; EVIL wrapping of read ... (not ready)
  #+()
  (setf (symbol-function 'read) #'read-rkt-all)
  (values))

