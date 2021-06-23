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
  (read-line stream t nil t)
  ; FIXME technically this becomes the module reader
  (values))

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

#+()
(in-package :clacket)

#+()
(in-package :cl-user)

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

(defun read-quasisyntax (s c a)
  (declare (ignore a))
  ; FIXME not quite right need to swap quote with syntax
  `(|quasisyntax| ,(funcall (get-macro-character #\`) s c)))

(defun read-box (s c a)
  (declare (ignore c a))
  `(|box| ,(read s))

  )

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
  ;;(format nil "error: ~a" condition)
  #+()
  (defpackage #'thing)
  #+()
  (invoke-restart 'unintern)
  #+()
  (invoke-restart 'ccl::make-nickname)

  (let* ((package-name (slot-value condition 'package))
         ;;#+()
         (package-keyword (when package-name (intern package-name 'keyword))))
    #+() ; so this doesn't work ... so we have to do something evil
    (CCL::%DEFPACKAGE package-name '(:USE :CL))
    (if package-name
        (progn
          (eval `(defpackage ,package-keyword (:use :cl)))
          (invoke-restart 'use-value package-name))
        (signal 'error)
        )

    ;;(signal 'stahp)
    ;;(invoke-restart 'continue)
    ;; XXX ARGH it already read the whole name by this point >_<
    ;;(invoke-restart 'ccl-unintern package-name)
    ;;(invoke-restart 'use-value package-keyword)

    ;; yeah .. this stack overflow is pretty gnarley crashes ccl entirely

    )
  )

(defun asdfasdf (condition)
  (let* ((fa (slot-value condition 'ccl::format-arguments))
         (fc (slot-value condition 'ccl::format-control))
         (name (car fa))
         (package (cadr fa)))
    ; TODO if string prefix Reader error:
    ; illegal symbol error will hit us with symbol: because the symbol name is empty
    (cond ((or (string= fc "Reader error: Illegal symbol syntax.")
               (string= (subseq fc 0 4) "oops"))
           ;;(invoke-restart 'cl-user::please-continue)
           (signal 'sigh) ; so the ccl error has no way to recover here because nothing is included in the error
           #+()
           (make-symbol (concatenate 'string (package-name package) ":")))
          ((packagep package)
           (make-symbol (concatenate 'string (package-name package) ":" (if name name "")))
           ;; FIXME return value?
           (invoke-restart 'continue))
          (t (format t "asdfasdf package: ~s ~s~%" package condition) (signal 'sigh))
          )))

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
         (simple-error #'asdfasdf #+()(lambda (c) c (signal 'error)))
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
        #+() ; for debug
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

#+() ; sbcl
(defun sb-impl::read-after-dot (stream firstchar collectp)
  ;; FIRSTCHAR is non-whitespace!
  (let ((lastobj ()))
    (do ((char firstchar (sb-impl::flush-whitespace stream)))
        ((eq char #\))
         (if (zerop collectp)
             (return-from sb-impl::read-after-dot nil)
             (sb-impl::simple-reader-error stream "Nothing appears after . in list.")))
      ;; See whether there's something there.
      (multiple-value-bind (winp obj) (sb-impl::read-maybe-nothing stream char)
        (unless (zerop winp) (return (setq lastobj obj)))))
    ;; At least one thing appears after the dot.
    ;; Check for more than one thing following dot.
    (loop
     (let ((char (sb-impl::flush-whitespace stream)))
       (cond ((eq char #\)) (return lastobj)) ;success!
             ;; Try reading virtual whitespace.
             ((eq firstchar #\.) (return lastobj)) ; racket elip helper XXX totally broken
             ((not (zerop (logand (sb-impl::read-maybe-nothing stream char)
                                  (sb-impl::truly-the fixnum collectp))))
              (sb-impl::simple-reader-error
               stream "More than one object follows . in list.")))))))

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

;; enable
(defun enable-clacket ()
  (setf (readtable-case *readtable*) :preserve) ; FIXME it seems that this does not stick in sbcl?
  ;; #lang -> (values)
  (set-dispatch-macro-character #\# #\l #'racket-lang-line)
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
  ;; #' -> '
  (set-dispatch-macro-character #\# #\' #'read-syntax)
  ;; #` -> `
  (set-dispatch-macro-character #\# #\` #'read-quasisyntax)
  ;; #: -> :
  (set-dispatch-macro-character #\# #\: (lambda (s c a) (declare (ignore c a)) (intern (symbol-name (read s)) 'keyword)))
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
