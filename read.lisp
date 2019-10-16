(in-package :clacket)

;; lang line
(defun racket-lang-line (stream char arg)
  (declare (ignore arg))
  #+()
  (invoke-debugger (make-condition 'simple-error))
  (let* ((l char)
         (a (read-char stream))
         (n (read-char stream))
         (g (read-char stream))
         (space (read-char stream)))
    (when (not (and (eql l #\L)  ; bloody cl reader
                    (eql a #\a)
                    (eql n #\n)
                    (eql g #\g)
                    (eql space #\space)))
      (error (format nil "this macro should always be #lang ? but is ~a instead"
                     (uiop:strcat l a n g space)))))
  (read-line stream t nil t)
  (values))

;; wow ... not kidding it is easy
;; https://stackoverflow.com/questions/1988/how-far-can-lisp-macros-go
(defun right-square-bracket-reader (s c)
  (declare (ignore c))
  (read-delimited-list #\] s t))

(defun right-curly-bracket-reader (s c)
  (declare (ignore c))
  (read-delimited-list #\} s t))

;; syntax
(defun read-syntax (s c a)
  (declare (ignore a))
  (funcall (get-macro-character #\') s c))

;; handle packages
(defun handle-rkt-pkg (condition)
  (format nil "error: ~a" condition)
  (invoke-restart 'unintern))

;; reader
(defun read-rkt (stream)
  (handler-bind
      #+sbcl
      ((sb-int:simple-reader-package-error #'handle-rkt-pkg))
      #+ecl
      ((simple-error #'(lambda (c) (format nil "I have no idea: ~a" c))))
      (read stream nil)))

;;; XXX TODO
(defun read-rkt-all (ARGS TODO)
    (handler-bind
      #+sbcl
      ((sb-int:simple-reader-package-error #'handle-rkt-pkg))
      #+ecl
      ((simple-error #'(lambda (c) (format nil "I have no idea: ~a" c))))
      (read stream ARGS TODO)))

(defun read-rkt-file (p)
  (with-open-file (in p)
    :external-format :utf-8
    (do ((expr (read-rkt in)
               (read-rkt in)))
        ((null expr))
      (pprint expr)
      (case (car expr)
        (define (let ((next (cadr expr)))
                  (if (listp next)
                      (print "TODO defun ...")
                      (progn
                        (eval expr)))))
        (otherwise (values))))))

;; enable
(defun enable-racket-reader ()
  ;; #lang -> (values)
  (set-dispatch-macro-character #\# #\l #'racket-lang-line)
  ;; [] -> ()
  (set-macro-character #\[ #'right-square-bracket-reader)
  (set-syntax-from-char #\] #\))
  ;; {} -> ()
  (set-macro-character #\{ #'right-curly-bracket-reader)
  (set-syntax-from-char #\} #\))
  ;; #' -> '
  (set-dispatch-macro-character #\# #\' #'read-syntax)
  ;; #t -> t
  (set-dispatch-macro-character #\# #\t (lambda (s c a) (declare (ignore s c a)) t))
  ;; #f -> nil
  (set-dispatch-macro-character #\# #\f (lambda (s c a) (declare (ignore s c a)) nil))
  ;; define -> defvar
  (setf (macro-function 'define) (macro-function 'defvar))
  ;; EVIL wrapping of read ... (not ready)
  #+()
  (setf (symbol-function 'read) #'read-rkt-all))
