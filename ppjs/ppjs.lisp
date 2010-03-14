(defpackage #:ppjs
  (:use :cl :alexandria :eos))
(in-package :ppjs)

(nisp.i::define-simple-command ppjs
  (let* ((*read-eval* nil))
    (nisp.i::reply (format nil "js: ~A"
                           (remove #\newline
                                   (call-with-js-pprint-table
                                    (lambda ()
                                      (prin1-to-string (read-from-string (nisp.i::remaining-parameters))))))))))
(defvar *recursivep* nil)
(defvar *statementp* nil)
(defvar *function-level* 0)

(defun call-wrap-parens (stream thunk)
  (when *recursivep* (princ #\( stream))
  (let ((*recursivep* t))
    (funcall thunk))
  (when *recursivep* (princ #\) stream)))

(defmacro wrap-parens (stream &body body)
  `(call-wrap-parens ,stream (lambda () ,@body)))

(defun call-wrap-statement (stream thunk)
  (let ((*statementp* t))
    (funcall thunk))
  (unless *recursivep*
    (princ #\; stream)
    (princ #\Newline stream)))

(defmacro wrap-statement (stream &body body)
  `(call-wrap-statement ,stream (lambda () ,@body)))

(defun call-wrap-function (name lambda-list stream thunk)
  (let ((*function-level* (1+ *function-level*)))
    (princ "function " stream)
    (prin1 name stream)
    (princ #\Space stream)
    (pprint-function-lambda-list stream lambda-list)
    (princ #\Space stream)
    (call-wrap-braces stream thunk)))

(defmacro wrap-function (name lambda-list stream &body body)
  `(call-wrap-function ,name ,lambda-list ,stream (lambda () ,@body)))

(defun call-wrap-braces (stream thunk)
  (princ #\{ stream)
  (princ #\newline stream)
  (funcall thunk)
  (princ #\} stream))

(defmacro wrap-braces (stream &body body)
  `(call-wrap-braces ,stream (lambda () ,@body)))

(defun pprint-+ (stream list)
  (wrap-statement stream
    (if (not (null (cdr list)))
        (wrap-parens stream
          (funcall (formatter "~{~S~^ + ~}") stream (cdr list)))
        (princ 0 stream))))

(defun pprint-* (stream list)
  (wrap-statement stream
    (if (not (null (cdr list)))
        (wrap-parens stream
          (funcall (formatter "~{~S~^ * ~}") stream (cdr list)))
        (princ 1 stream))))

(defun pprint-- (stream list)
  (wrap-statement stream
    (case (length (cdr list))
      (1 (princ "-" stream)
         (prin1 (cadr list) stream))
      (0 (error "You cannot subtract with 0 arguments!"))
      (otherwise
       (wrap-parens stream
         (funcall (formatter "~{~S~^ - ~}") stream (cdr list)))))))

(defun pprint-/ (stream list)
  (wrap-statement stream
    (case (length (cdr list))
      (1 (when (and (numberp (cadr list)) (zerop (cadr list)))
           (error 'division-by-zero :operation 'pprint-/
                  :operands (list 1 (cadr list))))
         (princ "1/" stream)
         (prin1 (cadr list) stream))
      (0 (error "You cannot divide by 0 arguments."))
      (otherwise
       (wrap-parens stream
         (funcall (formatter "~{~S~^ / ~}") stream (cdr list)))))))

(defun pprint-defun (stream list)
  (wrap-function (second list) (third list) stream
    (funcall (formatter "~{~A~#[~;return ~:;~]~}") stream
             (cdddr list))))

(defun pprint-function-lambda-list (stream list)
  (if list
      (prin1 list stream)
      (princ "()" stream)))

(defun pprint-symbol (stream symbol)
  (princ (string-downcase (symbol-name symbol)) stream))

(defparameter *js-table*
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'symbol 'pprint-symbol)
    (set-pprint-dispatch '(cons (member +)) 'pprint-+)
    (set-pprint-dispatch '(cons (member -)) 'pprint--)
    (set-pprint-dispatch '(cons (member *)) 'pprint-*)
    (set-pprint-dispatch '(cons (member /)) 'pprint-/)
    (set-pprint-dispatch '(cons (member defun)) 'pprint-defun)
    *print-pprint-dispatch*))
(defun js-pprint-table ()
  *js-table*)
(defun call-with-js-pprint-table (thunk)
  (declare (type function thunk))
  (let ((*print-pprint-dispatch* (js-pprint-table)))
    (funcall thunk)))

(defmacro ppjs (&body body)
  `(call-with-js-pprint-table (lambda () (prin1 ',@body))))

(defmacro ppjsi (&body body)
  `(irc:privmsg nisp.i::*devel-bot* "#bots"
               (format nil "js: ~A // ~A => ~S"
                       (remove #\newline (call-with-js-pprint-table (lambda () (prin1-to-string ',@body))))
                       (prin1-to-string',@body)
                       ,@body)))
