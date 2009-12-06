(defpackage #:nispbot-tests
  (:use :common-lisp
        :fiveam
        :nispbot)
  (:shadowing-import-from :irc pass))
(in-package :FiveAM)

(defmacro def-fixture (name args &body body)
  "Defines a fixture named NAME. A fixture is very much like a
macro but is used only for simple templating. A fixture created
with DEF-FIXTURE is a macro which can use the special macrolet
&BODY to specify where the body should go.

See Also: WITH-FIXTURE

*note* that this overrides the default 5am DEF-FIXTURE. 5am's
DEF-FIXTURE evals itself too much causing a SBCL compiler warning
about redefining a symbol."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rem-fixture ',name)
     (setf (get-fixture ',name) (cons ',args ',body))
     ',name))

(in-package :nispbot-tests)
(setf 5am:*debug-on-error* nil)
(setf 5am:*debug-on-failure* nil)

(def-suite all-tests
    :description "Top level test suite")

(def-suite basic-irc
    :in all-tests
    :description "Tests all the irc related stuff")

(in-suite basic-irc)

(test (make-irc-message)
   "Testing usage of `cl-irc:make-irc-message'"
   (is (string=
        (format nil "privmsg #channel :text~%")
        (irc::make-irc-message "privmsg" "#channel" "text"))
       "Simplest case of a private message to a channel"))

(test (tokenize-string)
  "Demonstrate irc::tokenize-string"
  (is (equal (irc::tokenize-string
          "Some string to test on"
          :delimiters '(#\Space))
         '("Some" "string" "to" "test" "on"))))

(test parse-bot-command
  (is-every string=
    ((nispbot::parse-bot-command ",arglist +")
     "arglist")
    ((multiple-value-bind (ignore arg) (nispbot::parse-bot-command ",arglist xyz")
       arg)
     "xyz")))

(test (function-lambda-list-to-string :depends-on parse-bot-command)
  "Get a valid arglist with no errors."
  (is
   (string=
    (nispbot::function-lambda-list-to-string "+")
    "(&REST ARGS)"))
  (5am:finishes
    (nispbot::function-lambda-list-to-string "1")
    "Do better then throwing an error on non-function objects"))


(def-suite config-tests :in all-tests)
(in-suite config-tests)

(test *channel*-is-a-string
  "Make sure that we don't change the type without letting users know."
  (is (stringp nispbot-config::*channel*)))


(def-suite error-handling-tests :in all-tests)
(in-suite error-handling-tests)