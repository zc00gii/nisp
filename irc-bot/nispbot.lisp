(in-package :cl-user)
(defpackage #:nisp.irc
  (:use :common-lisp))
(in-package :nisp.irc)

(defmacro ct (form type)
  `(let ((x ,form))
     (check-type x ,type (format nil "~S: ~A" ',type (documentation ',type 'type)))))

(deftype nickname-start-character ()
  "Valid character at the start of an IRC nickname."
  ;; These are taken from advice from duckinator on eighthbit.net/offtopic
  ;; Also see rfc 2812 sec: 2.3.1
  '(member #\| #\[ #\] #\` #\^ #\\ #\{ #\} #\a #\b #\c #\d #\e #\f #\g #\h
    #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R
    #\S #\T #\U #\V #\W #\X #\Y #\Z))

(deftype nickname-character ()
  "Valid character after the first character of an IRC nickname."
  ;; Taken from advice from duckinator on eighthbit.net/offtopic
  '(or nickname-start-character
    (member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-)))

(defun nickname-string-p (string)
  (declare (type string string))
  (and (typep (char string 0) 'nickname-start-character)
       (every 
        (lambda (x)
          (declare (type character x))
          (typep x 'nickname-character))
        (subseq string 1))))

(deftype nickname-string (&optional size)
  `(and (string ,size)
        (satisfies nickname-string-p)))
(deftype channel-start-character ()
  "Valid starting prefix of a channel name.

On most IRC networks # indicates a normal channel."
  ;; rfc2821 sec 1.3
  '(member #\& #\# #\+ #\!))

(defgeneric nickname (object))
(defgeneric (setf nickname) (nick object))
(defgeneric normalize-nickname (object))

(defclass nickname ()
  ((nickname :type nickname-string
             :reader nickname
             :initarg :nick
             :initarg :nickname
             :documentation "IRC user nickname")))

(defmethod normalize-nickname ((object nickname))
  (normalize-nickname  (slot-value object 'nickname)))

(defmethod normalize-nickname ((nickname string))
  (string-downcase nickname))

(defmethod (setf nickname) ((nickname string) (object nickname))
  (setf (slot-value object 'nickname) nickname))

(defpackage #:nispbot
  (:use :common-lisp :lift
        :nisp
        :nisp.irc
        :cl-irc :cl-ppcre
        :nispbot-config
        :nisp.ldap
        :nisp.8b-ldap
        :nisp-empty-package
        :nisp-safe
        :nistilities)
  (:shadowing-import-from :nisp.irc :nickname)
  (:shadowing-import-from :cl-irc :pass))

(in-package :nispbot)

;;; cl-ppcre registers. Do we need this? --nixeagle
(setq *allow-named-registers* t)

(defvar *nispbot*)

(deftype valid-comchar ()
  "Usable characters for irc comchars.

Most anything else in the ASCII set can't be used as they occur as part
of normal conversation and an IRC bot that interferes with that is not
a very friendly bot."
  '(member #\! #\# #\% #\) #\+ #\, #\-
    #\@ #\\ #\] #\_ #\` #\{ #\| #\} #\~))


(defgeneric comchar (object))
(defgeneric (setf comchar) (character object))

(defclass comchar ()
  ((comchar :type valid-comchar
             :reader comchar
             :initarg :char
             :initarg :comchar
             :documentation "Single character that the program responds to."))
  (:documentation "Represents an irc bot comchar.

This is a single character, usually a symbol that the bot responds
to. This class will signal an error if a comchar is not of the type
valid-comchar.")
  (:default-initargs :comchar #\!))

(defmethod (setf comchar) ((char character) (object comchar))
  (declare (type base-char char))
  (setf (slot-value object 'comchar) char))

(defmethod (setf comchar) ((char string) (object comchar))
  (declare (type (base-string 1) char))
  (setf (slot-value object 'comchar)
        (character char)))

(defclass irc-bot (irc:connection comchar)
  ((admin-hosts :accessor irc-bot-admin-hosts
                :initarg :admin-hosts
                :initform nispbot-config::*admin-hosts*)
   (safe :accessor irc-bot-safe
         :initform (make-safe-set))))


(defun make-irc-bot (nick server)
  (connect :nickname nick :connection-type 'irc-bot
           :server server
           :password nispbot-config::*password*))

(defgeneric join-all-channels (instance)
  (:documentation "Join all channels in *channels*. Later this will be expanded to use the connection's channel list."))

(defmethod join-all-channels ((bot irc-bot))
  (mapc
   (lambda (channel)
     (join bot channel))
   nispbot-config::*channels*))

(defgeneric reset-command-hook (instance))
(defmethod reset-command-hook ((bot irc-bot))
  (irc:remove-hooks bot 'irc:irc-privmsg-message)
  (irc:add-hook bot 'irc:irc-privmsg-message #'command-hook))

(defgeneric parse-eval-request (instance message))
(defmethod parse-eval-request ((bot irc-bot) (msg irc-privmsg-message))
  "might want to return our own message type eventually"
  (when (is-eval-request bot msg)
    (subseq (second (arguments msg)) 1)))
(defmethod parse-eval-request ((bot irc-bot) (msg string))
  (when (is-eval-request bot msg)
    (subseq msg 1)))
(defmethod parse-eval-request (bot msg)
  "If we get nil as a message, return nil"
  (declare (ignore bot))
  (when msg
    (error "Failed to parse message ~A" msg))
  nil)
 
(defgeneric is-eval-request (instance message))
(defmethod is-eval-request ((bot irc-bot) (msg irc-privmsg-message))
  (is-eval-request bot (second (arguments msg))))
;;; return something more useful then this...
(defmethod is-eval-request ((bot irc-bot) (msg string))
  (and (< 0 (length msg))
       (eq (char msg 0) (comchar bot))))

(defgeneric safe-eval (instance forms))
(defmethod safe-eval ((message irc:irc-privmsg-message) forms)
  (let ((read-result (safe-read message forms)))
    (multiple-value-bind (res)
                 (let ((*package* (nisp-safe::safe-package
                           (safe-select (irc-bot-safe (connection message))
                                                     (host message)))))
                   (cl::eval read-result))
      res)))

(defun nisp-safe::populate-ldap-stuff (safe-package)
  (declare (ignore safe-package))
  (let ((prior-hello-results '(nil nil nil nil nil nil)))
    (defun safe-closure::hello ()
      (labels ((get-hello (&optional (old-hello ""))
               (if (member old-hello prior-hello-results)
                   (get-hello (nisp.hello:hello))
                   (progn
                     (let ((old-results (nbutlast prior-hello-results)))
                       (setf prior-hello-results
                             (push old-hello old-results)))
                     old-hello))))
        (get-hello (nisp.hello:hello)))))
  (defun safe-closure::ldap-entry (string &optional attrs)
    (one-line-ldif (get-single-entry string :attrs attrs)))
  (defun safe-closure::ircUser (string)
    (safe-closure:ldap-entry (concatenate 'string "uid=" string))))

(defun command-hook (message)
  (declare (notinline command-hook))
  "For now lets try to parse just one command"
  (setq 
   nisp-safe::*populate-functions*
        (adjoin 'nisp-safe::populate-ldap-stuff
                nisp-safe::*populate-functions*))
  (let* ((forms (parse-eval-request (connection message) message))
         (admin-request (parse-eval-request (connection message)
                                            forms)))
    (when forms
      (handler-case
          (if (and (member (host message) 
                           (irc-bot-admin-hosts (connection message)) :test #'string=)
                   admin-request)
              ;; User is person running the bot, so allow any lisp to
              ;; be evaluated by that person.
              (trivial-timeout:with-timeout (10)
                (privmsg (connection message)
                         (first (arguments message))
                         (strip-newlines
                          (format nil "~A"
                                  (with-package :nispbot
                                    (eval (read-from-string admin-request)))))))
              (trivial-timeout:with-timeout (1)
              ;; Untrusted users, eval their stuff in sandboxes
                (privmsg (connection message)
                         (first (arguments message))
                         (strip-newlines
                          (format nil "~A"
                                  (safe-eval message forms))))))
;        (end-of-file (condition) (values nil condition))
        (error (condition) (privmsg (connection message)
                                    (first (arguments message))
                                    (strip-newlines (format nil "~A" condition))))))))

#+nil
(defun parse-links (string)
  (let ((it ()))
  (cl-ppcre:do-matches-as-strings (var "\\\[\\\[(.*?)\\\]\\\]" string it)
    (describe var)
    (push var it))))

(defun matches-list (reg &rest strings)
  (let ((ret ()))
    (dolist (str strings ret)
      (push
       (second (multiple-value-list
                (cl-ppcre:scan-to-strings reg str))) ret))))


(defmethod safe-read ((msg irc-privmsg-message)
                       (forms string) &optional owner)
  "Read given IRC message in the package corresponding to requesters hostmask"
  (declare (ignore owner))
  (safe-read (irc-bot-safe (connection msg))
             forms
             (host msg)))

(defun pull ()
  "Pull the source from github."
  (trivial-shell:shell-command "git pull"))


;; From nisp-dev-helper
(defun start-nispbot-instance (&optional (nick nispbot-config::*nickname*))
  (setq nispbot::*nispbot* (nispbot::make-irc-bot nick "irc.eighthbit.net"))
  (irc:start-background-message-handler nispbot::*nispbot*)
  (sleep 3) 
  (nispbot::join-all-channels nispbot::*nispbot*)
  (nispbot::reset-command-hook nispbot::*nispbot*))

(defvar *freenode*)
(defun start-freenode-instance ()
  "Quickie to get something up on freenode"
  (setq nispbot::*freenode*
        (irc:connect :connection-type 'nispbot::irc-bot
                     :nickname nispbot-config::*nickname*
                     :server "irc.freenode.net"
                     :password nispbot-config::*freenode-password*))
  (irc:start-background-message-handler nispbot::*freenode*)
  (sleep 3)
  (irc:join nispbot::*freenode* "#botters")
  (nispbot::reset-command-hook nispbot::*freenode*))