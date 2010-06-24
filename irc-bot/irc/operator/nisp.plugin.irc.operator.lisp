(defpackage #:nisp.plugin.irc.operator
  (:use :cl :alexandria :nisp-core :ironclad))

(in-package :nisp.plugin.operator)

;; Hashing functions

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence :sha256 
			     (ironclad:ascii-string-to-byte-array password))))

(defun hash= (hash1 hash2)
  (string= hash1 hash2))

(defun verify-password (password hash)
  (hash= (hash-password password) hash))

;; Channel modes class

(defclass channel-modes ()
  ((name  :accessor chan-name
	  :initform "#channel"
	  :initarg  :name)

   (modes :accessor chan-modes
	  :initform ""
	  :initarg :chan-modes))

;; Privileges class

(defclass privileges ()
  ((channels  :accessor priv-chans
	      :initform '((make-instance channels))
	      :initarg :channels)

   (commands  :accessor priv-coms
	      :initform '()
	      :initarg  :commands)))


;; Operator class

(defclass operator () 
  ((nick                 :accessor oper-nick
			 :initform person
			 :initarg  :nick)

   (host                 :accessor oper-host
			 :initform "nick!ident@host"
			 :initarg :host)

   (password             :accessor oper-password
			 :initform (hash-password "password")
			 :initarg  :password)

   (privileges           :accessor oper-privs
			 :initform (make-instance privileges)
			 :initarg  :privs)

   (need-password        :accessor oper-need-pass
			 :initform t
			 :initarg  :need-pass)))
