(defpackage #:nisp.plugin.irc.operator
  (:use :cl :alexandria :nisp-core :ironclad))

(in-package :nisp.plugin.operator)

;; (defclass operator)

;; Hashing functions

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence :sha256 
			     (ironclad:ascii-string-to-byte-array password))))

(defun hash= (hash1 hash2)
  (string= hash1 hash2))

(defun verify-password (password hash)
  (hash= (hash-password password) hash))