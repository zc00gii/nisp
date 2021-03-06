(in-package :cl-user)
(asdf:load-system :closer-mop)
(defpackage #:nisp.mop2
  (:use :cl)
  (:shadowing-import-from :closer-mop :defmethod)
  (:import-from :closer-mop :class-slots))

(in-package :nisp.mop2)

(defmethod closer-mop:class-slots ((class-symbol symbol))
  "List all slots of CLASS-SYMBOL."
  (funcall 'closer-mop:class-slots (find-class class-symbol)))