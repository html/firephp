(defpackage #:firephp-asd
  (:use :cl :asdf))

(in-package :firephp-asd)

(defsystem firephp
 :name "firephp"
 :version (:read-from-file "version.lisp-expr")
 :maintainer "Olexiy Zamkoviy"
 :author "Olexiy Zamkoviy"
 :licence "LLGPL"
 :description "FirePHP debug utility"
 :depends-on (:cl-json :hunchentoot)
 :components ((:file "firephp")))

