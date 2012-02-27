(defpackage #:firephp-asd
  (:use :cl :asdf))

(in-package :firephp-asd)

(defsystem firephp
 :name "firephp"
 :version "0.0.1"
 :maintainer "Olexiy Zamkoviy"
 :author "Olexiy Zamkoviy"
 :licence "LLGPL"
 :description "FirePHP debug utility"
 :depends-on (:cl-json :hunchentoot)
 :components ((:file "firephp")))

