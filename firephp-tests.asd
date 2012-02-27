(defpackage #:firephp-test-asd
  (:use :cl :asdf))

(in-package :firephp-test-asd)

(defsystem firephp-tests
 :name "firephp-tests"
 :version "0.0.1"
 :maintainer "Olexiy Zamkoviy"
 :author "Olexiy Zamkoviy"
 :licence "LLGPL"
 :description "FirePHP debug utility tests"
 :depends-on (:cl-json :hunchentoot :firephp :hu.dwim.stefil)
 :components ((:file "firephp-tests")))

