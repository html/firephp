(defpackage #:firephp-tests
  (:use :cl :hu.dwim.stefil)
  (:documentation
   "FirePHP protocol server implementation tests"))

(in-package :firephp-tests)

(defmacro with-replaced-function (fdef &rest body)
  (let ((oldf (gensym))
        (result (gensym))
        (name (car fdef))
        (args (cadr fdef))
        (rbody (cddr fdef)))
    `(let ((,oldf (symbol-function ',name)))
       (setf (symbol-function ',name) (lambda ,args ,@rbody))
       (let ((,result (progn ,@body)))
         (setf (symbol-function ',name) ,oldf)
         ,result))))

(defsuite* all-tests)

(defun do-all-tests ()
  (all-tests))

(defmacro with-temporary-reply (&body body)
  `(let ((hunchentoot:*reply* (make-instance 'hunchentoot:reply)))
     ,@body))

(defun headers-are-present (headers)
  (loop for i in headers do 
        (is  
          (string= 
            (hunchentoot:header-out (car i))
            (cdr i)))))

(deftest sends-right-headers-on-dump-with-label ()
  (with-temporary-reply 
    (firephp:send-message "Test" :label "TestLabel")
    (headers-are-present 
      '((:X-WF-1-INDEX . "1") 
        (:X-WF-1-2-1-1 . "20|{\"TestLabel\":\"Test\"}|")
        (:X-WF-1-STRUCTURE-2 . "http://meta.firephp.org/Wildfire/Structure/FirePHP/Dump/0.1")
        (:X-WF-1-PLUGIN-1 . "http://meta.firephp.org/Wildfire/Plugin/FirePHP/Library-FirePHPCore/0.3")
        (:X-WF-PROTOCOL-1 . "http://meta.wildfirehq.org/Protocol/JsonStream/0.2")))))

(deftest sends-right-headers-on-dump ()
  (with-temporary-reply 
    (firephp:send-message "Test")
    (headers-are-present 
      '((:X-WF-1-INDEX . "1")
        (:X-WF-1-2-1-1 . "6|\"Test\"|")
        (:X-WF-1-STRUCTURE-2 . "http://meta.firephp.org/Wildfire/Structure/FirePHP/Dump/0.1")
        (:X-WF-1-PLUGIN-1 . "http://meta.firephp.org/Wildfire/Plugin/FirePHP/Library-FirePHPCore/0.3")
        (:X-WF-PROTOCOL-1 . "http://meta.wildfirehq.org/Protocol/JsonStream/0.2")))))

(deftest sends-right-headers-on-log ()
  (with-temporary-reply 
    (firephp:send-message "Test" :type :log)
    (headers-are-present 
      '((:X-WF-1-INDEX . "1") 
        (:X-WF-1-1-1-1 . "23|[{\"Type\":\"LOG\"},\"Test\"]|")
        (:X-WF-1-STRUCTURE-1 . "http://meta.firephp.org/Wildfire/Structure/FirePHP/FirebugConsole/0.1")
        (:X-WF-1-PLUGIN-1 . "http://meta.firephp.org/Wildfire/Plugin/FirePHP/Library-FirePHPCore/0.3")
        (:X-WF-PROTOCOL-1 . "http://meta.wildfirehq.org/Protocol/JsonStream/0.2")))))

(deftest sends-right-headers-on-log-with-label ()
  (with-temporary-reply 
    (firephp:send-message "Test" :type :log :label "TestLabel")
    (headers-are-present 
      '((:X-WF-1-INDEX . "1") 
        (:X-WF-1-1-1-1 . "43|[{\"Type\":\"LOG\",\"Label\":\"TestLabel\"},\"Test\"]|")
        (:X-WF-1-STRUCTURE-1 . "http://meta.firephp.org/Wildfire/Structure/FirePHP/FirebugConsole/0.1")
        (:X-WF-1-PLUGIN-1 . "http://meta.firephp.org/Wildfire/Plugin/FirePHP/Library-FirePHPCore/0.3")
        (:X-WF-PROTOCOL-1 . "http://meta.wildfirehq.org/Protocol/JsonStream/0.2")))))
