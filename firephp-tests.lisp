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

(deftest sends-right-headers-on-fb-call-1 ()
  (with-temporary-reply 
    (firephp:fb "Test" "strings")
    (headers-are-present 
      '((:X-WF-1-INDEX . "1") 
        (:X-WF-1-1-1-1 . "72|[{\"Type\":\"LOG\"},\"#1 &quot;Test&quot;<br\\/>#2 &quot;strings&quot;<br\\/>\"]|")
        (:X-WF-1-STRUCTURE-1 . "http://meta.firephp.org/Wildfire/Structure/FirePHP/FirebugConsole/0.1")
        (:X-WF-1-PLUGIN-1 . "http://meta.firephp.org/Wildfire/Plugin/FirePHP/Library-FirePHPCore/0.3")
        (:X-WF-PROTOCOL-1 . "http://meta.wildfirehq.org/Protocol/JsonStream/0.2") 
        (:CONTENT-TYPE . "text/html")))))

(deftest sends-right-headers-on-fb-call-2 ()
  (let ((firephp:*escape-html-p* nil))
    (with-temporary-reply 
      (firephp:fb "Test" "strings")
      (headers-are-present 
        '((:X-WF-1-INDEX . "1")
          (:X-WF-1-1-1-1 . "48|[{\"Type\":\"LOG\"},\"#1 \\\"Test\\\"\\n#2 \\\"strings\\\"\\n\"]|")
          (:X-WF-1-STRUCTURE-1 . "http://meta.firephp.org/Wildfire/Structure/FirePHP/FirebugConsole/0.1")
          (:X-WF-1-PLUGIN-1 . "http://meta.firephp.org/Wildfire/Plugin/FirePHP/Library-FirePHPCore/0.3")
          (:X-WF-PROTOCOL-1 . "http://meta.wildfirehq.org/Protocol/JsonStream/0.2") 
          (:CONTENT-TYPE . "text/html"))))))

(deftest sends-right-headers-on-descr-call-1 ()
  (with-temporary-reply 
    (firephp:descr "Test" "strings")
    (headers-are-present 
      '((:X-WF-1-INDEX . "1")
        (:X-WF-1-1-1-1
          . "212|[{\"Type\":\"LOG\"},\"&quot;Test&quot;<br\\/>  [simple-string]<br\\/><br\\/>Element-type: CHARACTER<br\\/>Length: 4<br\\/>&quot;strings&quot;<br\\/>  [simple-string]<br\\/><br\\/>Element-type: CHARACTER<br\\/>Length: 7<br\\/>\"]|")
        (:X-WF-1-STRUCTURE-1 . "http://meta.firephp.org/Wildfire/Structure/FirePHP/FirebugConsole/0.1")
        (:X-WF-1-PLUGIN-1 . "http://meta.firephp.org/Wildfire/Plugin/FirePHP/Library-FirePHPCore/0.3")
        (:X-WF-PROTOCOL-1 . "http://meta.wildfirehq.org/Protocol/JsonStream/0.2")
        (:CONTENT-TYPE . "text/html")))))

(deftest sends-right-headers-on-descr-call-2 ()
  (let ((firephp:*escape-html-p* nil))
    (with-temporary-reply 
      (firephp:descr "Test" "strings")
      (headers-are-present 
        '((:X-WF-1-INDEX . "1")
          (:X-WF-1-1-1-1
            . "156|[{\"Type\":\"LOG\"},\"\\\"Test\\\"\\n  [simple-string]\\n\\nElement-type: CHARACTER\\nLength: 4\\n\\\"strings\\\"\\n  [simple-string]\\n\\nElement-type: CHARACTER\\nLength: 7\\n\"]|")
          (:X-WF-1-STRUCTURE-1 . "http://meta.firephp.org/Wildfire/Structure/FirePHP/FirebugConsole/0.1")
          (:X-WF-1-PLUGIN-1 . "http://meta.firephp.org/Wildfire/Plugin/FirePHP/Library-FirePHPCore/0.3")
          (:X-WF-PROTOCOL-1 . "http://meta.wildfirehq.org/Protocol/JsonStream/0.2")
          (:CONTENT-TYPE . "text/html"))))))
