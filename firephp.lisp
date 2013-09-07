(defpackage #:firephp
  (:use :cl)
  (:documentation
   "FirePHP protocol server implementation"))

(in-package :firephp)
(export '(send-message fb))

(defun split-into-chunks (sequence &optional (size 1))
  (let ((list (copy-seq sequence)))
    (loop while list collect 
          (if (< (length list) size)
            (prog1 
              list
              (setf list nil))
            (prog1
              (subseq list 0 size)
              (setf list (subseq list size)))))))

(defun send-header (name value)
  (setf (hunchentoot:header-out name) value))

(defun get-last-message-index ()
  (let ((value (hunchentoot:header-out :x-wf-1-index)))
    (if value (parse-integer value)  
      0)))

(defun send-message (message &rest args &key (type :dump) (label nil))
  (unless (boundp 'hunchentoot:*reply*)
    (warn "It seems like hunchentoot is not started, trying to send message ~A" message)
    (return-from send-message))
  (let* 
    ((dump (equal type :dump))
     (structure-index (if dump 2 1))
     (message-index (1+ (get-last-message-index)))
     (firephp-version "0.3"))

    (send-header "X-Wf-Protocol-1" "http://meta.wildfirehq.org/Protocol/JsonStream/0.2")
    (send-header "X-Wf-1-Plugin-1" (format nil "http://meta.firephp.org/Wildfire/Plugin/FirePHP/Library-FirePHPCore/~a" firephp-version))

    (if dump 
      (progn 
        (send-header "X-Wf-1-Structure-2" "http://meta.firephp.org/Wildfire/Structure/FirePHP/Dump/0.1")
        (if label
          (setf message (format nil "{\"~A\":~A}" label (json:encode-json-to-string message)))
          (setf message (json:encode-json-to-string message))))
      (progn 
        (send-header "X-Wf-1-Structure-1" "http://meta.firephp.org/Wildfire/Structure/FirePHP/FirebugConsole/0.1")
        (loop for i in (list :type) do 
              (when (getf args i)
                (setf (getf args i) (string-upcase (getf args i))))) 

        (setf message 
              (format nil "[~A,~A]" 
                      (json:encode-json-alist-to-string 
                        (loop for (key value) on args :by #'cddr 
                              collect (cons (string-capitalize key) value)))
                      (json:encode-json-to-string message)))))

    (let* ((chunks (split-into-chunks message 5000))
           (chunks-length (length chunks)))
      (loop for i in chunks 
            for j from 0 do 
            (if (> chunks-length 2)
              (send-header (format nil "X-Wf-1-~d-1-~d" structure-index message-index)
                           (format nil "~a|~a|~a" 
                                   (if (zerop j) (length message) "")
                                   i
                                   (if (< j (- chunks-length 2)) "\\" "")))
              (send-header (format nil "X-Wf-1-~d-1-~d" structure-index message-index)
                           (format nil "~a|~a|~a" (length i) i "")))
            (incf message-index 1)))

    (send-header "X-Wf-1-Index" (write-to-string (- message-index 1)))))

(defun fb (&rest args)
  "Simple debug function applies to any arguments and just displays them"
  (send-message 
    (format nil "~{#~d ~A~^<br/>~}" 
            (loop for i from 1 
                  for j in (mapcar #'hunchentoot:escape-for-html (mapcar #'prin1-to-string args))
                  append (list i j))) :type :log))

(defun descr (&rest args)
  (send-message 
    (ppcre:regex-replace-all 
      (string #\Newline)
      (hunchentoot:escape-for-html 
        (with-output-to-string (s)
          (loop for i in args do 
                (describe i s))))
      "<br/>")
    :type :log))
