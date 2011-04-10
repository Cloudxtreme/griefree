(in-package #:griefree)

(defun new-call ()
  "Show user interface to make new call"
  (with-output-to-string (stream)
    (cl-who:with-html-output (stream)
      (:html
       (:head
        (:title "Are you ready?"))
       (:body
        (:form :action "/c/record"
               :method "post"
               (:input :type "submit" :value "Prepare to record")))))))

(defun record ()
  (let ((session-id (new-session :caller-ip (hunchentoot:real-remote-addr))))
    (with-output-to-string (stream)
      (cl-who:with-html-output (stream)
        (:html
         (:head
          (:title "Please call (888) 747-0593 and input the following PIN number to start your recording"))
         (:body
          (:h1 "Please call (888) 747-0593 and input the following PIN number to start your recording")
          (:h1 (cl-who:str (format nil "PIN: ~A" session-id)))
          (:a :href (cl-who:str (format nil "/c/check?id=~A" session-id))
              "I have finished.")))))))

(defun check ()
  (let* ((session-id (parse-integer (hunchentoot:get-parameter "id") :junk-allowed t))
         (session (get-session session-id)))
    (with-output-to-string (stream)
      (cl-who:with-html-output (stream)
        (:html
         (:head
          (:title "Please input telephone number"))
         (:body
          (:h1 "Please input telephone number")
          (:form :action "/c/place" :method "post"
                 (:input :type "text" :name "number")
                 (:input :type "hidden" :name "id" :value (session-id session))
                 (:input :type "submit" :value "Call"))))))))
    
(defun place-call ()
  (let* ((session (get-session (parse-integer (hunchentoot:post-parameter "id") :junk-allowed t)))
         (number (hunchentoot:post-parameter "number")))
    (setf (session-phone-number session) number)
    (setf (session-outgoing-call-id session) (call-number number))
    (with-output-to-string (stream)
      (cl-who:with-html-output (stream)
        (:html 
         (:head
          (:title "Placing call"))
         (:body
          (:h1 "Placing call")))))))