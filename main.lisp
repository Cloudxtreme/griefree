(require :hunchentoot)
(require :cl-log)
(require :xml-emitter)
(require :cl-who)
(require :drakma)
(require :quicktwiml)

(setf hunchentoot:*catch-errors-p* nil)
(hunchentoot:start (make-instance 'hunchentoot:acceptor 
                                  :port 8080))

(defstruct session 
  status
  id
  call-id ;; generated by twilio
  caller-ip
  outgoing-call-id
  phone-number
  record-url)

(defvar *session-table* (make-hash-table :test 'equal))

(defun new-session (&optional &key caller-ip)
  (let* ((session-id (random 9999999))
         (session (make-session :status 'new
                                :id session-id
                                :caller-ip caller-ip)))
    (setf (gethash session-id *session-table*) session)
    (session-id session)))

(defun find-session-with-call-id (call-id)
  "Find a session record by the given call id"
  (loop 
     for k being the hash-keys of *session-table*
     when (string-equal call-id (session-call-id (gethash k *session-table*)))
     do 
       (return (gethash k *session-table*))))
       
(defun find-session-with-outgoing-call-id (call-id)
  (loop 
     for k being the hash-keys of *session-table*
     when (string-equal call-id (session-outgoing-call-id (gethash k *session-table*)))
     do 
       (return (gethash k *session-table*))))

(defun get-session (session-id)
  (gethash session-id *session-table*))

(defun check-session (session-id)
  (let ((session (gethash session-id *session-table*)))
    (and session (equal (session-status session) 'new))))

(defun done-session (session-id)
  (setf (session-status (gethash session-id *session-table*)) 'done))

(defun expire-session (session-id)
  (remhash session-id *session-table*))

(pushnew (hunchentoot:create-prefix-dispatcher "/c/place"
                                               'place-call)
         hunchentoot:*dispatch-table*)

(pushnew (hunchentoot:create-prefix-dispatcher "/c/new"
                                               'new-call)
         hunchentoot:*dispatch-table*)

(pushnew (hunchentoot:create-prefix-dispatcher "/c/record"
                                               'record)
         hunchentoot:*dispatch-table*)

(pushnew (hunchentoot:create-prefix-dispatcher "/c/check"
                                               'check)
         hunchentoot:*dispatch-table*)

(pushnew (hunchentoot:create-prefix-dispatcher "/asf123x/call"
                                               'call)
         hunchentoot:*dispatch-table*)

(pushnew (hunchentoot:create-prefix-dispatcher "/asf123x/place-call-cb"
                                               'place-call-cb)
         hunchentoot:*dispatch-table*)

(pushnew (hunchentoot:create-prefix-dispatcher "/asf123x/receive-call"
                                               'receive-call)
         hunchentoot:*dispatch-table*)

(pushnew (hunchentoot:create-prefix-dispatcher "/asf123x/session-match"
                                               'session-match)
         hunchentoot:*dispatch-table*)

(setf (cl-log:log-manager)
      (make-instance 'cl-log:log-manager 
                     :message-class 'cl-log:formatted-message))

(cl-log:start-messenger 'cl-log:text-file-messenger
                        :filename "/tmp/log.txt")

(defun call ()
  (cl-log:log-message :log (format nil "~A" (hunchentoot:post-parameters*)))
  (with-output-to-string (s)
    (xml-emitter:with-xml-output (s)
      (xml-emitter:with-tag ("Response")
        (xml-emitter:with-tag ("Gather"
                               '(("action" "http://ec2-175-41-199-117.ap-northeast-1.compute.amazonaws.com/asf123x/session-match")
                                 ("method" "POST")))
          (xml-emitter:simple-tag "Say" "Hello, welcome to grief free service. Please input your session ID, followed by the pound sign"))))))

(defun session-match ()
  (cl-log:log-message :log (format nil "~A" (hunchentoot:post-parameters*)))
  (let ((session-id (parse-integer (hunchentoot:post-parameter "Digits") :junk-allowed t)))
    (if (check-session session-id)
        (progn 
          (done-session session-id)
          (setf (session-call-id (get-session session-id))
                (hunchentoot:post-parameter "CallSid"))
          (with-output-to-string (s)
            (xml-emitter:with-xml-output (s)
              (xml-emitter:with-tag ("Response")
                (xml-emitter:simple-tag "Say" "Please say your message after beep")
                (xml-emitter:with-tag ("Record" 
                                       '(("action"  "http://ec2-175-41-199-117.ap-northeast-1.compute.amazonaws.com/asf123x/receive-call")
                                         ("method"  "POST")
                                         ("maxLength"  "20")
                                         ("finishOnKey"  "#"))))))))
        (with-output-to-string (s)
          (xml-emitter:with-xml-output (s)
            (xml-emitter:with-tag ("Response")
              (xml-emitter:simple-tag "Say" "Sorry, the session ID can not be found")))))))
                                   

(defun receive-call ()
  (cl-log:log-message :log (format nil "~A" (hunchentoot:post-parameters*)))
  (let ((session (find-session-with-call-id (hunchentoot:post-parameter "CallSid"))))
    (setf (session-record-url session) (hunchentoot:post-parameter "RecordingUrl"))
    (with-output-to-string (s)
      (xml-emitter:with-xml-output (s)
        (xml-emitter:with-tag ("Response")
          (xml-emitter:simple-tag "Say" "Thank you for your message. Please listen it in your browser."))))))

(defun place-call-cb ()
  (cl-log:log-message :log (format nil "~A" (hunchentoot:post-parameters*)))
  (let ((session (find-session-with-outgoing-call-id (hunchentoot:post-parameter "CallSid"))))
    (with-output-to-string (s)
      (xml-emitter:with-xml-output (s)
        (xml-emitter:with-tag ("Response")
          (xml-emitter:simple-tag "Say" "The following message is recorded for a user to you.")
          (xml-emitter:simple-tag "Play" (session-record-url session)))))))

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
    

(setf drakma:*text-content-types* (list (list "text" "application")))

(defun call-number (number)
  "Call a number, callback to place-call, and return the sid"
  (let ((sid (car (last (car (cdadr (s-xml:parse-xml-string (octets-to-string (drakma:http-request "https://api.twilio.com/2010-04-01/Accounts/AC1c912de8d4f8c2068e836a50b3ce2ef0/Calls"
                                                                                                   :method :post
                                                                                                   :basic-authorization (list "AC1c912de8d4f8c2068e836a50b3ce2ef0" "bb5eaeb5dc3c3dab192cf4356008ff17")
                                                                                                   :parameters `(("From" . "8887470593")
                                                                                                                 ("To" . ,number)
                                                                                                                 ("Url" . "http://ec2-175-41-199-117.ap-northeast-1.compute.amazonaws.com/asf123x/place-call-cb")
                                                                                                                 ("Method" . "POST")))))))))))
         sid))
    

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