;;;; griefree.lisp

(in-package #:griefree)

;;; "griefree" goes here. Hacks and glory await!

(require :hunchentoot)
(require :cl-log)
(require :xml-emitter)
(require :cl-who)
(require :drakma)
(require :quicktwiml)

(defun initialize (port)
  (setf hunchentoot:*catch-errors-p* nil)
  (hunchentoot:start (make-instance 'hunchentoot:acceptor 
                                    :port port))

  (setf hunchentoot:*dispatch-table* nil)
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
                          :filename "/tmp/log.txt"))



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
    

