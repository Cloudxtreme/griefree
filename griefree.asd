;;;; griefree.asd

(asdf:defsystem #:griefree
  :serial t
  :depends-on (#:hunchentoot
               #:cl-log
               #:cl-who
               #:drakma
               #:quicktwiml)
  :components ((:file "package")
               (:file "session")
               (:file "twiliocallback")
               (:file "web")
               (:file "griefree")))

