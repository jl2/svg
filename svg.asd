;;;; svg.asd

(asdf:defsystem #:svg
  :serial t
  :description "Describe svg here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "svg")))

