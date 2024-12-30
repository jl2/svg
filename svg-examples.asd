;; svg-examples.asd

(asdf:defsystem #:svg-examples
  :serial t
  :description "A very simple SVG generator."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com"
  :license "ISC"
  :depends-on (#:3d-vectors #:svg #:hunchentoot)
  :components ((:file "svg-examples")))

