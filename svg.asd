;;;; svg.asd

(asdf:defsystem #:svg
  :serial t
  :description "A very simple SVG generator."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com"
  :license "ISC"
  :depends-on (#:3d-vectors)
  :components ((:file "package")
               (:file "svg")))

