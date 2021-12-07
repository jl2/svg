;;;; package.lisp

(defpackage #:svg
  (:use #:cl #:3d-vectors)
  (:export
   #:rectangle
   #:circle
   #:ellipse
   #:rectangle
   #:ellipse
   #:circle
   #:polyline
   #:line
   #:polygon
   #:regular-polygon
   #:star
   #:begin-svg
   #:end-svg
   #:with-svg
   ))
