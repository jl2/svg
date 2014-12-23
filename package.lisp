;;;; package.lisp

(defpackage #:svg
  (:use #:cl)
  (:export #:make-svg
		   #:add-point-coords
		   #:add-line-coords
		   #:add-polygon-coords
		   #:to-file
		   #:to-file-name
                   #:color
		   ))

;; (let ((pack (find-package :foo)))
;;   (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))

