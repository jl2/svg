;;;; svg.lisp

(in-package #:svg)

;;; "svg" goes here. Hacks and glory await!

(defstruct rgba-color
  (red 0.0 :type real)
  (green 0.0 :type real)
  (blue 0.0 :type real)
  (alpha 0.0 :type real))

(defun color (r g b &optional (a 1.0))
  (make-rgba-color :red r :green g :blue b :alpha a))

(defun to-svgstring (color)
  (declare (color rgba-color))
  (format nil "rgba(~d,~d,~d,~d)"
          (truncate (* 255 (rgba-color-red color)))
          (truncate (* 255 (rgba-color-green color)))
          (truncate (* 255 (rgba-color-blue color)))
          (truncate (* 255 (rgba-color-alpha color)))))

(defstruct svg
  (points nil :type list)
  (polys nil :type list)
  (lines nil :type list))

(defstruct point
  (x 0.0 :type (or double-float single-float integer))
  (y 0.0 :type (or double-float single-float integer))
  (color nil))

(defun scale-point (pt s)
  (declare (point pt))
  (declare (number s))
  (make-point :x (* (point-x pt) s) :y (* (point-y pt) s)))

(defstruct line
  (pt1 nil :type point)
  (pt2 nil :type point)
  (color nil))

(defun scale-line (ln s)
  (declare (line ln))
  (declare (number s))
  (make-line :pt1 (scale-point (line-pt1 ln) s) :pt2 (scale-point (line-pt2 ln) s) :color (line-color ln)))

(defstruct polygon
  (points nil :type list)
  (fill-color nil)
  (stroke-color nil))

(defun add-point-coords (img x y &optional (color nil))
  (declare ((or single-float double-float integer ) x))
  (declare ((or single-float double-float integer ) y))
  (setf (svg-points img) (cons (make-point :x x :y y :color color) (svg-points img)))
  img)

(defun add-point (img pt &optional (color nil))
  (setf (svg-points img) (cons pt (svg-points img)))
  img)

(defun add-line-coords (img x1 y1 x2 y2 &optional (color nil))
  (declare ((or single-float double-float integer ) x1))
  (declare ((or single-float double-float integer ) y1))
  (declare ((or single-float double-float integer ) x2))
  (declare ((or single-float double-float integer ) y2))
  (setf (svg-lines img) (cons (make-line :pt1 (make-point :x x1 :y y1 :color nil) :pt2 (make-point :x x2 :y y2 :color nil) :color color) (svg-lines img)))
  img)

(defun add-line (img ln)
  (setf (svg-lines img) (cons ln (svg-lines img)))
  img)

(defun add-line-points (img pt1 pt2 &optional (color nil))
  (setf (svg-lines img) (cons (make-line :pt1 pt1 :pt2 pt2 :color color) (svg-lines img)))
  img)

(defun add-polygon-coords (img &optional (fill-color nil)  (stroke-color nil) &rest pts)
  (declare (list x1))
  (setf
   (svg-polys img)
   (cons
	(make-polygon :fill-color fill-color :stroke-color stroke-color :points (mapcar #'(lambda (pt) (make-point :x (car pt) :y (cadr pt))) pts)) (svg-polys img)))
  img)

(defun to-file (img &key (stream t) (scale 1.0) (width 800) (height 600))
  (format stream "<?xml version=\"1.0\" standalone=\"no\"?> <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"> <svg width=\"~d\" height=\"~d\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">" width height)
  (dolist (poly (svg-polys img))
    (let ((fill-color (polygon-fill-color poly))
          (stroke-color (polygon-stroke-color poly)))
      (format stream "<polygon points=\"")
      (dolist (pt (polygon-points poly))
        (format stream "~f,~f " (* scale (point-x pt)) (* scale (point-y pt)) ))
      (if fill-color
          (format stream "\" style=\"fill:~a; stroke:~a;stroke-width:1\"/>" (to-svgstring fill-color) (to-svgstring stroke-color))
        (format stream "\" style=\"fill:#cccccc; stroke:#000000;stroke-width:1\"/>"))))

  (dolist (line (svg-lines img))
	(let* ((p1 (line-pt1 line))
               (p2 (line-pt2 line))
               (color (line-color line))
               (cstring (if color
                            (to-svgstring color)
                          "rgb(0,0,0)")))
               
	  (format stream "<line x1=\"~f\" y1=\"~f\" x2=\"~f\" y2=\"~f\" style=\"stroke:~a;stroke-width:2\"/>"
			  (* scale (point-x p1))
			  (* scale (point-y p1))
			  (* scale (point-x p2))
			  (* scale (point-y p2))
                          cstring)))
  
  (dolist (pt (svg-points img))
    (let* ((color (point-color pt))
           (cstring (if color
                        (to-svgstring color)
                      "green")))
      (format stream "<circle cx=\"~f\" cy=\"~f\" r=\"3\" stroke=\"~a\" stroke-width=\"2\" fill=\"~a\"/>" (* scale (point-x pt)) (* (point-y pt)) cstring cstring)))

  (format stream "</svg>"))

(defun to-file-name (img fname &key (scale 1.0) (width 800) (height 600))
  (with-open-file (stream fname :direction :output :if-exists :supersede :if-does-not-exist :create)
				  (to-file img :stream stream :scale scale
                                           :width width :height height)))
