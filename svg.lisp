;;;; svg.lisp

(in-package #:svg)

(defun color (stream color)
  (declare (type (or string vec4) color))
  (etypecase color
    (vec4
     (format stream "rgba(~d,~d,~d,~d)"
          (truncate (* 255 (vx color)))
          (truncate (* 255 (vy color)))
          (truncate (* 255 (vz color)))
          (truncate (* 255 (vw color)))))
    (string
     (format stream "~s" color))))

(defun rectangle (stream pt size
                  &key
                    (rounded nil)
                    (stroke-width 1.0)
                    (stroke-color (vec4 0 0 0 1.0))
                    (fill-color (vec4 0 0 0 1.0)))
  ;; TODO: Don't copy/paste this...
  (cond (rounded
         (format stream 
                 "<rect x=\"~f\" y=\"~f\" width=\"~f\" height=\"~f\" rx=\"~f\" ry=\"~f\" stroke=~s stroke-width=\"~f\" fill=~s/>~%"
                 (vx pt) (vy pt)
                 (vx size) (vy size)
                 (vx rounded) (vy rounded)
                 (color nil stroke-color)
                 stroke-width
                 (color nil fill-color)))
        (t
         (format stream 
                 "<rect x=\"~f\" y=\"~f\" width=\"~f\" height=\"~f\" stroke=~s stroke-width=\"~f\" fill=~s/>~%"
                 (vx pt) (vy pt)
                 (vx size) (vy size)
                 (color nil stroke-color)
                 stroke-width
                 (color nil fill-color)))))

(defun ellipse (stream pt size
                &key
                  (stroke-width 1.0)
                  (stroke-color (vec4 0 0 0 1.0))
                  (fill-color (vec4 0 0 0 1.0)))
  )

(defun circle (stream center radius
               &key
                 (stroke-width 1.0)
                 (stroke-color (vec4 0 0 0 1.0))
                 (fill-color (vec4 0 0 0 1.0)))
  (format stream 
          "<circle cx=\"~f\" cy=\"~f\" r=\"~f\" stroke=~s stroke-width=\"~f\" fill=~s/>~%"
          (vx center) (vy center)
          radius
          (color nil stroke-color)
          stroke-width
          (color nil fill-color)))

(defun polyline (stream points
                 &key
                   (stroke-width 1.0)
                   (stroke-color (vec4 0 0 0 1.0)))
  )

(defun line (stream pt1 pt2
             &key
               (stroke-width 1.0)
               (stroke-color (vec4 0 0 0 1.0)))
  )

(defun polygon (stream points
                &key
                  (stroke-width 1.0)
                  (stroke-color (vec4 0 0 0 1.0))
                  (fill-color (vec4 0 0 0 1.0)))
  )

(defun begin-svg (stream width height
                  &key
                    (view-min (vec2 -1.0 -1.0))
                    (view-width (vec2 2.0 2.0)))
  (format stream "~
<?xml version=\"1.0\" standalone=\"no\"?>~%<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">~%
<svg width=\"~d\" height=\"~d\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"~a ~a ~a ~a\">~%"
          width
          height
          (vx view-min)
          (vy view-min)
          (vx view-width)
          (vy view-width)))

(defun end-svg (stream)
  (format stream "</svg>~%"))

(defmacro with-svg-output ((stream width height
                            &key
                              (view-min (vec2 -0.0 -0.0))
                              (view-width (vec2 2.0 2.0)))
                           &body body)
  `(unwind-protect
        (progn
		  (begin-svg ,stream ,width ,height
                     :view-min ,view-min
                     :view-width ,view-width)
		  ,@body)
     (end-svg ,stream)))
  
;; (defun to-file (img &key (stream t) (scale 1.0) (width 800) (height 600))
;;   (format stream "<?xml version=\"1.0\" standalone=\"no\"?> <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"> <svg width=\"~d\" height=\"~d\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">" width height)
;;   (dolist (poly (svg-polys img))
;;     (let ((fill-color (polygon-fill-color poly))
;;           (stroke-color (polygon-stroke-color poly)))
;;       (format stream "<polygon points=\"")
;;       (dolist (pt (polygon-points poly))
;;         (format stream "~f,~f " (* scale (point-x pt)) (* scale (point-y pt)) ))
;;       (if fill-color
;;           (format stream "\" style=\"fill:~a; stroke:~a;stroke-width:1\"/>" (to-svgstring fill-color) (to-svgstring stroke-color))
;;         (format stream "\" style=\"fill:#cccccc; stroke:#000000;stroke-width:1\"/>"))))

;;   (dolist (line (svg-lines img))
;; 	(let* ((p1 (line-pt1 line))
;;                (p2 (line-pt2 line))
;;                (color (line-color line))
;;                (cstring (if color
;;                             (to-svgstring color)
;;                           "rgb(0,0,0)")))
               
;; 	  (format stream "<line x1=\"~f\" y1=\"~f\" x2=\"~f\" y2=\"~f\" style=\"stroke:~a;stroke-width:2\"/>"
;; 			  (* scale (point-x p1))
;; 			  (* scale (point-y p1))
;; 			  (* scale (point-x p2))
;; 			  (* scale (point-y p2))
;;                           cstring)))
  
;;   (dolist (pt (svg-points img))
;;     (let* ((color (point-color pt))
;;            (cstring (if color
;;                         (to-svgstring color)
;;                       "green")))
;;       (format stream "<circle cx=\"~f\" cy=\"~f\" r=\"3\" stroke=\"~a\" stroke-width=\"2\" fill=\"~a\"/>" (* scale (point-x pt)) (* (point-y pt)) cstring cstring)))

;;   (format stream "</svg>"))

;; (defun to-file-name (img fname &key (scale 1.0) (width 800) (height 600))
;;   (with-open-file (stream fname :direction :output :if-exists :supersede :if-does-not-exist :create)
;; 				  (to-file img :stream stream :scale scale
;;                                            :width width :height height)))
