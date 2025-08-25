;;;; svg.lisp

(in-package #:svg)

(defclass svg-context ()
  ((stream :initform nil)
   (default-stroke-width :initform 1.0)
   (default-stroke-color :initform (vec4 0 0 0 1.0))
   (default-fill-color :initform (vec4 0 0 0 1.0))
   (view-min :initform (vec2 -1.0 -1.0))
   (view-width :initform (vec2 2.0 2.0))
   (background-color :initform (vec4 1 1 1 1))))

(defparameter *default-stroke-width* 1.0)
(defparameter *default-stroke-color* (vec4 0 0 0 1.0))
(defparameter *default-fill-color* (vec4 0 0 0 1.0))
(defparameter *default-alpha* 0.5)
(defvar *default-text-style* "default")

(defun make-style (stream &key
                            (style-name "default")
                            (font-size 12)
                            (font-unit "pt")
                            (font-face "sans-serif")
                            (fill *default-fill-color*)
                            (stroke *default-stroke-color*))
  (format stream ".~a {
  font: ~a~a ~a;
  fill: ~a;
  stroke: ~a;
}" style-name font-size font-unit font-face (color nil fill) (color nil stroke)))

(defun color (stream color)
  "Write an SVG color to stream."
  (declare (type (or string list vec3 vec4) color))
  (etypecase color
    (vec4
     (format stream "rgba(~d,~d,~d,~f)"
             (truncate (* 255 (vx color)))
             (truncate (* 255 (vy color)))
             (truncate (* 255 (vz color)))
             (vw color)))
    (vec3
     (format stream "rgba(~d,~d,~d,~f)"
             (truncate (* 255 (vx color)))
             (truncate (* 255 (vy color)))
             (truncate (* 255 (vz color)))
             *default-alpha*))
    (list
     (format stream "rgba(~{~a~^,~}, ~a)"
             (mapcar (lambda (x) (* 255 x)) (subseq color 0 3))
             (if (= (length color) 4)
                 (fourth color)
                 *default-alpha*))
     )
    (string
     (format stream "~a" color))))

(defun rectangle (stream pt size
                  &key
                    (rounded nil)
                    (stroke-width *default-stroke-width*)
                    (stroke-color *default-stroke-color*)
                    (fill-color *default-fill-color*))
  "Write an SVG rectangle to stream."
  (format stream
          "<rect x=\"~f\" y=\"~f\" width=\"~f\" height=\"~f\" ~a stroke=~s stroke-width=\"~f\" fill=~s/>~%"
          (vx pt) (vy pt)
          (vx size) (vy size)
          (if rounded
              (format nil "rx=\"~f\" ry=\"~f\"" (vx rounded) (vy rounded))
              "")
          (color nil stroke-color)
          stroke-width
          (color nil fill-color)))

(defun ellipse (stream pt size
                &key
                  (stroke-width *default-stroke-width*)
                  (stroke-color *default-stroke-color*)
                  (fill-color *default-fill-color*))
  "Write an SVG ellipse to stream."
  (format stream
          "<ellipse cx=\"~f\" cy=\"~f\" rx=\"~f\" ry=\"~f\" stroke=~s stroke-width=\"~f\" fill=~s/>~%"
          (vx pt) (vy pt)
          (vx size) (vy size)
          (color nil stroke-color)
          stroke-width
          (color nil fill-color)))

(defun circle (stream center radius
               &key
                 (stroke-width *default-stroke-width*)
                 (stroke-color *default-stroke-color*)
                 (fill-color *default-fill-color*))
  "Write an SVG circle to stream."
  (format stream
          "<circle cx=\"~f\" cy=\"~f\" r=\"~f\" stroke=~s stroke-width=\"~f\" fill=~s/>~%"
          (vx center) (vy center)
          radius
          (color nil stroke-color)
          stroke-width
          (color nil fill-color)))

(defun default-style (stream)
  (format stream "<style>~a</style>~%" *default-text-style*))

(defun style (stream style-text)
  (format stream "<style>~a</style>~%" style-text))

(defun text (stream position text &key
                                    (style *default-text-style*)
                                    (flip-y t)
                                    (view-width nil))
  (when flip-y
    (format stream "<g transform=\"matrix(1 0 0 -1 0 ~a)\">~%" (vy view-width)))
  (format stream
          "<text x=\"~a\" y=\"~a\" class=\"~a\">~a</text>~%"
          (vx position)
          (vy position)
          style
          text)
  (when flip-y
    (format stream "</g>~%")))

(defun polyline (stream points
                 &key
                   (stroke-width *default-stroke-width*)
                   (stroke-color *default-stroke-color*))
  "Write an SVG polyline to stream."
  (format stream
          "<polyline points=\"")
  (loop
    :for pt :in points :do
      (format stream "~f ~f "
              (vx pt) (vy pt)))
  (format stream "\" stroke=~s stroke-width=\"~f\"/>"
          (color nil stroke-color)
          stroke-width))

(defun line (stream pt1 pt2
             &key
               (stroke-width *default-stroke-width*)
               (stroke-color *default-stroke-color*))
  "Write an SVG line to stream."
  (format stream
          "<line x1=\"~f\" y1=\"~f\" x2=\"~f\" y2=\"~f\" stroke=~s stroke-width=\"~f\"/>"
          (vx pt1) (vy pt1)
          (vx pt2) (vy pt2)
          (color nil stroke-color)
          stroke-width))

(defun polygon (stream points
                &key
                  (stroke-width *default-stroke-width*)
                  (stroke-color *default-stroke-color*)
                  (fill-color *default-fill-color*))
  "Write an SVG polygon to stream."
  (format stream
          "<polygon points=\"")
  (etypecase points
    (list (loop
            :for pt :in points :do
              (format stream "~f ~f "
                      (vx pt) (vy pt))))
    (t (loop
         :for pt :across points :do
           (format stream "~f ~f "
                   (vx pt) (vy pt)))))
  
  (format stream "\" stroke=~s stroke-width=\"~f\" fill=~s/>"
          (color nil stroke-color)
          stroke-width
          (color nil fill-color)))

(defun regular-polygon (stream center side-count radius
                        &key
                          (angle-offset 0.0)
                          (stroke-width *default-stroke-width*)
                          (stroke-color *default-stroke-color*)
                          (fill-color *default-fill-color*))
  "Write an SVG polygon to stream."
  (format stream
          "<polygon points=\"")
  (let ((angle-increment (/ (* 2 pi) side-count)))
    (loop
      :for side :below side-count
      :for theta = (+ angle-offset (* side angle-increment))
      :do
         (format stream "~f ~f "
                 (+ (vx center) (* radius (cos theta)))
                 (+ (vy center) (* radius (sin theta))))))
  (format stream "\" stroke=~s stroke-width=\"~f\" fill=~s/>"
          (color nil stroke-color)
          stroke-width
          (color nil fill-color)))

(defun star (stream center point-count outter-radius inner-radius
                        &key
                          (angle-offset 0.0)
                          (stroke-width *default-stroke-width*)
                          (stroke-color *default-stroke-color*)
                          (fill-color *default-fill-color*))
  "Write an SVG polygon star to stream."
  (format stream
          "<polygon points=\"")
  (let ((angle-increment (/ (* 2 pi) point-count))
        (half-angle-increment (/ pi point-count)))
    (loop
      :for point :below point-count
      :for theta = (+ angle-offset (* point angle-increment))
      :for theta-two = (+ angle-offset half-angle-increment
                          (* point angle-increment))
      :do
         (format stream "~f ~f "
                 (+ (vx center) (* outter-radius (cos theta)))
                 (+ (vy center) (* outter-radius (sin theta))))
         (format stream "~f ~f "
                 (+ (vx center) (* inner-radius (cos theta-two)))
                 (+ (vy center) (* inner-radius (sin theta-two))))))
  (format stream "\" stroke=~s stroke-width=\"~f\" fill=~s/>"
          (color nil stroke-color)
          stroke-width
          (color nil fill-color)))

(defun view-box (stream view-min view-width)
  "Write an SVG polygon to stream."
  (cond ((and (and view-min view-width) (or view-min view-width))
         (format stream "viewBox=\"~f ~f ~f ~f\""
                 (vx view-min)
                 (vy view-min)
                 (vx view-width)
                 (vy view-width)))
        ((and (null view-min) (null view-width))
         "")
        (t
         (error "view-min and view-max must both be specified or both nil"))))

(defun begin-svg (stream width height
                  &key
                    (view-min (vec2 -1.0 -1.0))
                    (view-width (vec2 2.0 2.0))
                    (fill (vec4 1 1 1 1))
                    (title nil)
                    (include-default-text-style t)
                    (flip-y nil))
  "Write the beginning of an SVG file to stream."
  (format stream "~
<?xml version=\"1.0\" standalone=\"no\"?>~%<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">~%
<svg width=\"~d\" height=\"~d\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" ~a fill=~s >~%"
          width
          height
          (view-box nil view-min view-width)
          (color nil fill))
  (when title
    (format stream "<title>~a</title>~%" title))
  (when include-default-text-style
    (style stream (make-style nil)))
  (when flip-y
    (format stream "<g transform=\"matrix(1 0 0 -1 0 ~a)\">~%" (vy view-width)))
  (when flip-y))

(defun end-svg (stream flip-y)
  "Write the end of an SVG file to stream."
  (when flip-y
    (format stream "</g>"))
  (format stream "</svg>~%"))

(defmacro with-svg ((stream width height
                            &key
                              (view-min (vec2 -1.0 -1.0))
                              (view-width (vec2 2.0 2.0))
                              (default-text-style nil)
                              (default-stroke-color nil)
                              (default-stroke-width nil)
                              (default-fill-color nil)
                              (title nil)
                              (flip-y nil))
                    
                    &body body)
  `(unwind-protect
        (progn
          (let ((*default-stroke-width* (if ,default-stroke-width
                                            ,default-stroke-width
                                            *default-stroke-width*))
                (*default-stroke-color* (if ,default-stroke-color
                                            ,default-stroke-color
                                            *default-stroke-color*))
                (*default-fill-color* (if ,default-fill-color
                                          ,default-fill-color
                                          *default-fill-color*))
                (*default-text-style* (if ,default-text-style
                                          ,default-text-style
                                          *default-text-style*)))
            (begin-svg ,stream ,width ,height
                       :view-min ,view-min
                       :view-width ,view-width
                       :title ,title
                       :flip-y ,flip-y
                       :include-default-text-style t)
            ,@body)
          (end-svg ,stream ,flip-y))))
