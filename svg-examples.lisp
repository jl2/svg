;; svg.lisp

(defpackage #:svg-examples
  (:use #:cl #:3d-vectors)
  (:export
   #:start-examples
   #:mandel-example
   #:polygon-example
   ))

(in-package #:svg-examples)

(defun start-examples ()
  (let ((easy-handler (make-instance 'hunchentoot:easy-acceptor :port 4242)))
    (hunchentoot:start easy-handler)
    (format t "Visit:~%~{~a~}" (mapcar #'car hunchentoot::*easy-handler-alist*))
    easy-handler))

(hunchentoot:define-easy-handler (gensvg :uri "/") ()
  (setf (hunchentoot:content-type*) "image/svg+xml")
  (with-output-to-string (outf)
    (svg:with-svg (outf 1200 1200 :default-stroke-width 0.0015)
      (let ((count (+ 180 (random 180)))
            (center (vec2 0 0))
            (sides (+ 3 (random 7)))
            (spins (1+ (random 5.0)))
            (offset (random (/ pi 2)))
            (step (1+ (random 5))))
        (loop :for i :below count :by step :do
          (svg:regular-polygon
           outf center sides (- 1.0 (/ i count))
           :angle-offset (+ offset (* i (/ pi (/ count spins))))
           :stroke-color (vec4 0 0.8 0 1)
           :fill-color (vec4 (random 1.0) (+ 0.2 (random 0.5)) (random 1.0) 0.25)))))))

(defun complex-to-vector (num)
  (vec2 (realpart num) (imagpart num)))

(defun random-complex (&optional (width 5.0) (height 5.0))
  (complex (- (/ width 2.0) (random width))
           (- (/ height 2.0) (random height))))

(defun mandel-iterate (z &optional (c 0.0) (exp 2.0))
  (+ c (expt z exp)))

(hunchentoot:define-easy-handler (mandelsvg :uri "/mandel") ()
  (setf (hunchentoot:content-type*) "image/svg+xml")
  (with-output-to-string (outf)
    (let ((c (random-complex 3.8 3.80))
          (exp (+ 1.0  (random pi))))
      (svg:with-svg (outf
                     2048 2048
                     :default-stroke-width 0.000015
                     :view-min (vec2 -2.5 -2.5)
                     :view-width (vec2 5.0 5.0)
                     :title (format nil "C: ~a exp: ~a" c exp))
        (loop
          :with xmin = -1.25
          :with xmax = 1.25
          :with xcount = 8
          :with dx = (/ (- xmax xmin)
                        xcount)
          :for i :below xcount
          :for curx = (+ xmin (* dx i))
          :do
             (loop
               :with ymin = -1.25
               :with ymax = 1.25
               :with ycount = 256
               :with dy = (/ (- ymax ymin)
                             ycount)
               :for j :below ycount
               :for cury = (+ ymin (* dy j))
               :for pt = (complex curx cury)
               
               :do
                  (loop
                    :with alpha = 0.2
                    :for mpt = pt :then goes-to
                    :for i :below 9
                    :while (< (abs mpt) 4.0)
                    :for goes-to = (mandel-iterate mpt c exp)
                      :then (mandel-iterate goes-to c exp)
                    :with green = (random 0.2)
                    :for color :in (list (vec4 (random 0.2) green (random 1.0) alpha)
                                         (vec4 (random 0.3) green (random 0.9) alpha)
                                         (vec4 (random 0.4) green (random 0.8) alpha)
                                         (vec4 (random 0.5) green (random 0.7) alpha)
                                         (vec4 (random 0.6) green (random 0.6) alpha)
                                         (vec4 (random 0.7) green (random 0.5) alpha)
                                         (vec4 (random 0.8) green (random 0.4) alpha)
                                         (vec4 (random 0.9) green (random 0.3) alpha)
                                         (vec4 (random 1.0) green (random 0.2) alpha))
                    :do
                       (svg:line outf
                                 (complex-to-vector mpt)
                                 (complex-to-vector goes-to)
                                 :stroke-width 0.002
                                 :stroke-color color)

                    )
               ))
        (svg:circle outf (complex-to-vector c) (/ 2.5 120) :fill-color (vec4 1.0 0.0 0.0 0.5))
        ;; (let* ((count 3600))
        ;;   (loop
        ;;     :for pts = (sort (loop :for pt = (+ (random-complex)) :then (+ (mandel-iterate pt))
        ;;                            :for j :below 3
        ;;                            :collecting pt)
        ;;                      #'<
        ;;                      :key #'abs)
        ;;     :for i :below count :by 1
        ;;     :do
        ;;        (svg:polygon outf (mapcar #'complex-to-vector pts)
        ;;                     :stroke-color (vec4 (random 0.2) (random 0.2) (random 0.2) 0.8)
        ;;                     :stroke-width 0.0014
        ;;                     :fill-color (vec4 (random 0.5) (+ 0.2 (random 0.125)) (random 0.5) 0.000125)
        ;;                     )))
        ))))

(hunchentoot:define-easy-handler (testsvg :uri "/test") ()
  (setf (hunchentoot:content-type*) "image/svg+xml")
  (with-output-to-string (outf)
    (svg:with-svg (outf
                   600 600
                   :default-stroke-width 0.0015
                   :view-min (vec2 -1.5 -1.5)
                   :view-width (vec2 3.0 3.0))
      (let* ((count 100))
        (loop
          :for i :below count :by 1
          :for pt1 = (random-complex)
          :for pt2 = (mandel-iterate pt1)
          :for pt3 = (mandel-iterate pt2 )
          :for pt4 = (mandel-iterate pt3 )
          :for pt5 = (mandel-iterate pt4 )
          :for pt6 = (mandel-iterate pt5)
          :do
             (svg:polygon outf (mapcar #'complex-to-vector
                                       (list pt1 pt2 pt3))
                          :stroke-color (vec4 (random 1.0) (+ 0.2 (random 0.5)) (random 1.0) 0.25)
                          :stroke-width 0.014
                          :fill-color (vec4 (random 1.0) (+ 0.2 (random 0.5)) (random 1.0) 0.125)
                          ))))))

