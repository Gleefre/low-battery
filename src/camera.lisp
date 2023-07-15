(in-package #:low-battery)

(defclass camera ()
  ((x :initform 0d0 :initarg :x :accessor x)
   (y :initform 0d0 :initarg :y :accessor y)
   (width :initform 5 :initarg :width :accessor width)
   (height :initform 5 :initarg :height :accessor height)
   ;;(angle :initform 0 :initarg :angle :accessor angle)
   ))

(defvar *camera*)

(defmacro with-camera ((&optional (camera '*camera*)) &body body)
  `(let ((*camera* ,camera))
     ,@body))

;; FIXME: Add support for angle
(defmacro do-accessible-cells ((var-x var-y &optional (camera '*camera*)) &body body)
  (alexandria:with-gensyms ($min-x $max-x $min-y $max-y $x $y $camera)
    `(let ((,$camera ,camera))
       (let ((,$min-x (floor (- (x ,$camera) (/ (width ,$camera) 2))))
             (,$max-x (ceiling (+ (x ,$camera) (/ (width ,$camera) 2))))
             (,$min-y (floor (- (y ,$camera) (/ (height ,$camera) 2))))
             (,$max-y (ceiling (+ (y ,$camera) (/ (height ,$camera) 2)))))
         (loop for ,$x from ,$min-x to ,$max-x
               do (loop for ,$y from ,$min-y to ,$max-y
                        do (let ((,var-x ,$x)
                                 (,var-y ,$y))
                             ,@body)))))))
