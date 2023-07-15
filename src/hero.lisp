(in-package #:low-battery)

(defvar *hero*)

(defclass hero ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor x)
   (updates :initform () :accessor updates)))

(defun make-hero ()
  (make-instance 'hero))



(defun move-up ())
(defun move-down ())
(defun move-left ())
(defun move-right ())
(defun interact ())
