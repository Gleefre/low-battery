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
             (,$max-x (ceiling (+ 2 (x ,$camera) (/ (width ,$camera) 2))))
             (,$min-y (floor (- (y ,$camera) (/ (height ,$camera) 2))))
             (,$max-y (ceiling (+ 2 (y ,$camera) (/ (height ,$camera) 2)))))
         (loop for ,$x from ,$min-x to ,$max-x
               do (loop for ,$y from ,$min-y to ,$max-y
                        do (let ((,var-x ,$x)
                                 (,var-y ,$y))
                             ,@body)))))))

(defparameter *unit* 100)

(defparameter *order* (list :platform :ice :battery :portal :update :home))

(defparameter *editing* nil)

(defvar *mode*)

(defun edit-cell (x y button))

(defun draw-cell (items x y)
  (when *editing*
    (sb:binds (sb:brect 0 0 *unit* *unit*)
      :press (lambda (b) (edit-cell x y b))))
  (loop for thing in *order*
        for (found . arg) = (find thing items :key #'car)
        when found
        do (case thing
             (:platform
              (s+:with-color (s:+white+)
                (s:rect 0 0 *unit* *unit*)))
             (:ice
              (s+:with-color (s:+blue+)
                (s:circle (/ *unit* 2) (/ *unit* 2) (/ *unit* 2))))
             (:battery
              (s:rect 10 30 80 60)
              (s+:with-color (s:+green+)
                (s:rect 20 35 (* 20 arg) 50)))
             (:portal
              (s:text "P" (/ *unit* 2) (/ *unit* 2)))
             (:update
              (s:text "%" (/ *unit* 2) (/ *unit* 2)))
             (:home
              (s:text "HOME" (/ *unit* 2) (/ *unit* 2))))))

(defun draw-room (width height)
  (let ((w (* *unit* (width *camera*)))
        (h (* *unit* (height *camera*))))
    (s+:with-fit (w h width height)
      (s+:with-scissor (0 0 w h)
        (do-accessible-cells (x y)
          (s+:with-translate ((* *unit* (- x (x *camera*))) (* *unit* (- y (y *camera*))))
            (draw-cell (cell x y) x y)))))))

(s:defsketch game ((camera (make-instance 'camera))
                   (room :main)
                   (editing nil))
  (let ((*editing* editing))
    (with-room (room)
      (with-camera (camera)
        (draw-room s:width s:height)))))

(defmethod kit.sdl2:keyboard-event ((game game) state ts rep? keysym)
  (when (eq state :keydown)
    (case (sdl2:scancode keysym)
      (:scancode-kp-plus (setf (game-editing game)
                               (not (game-editing game))))
      (:scancode-kp-8
       (when (game-editing game)
         (incf (y (game-camera game)) 1/2)))
      (:scancode-kp-2
       (when (game-editing game)
         (decf (y (game-camera game)) 1/2)))
      (:scancode-kp-4
       (when (game-editing game)
         (incf (x (game-camera game)) 1/2)))
      (:scancode-kp-6
       (when (game-editing game)
         (decf (x (game-camera game)) 1/2))))))
