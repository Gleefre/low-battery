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

(defparameter *unit* 100)

(defparameter *order* (list :platform :ice :battery :portal :update :home))

(defparameter *editing* nil)

(defclass editing ()
  ((modeline :initform (list (cons :platform nil)
                             (cons :ice nil)
                             (cons :battery 1)
                             (cons :portal nil)
                             (cons :update nil)
                             (cons :home nil))
             :accessor modeline)
   (mode :initform :select :accessor mode)
   (cell :initform nil :accessor editing-cell)
   (ref :initform nil :accessor ref)))

(defun edit-cell (x y button)
  (when *editing*
    (if (eq (mode *editing*) :select)
        (setf (editing-cell *editing*) (list x y))
        (let ((mode (nth (mode *editing*) (modeline *editing*))))
          (case (car mode)
            ((:platform :ice :portal :update :home)
             (if (member (car mode) (cell x y) :key #'car)
                 (alexandria:deletef (cell x y) (car mode) :key #'car)
                 (push (cons (car mode) nil) (cell x y))))
            (:battery
             (let ((cell (find (car mode) (cell x y) :key #'car)))
               (if cell
                   (if (= 3 (cdr cell))
                       (alexandria:deletef (cell x y) (car mode) :key #'car)
                       (incf (cdr cell)))
                   (push (cons (car mode) 1) (cell x y))))))))))

(defun draw-cell (items x y)
  (when *editing*
    (sb:binds (sb:brect 0 0 *unit* *unit*)
      :press (lambda (b) (edit-cell x y b)))
    (s+:with-color (s:+black+ :stroke)
      (s:rect 10 10 80 80))
    (when items
      (s+:with-color (s:+yellow+)
        (s:rect 10 10 80 80))))
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
              (s:text "HOME" (/ *unit* 2) (/ *unit* 2)))
             (:function
              (funcall arg)))))

(defun draw-room (width height)
  (let ((w (* *unit* (width *camera*)))
        (h (* *unit* (height *camera*))))
    (s+:with-fit (w h width height)
      (s+:with-scissor (0 0 w h)
        (do-accessible-cells (x y)
          (s+:with-translate ((* *unit* (- x (- (x *camera*)
                                                (/ (width *camera*) 2))
                                           1/2))
                              (* *unit* (- y (- (y *camera*)
                                                (/ (height *camera*) 2))
                                           1/2)))
            (draw-cell (cell x y) x y)))))))

(defun update-editing-mode (n button)
  (declare (ignorable button))
  (when *editing*
    (if (eql n (mode *editing*))
        (setf (mode *editing*) :select)
        (setf (mode *editing*) n))))

(defun draw-editing (game width height)
  (declare (ignorable game))
  (s+:with-translate ((/ width 2) (/ height 2))
    (s+:with-color (s:+black+ :stroke)
      (s:rect 0 0 (* 9/10 (/ width 2)) (* 9/10 (/ height 2)))
      (draw-room (* 9/10 (/ width 2)) (* 9/10 (/ height 2)))))
  (s+:with-translate ((/ width 2) 0)
    (s+:with-fit ((* *unit* 2) (* *unit* 1) (/ width 2) (/ height 3))
      (s:with-font (s:make-font :size (floor (min height width) 50) :align :center)
        (s:rect 5 5 90 40)
        (sb:binds (sb:brect 5 5 90 40)
          :press (lambda (b)
                   (declare (ignorable b))
                   (save-room (room-filename *room*))))
        (s:text "Save room" 50 10)
        (s:rect 5 55 90 40)
        (sb:binds (sb:brect 5 55 90 40)
          :press (lambda (b)
                   (declare (ignorable b))
                   (load-room (room-filename *room*))))
        (s:text "Reset room" 50 60)
        (s:rect 105 5 90 40)
        (sb:binds (sb:brect 105 5 90 40)
          :press (lambda (b)
                   (declare (ignorable b))
                   (setf (cells (current-room)) (make-hash-table :test #'equal))))
        (s:text "DELETE ALL" 150 10)
        (when (editing-cell *editing*)
          (s:rect 105 55 90 40)
          (sb:binds (sb:brect 105 55 90 40)
            :press (lambda (b)
                     (declare (ignorable b))
                     (setf (ref *editing*)
                           (cons *room* (editing-cell *editing*)))))
          (s:text "Save ref" 150 60)
          (s:rect 5 105 90 40)
          (sb:binds (sb:brect 5 105 90 40)
            :press (lambda (b)
                     (declare (ignorable b))
                     (apply #'shift-room (current-room) (editing-cell *editing*))
                     (decf (x *camera*) (car (editing-cell *editing*)))
                     (decf (y *camera*) (cadr (editing-cell *editing*)))
                     (setf (editing-cell *editing*) (list 0 0))))
          (s:text "Move to cell" 50 110))))
    (s:with-font (s:make-font :size (floor (min height width) 30) :align :center)
      (s:text (format nil "#<MODE: ~S>" (mode *editing*))
              (/ width 4) (/ height 3))
      (s:text (format nil "#<CAMERA: ~S ~S>" (x *camera*) (y *camera*))
              (/ width 4) (+ 40 (/ height 3)))
      (when (editing-cell *editing*)
        (let ((*print-length* 10))
          (s:text (format nil "#<CELL ~a: ~S>"
                          (editing-cell *editing*)
                          (apply #'cell (editing-cell *editing*)))
                  (/ width 4) (+ 80 (/ height 3)))))
      (when (ref *editing*)
        (s:text (format nil "#<REF: ~S>" (ref *editing*))
                  (/ width 4) (+ 120 (/ height 3))))))
  (s+:with-fit ((* *unit* 2) (* *unit* (ceiling (length (modeline *editing*)) 2)) (/ width 2) height)
    (loop for thing in (modeline *editing*)
          for n from 0
          for dx = *unit* then (- dx)
          for dy = 0 then (- *unit* dy)
          do (when (eql n (mode *editing*))
               (s+:with-color (s:+magenta+)
                 (s:rect 5 5 90 90)))
             (s+:with-fit (100 100 80 80 0 0 10 10)
               (draw-cell (list thing) nil nil)
               (sb:binds (sb:brect 0 0 *unit* *unit*)
                 :press (let ((n n)) (lambda (b) (update-editing-mode n b)))))
             (s:translate dx dy))))

(s:defsketch game ((camera (make-instance 'camera))
                   (room :main)
                   (editing nil))
  (let ((*editing* editing)
        (*room* room)
        (*camera* camera))
    (if *editing*
        (draw-editing s::*sketch* s:width s:height)
        (draw-room s:width s:height))))

(defmethod kit.sdl2:keyboard-event ((game game) state ts rep? keysym)
  (when (eq state :keydown)
    (case (sdl2:scancode keysym)
      (:scancode-kp-plus
       (setf (game-editing game) (make-instance 'editing)))
      (:scancode-kp-minus
       (setf (game-editing game) nil))
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
         (decf (x (game-camera game)) 1/2)))
      (:scancode-kp-divide
       (save-room (room-filename *room*)))
      (:scancode-kp-multiply
       (load-room (room-filename *room*)))
      (:scancode-kp-7
       (incf (width (game-camera game)))
       (incf (height (game-camera game))))
      (:scancode-kp-9
       (decf (width (game-camera game)))
       (decf (height (game-camera game)))))))

(defmethod kit.sdl2:mousebutton-event :around ((game game) state ts button x y)
  (let ((*editing* (game-editing game))
        (*room* (game-room game))
        (*camera* (game-camera game)))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event :around ((game game) state ts rep? keysym)
  (let ((*editing* (game-editing game))
        (*room* (game-room game))
        (*camera* (game-camera game)))
    (call-next-method)))

(s:define-start-function (start) game (:resizable t)
  (:start (load-rooms)))
