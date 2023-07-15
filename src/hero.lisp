(in-package #:low-battery)

(defvar *hero*)

(defparameter *hero-width* 2)
(defparameter *hero-height* 2)

(defparameter *start-charge* 2)

(defclass hero ()
  ((room :initform :main :initarg :room :accessor hero-room)
   (x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)
   (width :initform *hero-width* :initarg :width :accessor width)
   (height :initform *hero-height* :initarg :height :accessor height)
   (charge :initform *start-charge* :initarg :charge :accessor charge)
   (max-charge :initform *start-charge* :initarg :max-charge :accessor max-charge)
   (updates :initform () :accessor updates)
   (last-portal :initform () :initarg last-portal :accessor last-portal)
   (animate :initform nil :accessor animate)))

(defun make-hero ()
  (make-instance 'hero))

(defun fit-camera-to-hero ()
  (alexandria:maxf (x *camera*) (- (x *hero*) (/ (width *hero*) 2)))
  (alexandria:minf (x *camera*) (+ (x *hero*) (/ (width *hero*) 2)))
  (alexandria:maxf (y *camera*) (- (y *hero*) (/ (height *hero*) 2)))
  (alexandria:minf (y *camera*) (+ (y *hero*) (/ (height *hero*) 2))))

(defun update-hero (&optional (rec nil)
                    &aux (x (x *hero*)) (y (y *hero*)) (cell (cell x y)))
  (serapeum:bcond
    ((alexandria:when-let ((battery (find :battery cell :key #'car)))
       (when (plusp (cdr battery))
         (prog1 (cdr battery)
           (setf (cdr battery) 0))))
     :=> charge (setf (charge *hero*)
                      (min (+ (charge *hero*) charge)
                           (max-charge *hero*))))
    ((or (member *room* '(:main :home))
         (find :home cell :key #'car)
         (find :portal cell :key #'car)
         rec)
     (setf (charge *hero*) (max-charge *hero*)))
    ((and (find :ice cell :key #'car)
          (not rec))
     (decf (charge *hero*) 2))
    ((not rec)
     (decf (charge *hero*))))
  (alexandria:when-let ((update (find :update cell :key #'car)))
    (unless (member (list *room* x y) (updates *hero*))
      (push (list *room* x y) (updates *hero*))
      (incf (max-charge *hero*) (cdr update))
      (incf (charge *hero*) (cdr update))))
  (unless (plusp (charge *hero*))
    (when (last-portal *hero*)
      (portal-to (last-portal *hero*)))))

(defmacro animating ((var &rest clock-args &key &allow-other-keys) &body body)
  `(let ((animating-clock (sc:make-clock ,@clock-args)))
     (setf (game-animating *game*)
           (lambda (&aux (,var (sc:time animating-clock)))
             ,@body))))

(defun animated-move (dx dy)
  (let ((x (x *hero*))
        (y (y *hero*)))
    (animating (v :time-source (game-clock *game*) :speed 2)
      (if (>= v 0.99)
          (progn
            (setf (game-animating *game*) nil
                  (animate *hero*) nil
                  (x *hero*) (+ x dx)
                  (y *hero*) (+ y dy))
            (update-hero))
          (setf (x *hero*) (+ x (* dx v))
                (y *hero*) (+ y (* dy v))
                (animate *hero*) (list :move v)))
      (fit-camera-to-hero))))

(defparameter *portals-on* nil)

(defparameter *last-views* (make-hash-table :test #'equal))

(defun ref-view (ref)
  (gethash ref *last-views*))

(defun (setf ref-view) (new-view ref)
  (setf (gethash ref *last-views*) new-view))

(defun view (room x y)
  (ref-view (list room x y)))

(defun (setf view) (new-view room x y)
  (setf (ref-view (list room x y)) new-view))

(defun portal-to (ref)
  (setf *portals-on* t)
  (setf (view *room* (x *hero*) (y *hero*))
        (list (x *camera*) (y *camera*)))
  (let ((x (* *unit* (- (x *hero*) (- (x *camera*)
                                      (/ (width *camera*) 2)))))
        (y (* *unit* (- (y *hero*) (- (y *camera*)
                                      (/ (height *camera*) 2)))))
        (w (* *unit* (width *camera*)))
        (h (* *unit* (height *camera*))))
    (animating (v :time-source (game-clock *game*) :speed 1)
      (s+:enable-scissor (alexandria:lerp v 0 (- x (/ *unit* 2)))
                         (alexandria:lerp v 0 (- y (/ *unit* 2)))
                         (- (alexandria:lerp v w (+ x (/ *unit* 2)))
                            (alexandria:lerp v 0 (- x (/ *unit* 2))))
                         (- (alexandria:lerp v h (+ y (/ *unit* 2)))
                            (alexandria:lerp v 0 (- y (/ *unit* 2)))))
      (when (>= v 1)
        (animating (v :time-source (game-clock *game*) :speed 2)
          (s+:enable-scissor (- x (/ *unit* 2)) (- y (/ *unit* 2)) *unit* *unit*)
          (setf (animate *hero*) (list :skiss v))
          (when (>= v 1)
            (animating (v :time-source (game-clock *game*) :speed 4)
              (s+:enable-scissor (alexandria:lerp v (- x (/ *unit* 2)) x)
                                 (alexandria:lerp v (- y (/ *unit* 2)) y)
                                 (alexandria:lerp v *unit* 0)
                                 (alexandria:lerp v *unit* 0))
              (when (>= v 1)
                (go-to-room (car ref))
                (setf (x *camera*) (or (car (ref-view ref)) (cadr ref))
                      (y *camera*) (or (cadr (ref-view ref)) (caddr ref))
                      (x *hero*) (cadr ref)
                      (y *hero*) (caddr ref)
                      x (* *unit* (- (x *hero*) (- (x *camera*)
                                                   (/ (width *camera*) 2))))
                      y (* *unit* (- (y *hero*) (- (y *camera*)
                                                   (/ (height *camera*) 2)))))
                (update-hero T)
                (setf (last-portal *hero*) ref)
                (unless (member *room* '(:main :home))
                  (load-room (room-filename *room*)))
                (animating (v :time-source (game-clock *game*) :speed 4)
                  (s+:enable-scissor (alexandria:lerp v x (- x (/ *unit* 2)))
                                     (alexandria:lerp v y (- y (/ *unit* 2)))
                                     (alexandria:lerp v 0 *unit*)
                                     (alexandria:lerp v 0 *unit*))
                  (when (>= v 1)
                    (animating (v :time-source (game-clock *game*) :speed 2)
                      (s+:enable-scissor (- x (/ *unit* 2)) (- y (/ *unit* 2)) *unit* *unit*)
                      (setf (animate *hero*) (list :skiss (- 1 v)))
                      (when (>= v 1)
                        (setf (animate *hero*) nil)
                        (animating (v :time-source (game-clock *game*) :speed 1)
                          (s+:enable-scissor (alexandria:lerp v (- x (/ *unit* 2)) 0)
                                             (alexandria:lerp v (- y (/ *unit* 2)) 0)
                                             (- (alexandria:lerp v (+ x (/ *unit* 2)) w)
                                                (alexandria:lerp v (- x (/ *unit* 2)) 0))
                                             (- (alexandria:lerp v (+ y (/ *unit* 2)) h)
                                                (alexandria:lerp v (- y (/ *unit* 2)) 0)))
                          (when (>= v 1)
                            (setf (game-animating *game*) nil
                                  *portals-on* nil)))))))))))))))

(defun to-last-portal ()
  (alexandria:when-let ((portal (last-portal *hero*)))
    (portal-to portal)))

(defun move (dx dy &aux (x (x *hero*)) (y (y *hero*)))
  (when (find :platform (cell (+ x dx) (+ y dy)) :key #'car)
    (animated-move dx dy)))

(defun move-up ()
  (move 0 -1))

(defun move-down ()
  (move 0 1))

(defun move-left ()
  (move -1 0))

(defun move-right ()
  (move 1 0))

(defun interact (&aux (x (x *hero*)) (y (y *hero*)))
  (alexandria:when-let* ((portal (find :portal (cell x y) :key #'car))
                         (ref (cdr portal)))
    (portal-to ref)))
