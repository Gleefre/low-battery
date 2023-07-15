(in-package #:low-battery)

(defparameter *unit* 100)

(defparameter *order* (list :platform :ice :battery :portal :update :home))

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
          (s:text "Move to cell" 50 110)
          (s:rect 105 105 90 40)
          (sb:binds (sb:brect 105 105 90 40)
            :press (lambda (b)
                     (declare (ignorable b))
                     (setf (game-room game) (next-room))))
          (s:text "Next room" 150 110)))))
  (s+:with-fit ((* *unit* 2) (* *unit* (ceiling (length (modeline *editing*)) 2)) (/ width 2) (/ height 2))
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
             (s:translate dx dy)))
  (s+:with-translate (0 (/ height 2))
    (s:with-font (s:make-font :size (floor (min height width) 30) :align :center)
      (s:text (format nil "#<MODE: ~S>" (mode *editing*))
              (/ width 4) 0)
      (s:text (format nil "#<CAMERA: ~S ~S>" (x *camera*) (y *camera*))
              (/ width 4) 40)
      (when (editing-cell *editing*)
        (let ((*print-length* 10))
          (s:text (format nil "#<CELL ~a: ~S>"
                          (editing-cell *editing*)
                          (apply #'cell (editing-cell *editing*)))
                  (/ width 4) 80)))
      (when (ref *editing*)
        (s:text (format nil "#<REF: ~S>" (ref *editing*))
                (/ width 4) 120))
      (s:text (format nil "#<ROOM: ~S>" *room*)
              (/ width 4) 160))))