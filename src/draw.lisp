(in-package #:low-battery)

(defparameter *order* (list :platform :ice :battery :portal :update :home))
(defparameter *order-2* (list :text))

(defparameter *with-images* t)

(let ((font))
  (defun s::make-default-font ()
    (setf font (or font
                   (s:make-font :face (s:load-resource (uiop:native-namestring (data-path "font/PromptFont.ttf")))
                                :color s:+black+
                                :size 18)))))

(let ((font))
  (defun s::make-error-font ()
    (setf font (or font
                   (s:make-font :face (s:load-resource (uiop:native-namestring (data-path "font/PromptFont.ttf")))
                                :color s:+red+
                                :size 16)))))

(defun draw-hero (&aux (iter 0))
  (with-cell-focus ((x *hero*) (y *hero*))
    (when (animate *hero*)
      (case (car (animate *hero*))
        (:skiss (s+:enable-scissor 0 0 (* *unit* (- 1 (cadr (animate *hero*)))) *unit*))
        (:move (setf iter (mod (floor (cadr (animate *hero*)) 1/3) 3)))))
    (if *with-images*
        (progn
          (s:image (s:load-resource (pic (format nil "hero-~a.png" iter)))
                   -25 -25 (+ 50 *unit*) (+ 50 *unit*))
          (s:with-font (s:make-font :color s:+red+ :size 20 :align :center)
            (s:text (format nil "~2,'0D%" (charge *hero*))
                    50 35)))
        (s+:with-color (s:+magenta+)
          (s:ellipse (/ *unit* 2) (/ *unit* 2) (/ *unit* 2) (/ *unit* 3)))))
  (let ((*order* *order-2*)
        (*editing* nil))
    (do-accessible-cells (x y)
      (with-cell-focus (x y)
        (draw-cell (cell x y) x y))))
  (with-cell-focus ((x *hero*) (y *hero*))
    (animate-change))
  (s+:disable-scissor))

(defun draw-cell (items x y)
  (when (and *editing* (numberp x) (numberp y))
    (sb:binds (sb:brect 0 0 *unit* *unit*)
      :press (lambda (b) (edit-cell x y b)))
    (s+:with-color (s:+black+ :stroke)
      (s:rect 10 10 80 80)))
  (loop for thing in *order*
        for (found . arg) = (find thing items :key #'car)
        when found
        do (case thing
             (:platform
              (if *with-images*
                  (s:image (s:load-resource (pic (format nil "~(~a~)-platform.png"
                                                         (if (eq *room* :main)
                                                             :warm :cold))))
                           -5 -5 110 110)
                  (s+:with-color (s:+white+)
                    (s:rect 0 0 *unit* *unit*))))
             (:ice
              (if *with-images*
                  (s:image (s:load-resource (pic "snow.png"))
                           3 3 94 94)
                  (s+:with-color (s:+blue+)
                    (s:circle (/ *unit* 2) (/ *unit* 2) (/ *unit* 2)))))
             (:battery
              (if *with-images*
                  (progn
                    (s+:with-color (s:+black+)
                      (s:rect 15 35 60 30))
                    (s:image (s:load-resource (pic (format nil "battery-~a.png" arg)))
                             0 0 *unit* *unit*))
                  (progn
                    (s:rect 10 30 80 60)
                    (s+:with-color (s:+green+)
                      (s:rect 20 35 (* 20 arg) 50)))))
             (:text
              (if *with-images*
                  (progn
                    (s+:with-color (s:+black+)
                      (s:rect 7 57 35 25)
                      (s:rect 20 82 10 20))
                    (s:image (s:load-resource (pic "info.png"))
                             0 (/ *unit* 2) (/ *unit* 2) (/ *unit* 2)))
                  (s:text "info" 0 0)))
             (:portal
              (if *with-images*
                  (s:image (s:load-resource (pic (format nil "portal-~(~a~).png"
                                                         (if (member (list *room* x y) *portals-on* :test #'equal)
                                                             :on
                                                             :off))))
                           0 0 *unit* *unit*)
                  (s:text "P" (/ *unit* 2) (/ *unit* 2))))
             (:update
              (let* ((xx 1/2)
                     (yy 20)
                     (v (abs (1- (mod (* xx (sc:time (game-clock *game*))) 2))))
                     (dy (* yy (easing:in-out-sine v))))
                (s:with-font (s:make-font :size 25 :align :center :color
                                          (if (member (list *room* x y) (updates *hero*) :test #'equal)
                                              (s:gray 0.4)
                                              s:+red+))
                  (s:text (format nil "+~2,'0D%" arg) 50 (+ 30 dy)))))
             #+nil
             (:home
              (s:text "HOME" (/ *unit* 2) (/ *unit* 2)))
             #+nil
             (:function
              (funcall arg)))))

(defun make-cell-line (x y)
  (let ((text (find :text (cell x y) :key #'car)))
    (when text
      (cdr text))))

(defun draw-room (width height)
  (let ((w (* *unit* (width *camera*)))
        (h (* *unit* (+ 2 (height *camera*)))))
    (s+:with-fit (w h width height)
      (s:with-pen (s:make-pen)
        (s+:with-scissor (0 0 w h)
          (s:background (s:gray 0.5))
          (s+:with-translate (0 *unit*)
            (s+:with-scissor (0 0 w (- h (* 2 *unit*)))
              (s:background s:+black+)
              (when (game-animating *game*)
                (funcall (game-animating *game*)))
              (s:background (s:gray 0.2))
              (do-accessible-cells (x y)
                (with-cell-focus (x y)
                  (draw-cell (cell x y) x y)))
              (draw-hero)))))
      (s:with-font (s:make-font :size 20 :align :left)
        (s:text (format nil "BATTERY: ~2,'0D%" (charge *hero*)) 0 0)
        (s:text (format nil "BATTERY: ~2,'0D%" (charge *hero*)) 0 (- h *unit*)))
      (s:with-font (s:make-font :size 20 :align :right)
        (sb:binds (sb:brect 265 2 140 25)
          :press (lambda (b) (declare (ignorable b)) (toggle-sfx)))
        (sb:binds (sb:brect 265 (+ (- h *unit*) 2) 140 25)
          :press (lambda (b) (declare (ignorable b)) (toggle-sfx)))
        (sb:binds (sb:brect 427 2 125 25)
          :release (lambda (b) (declare (ignorable b)) (kit.sdl2:close-window *game*)))
        (sb:binds (sb:brect 427 (+ (- h *unit*) 2) 125 25)
          :release (lambda (b) (declare (ignorable b)) (kit.sdl2:close-window *game*)))
        (s+:with-color ((s:gray +0.8))
          (s:rect 265 2 140 25)
          (s:rect 265 (+ (- h *unit*) 2) 140 25)
          (s:rect 427 2 125 25)
          (s:rect 427 (+ (- h *unit*) 2) 125 25))
        (s:text (if *sfx-mute* "Unmute [M]" "  Mute [M]") 400 0)
        (s:text (if *sfx-mute* "Unmute [M]" "  Mute [M]") 400 (- h *unit*))
        (s:text "Quit [ESC]" 550 0)
        (s:text "Quit [ESC]" 550 (- h *unit*)))
      (s:with-font (s:make-font :size 20 :align :right)
        (alexandria:when-let ((portal (find :portal (cell (x *hero*) (y *hero*)) :key #'car)))
          (let ((text (if (cdr portal) "[E] [space]" "[inactive]")))
            (s:text text w 0)
            (s:text text w (- h 100)))))
      (s:with-font (s:make-font :size 25 :align :center)
        (alexandria:when-let ((cell-line (make-cell-line (x *hero*) (y *hero*))))
          (s:text cell-line (/ w 2) (/ *unit* 3))
          (s:text cell-line (/ w 2) (- h (* 2/3 *unit*))))))))

(defun draw-selectors (width height)
  (s+:with-fit ((* *unit* 2) (* *unit* (ceiling (length (modeline *editing*)) 2)) width height)
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

(defmacro press-button ((x y name) &body press-body)
  `(named-button ((+ 20 (* 400 ,x)) (+ 20 (* 200 ,y)) 360 160 ,name)
     ,@press-body))

(defun draw-action-buttons (width height)
  (s+:with-fit ((* *unit* 8) (* *unit* 8) width height)
    (s:with-font (s:make-font :size 35 :align :center)
      (press-button (0 0 "Save room")
        (save-room (room-filename *room*)))
      (press-button (0 1 "Reset room")
        (load-room (room-filename *room*)))
      (when (editing-cell *editing*)
        (press-button (0 2 "Move room origin")
          (apply #'shift-room (current-room) (editing-cell *editing*))
          (decf (x *camera*) (car (editing-cell *editing*)))
          (decf (y *camera*) (cadr (editing-cell *editing*)))
          (setf (editing-cell *editing*) (list 0 0))))
      (when (and (editing-cell *editing*)
                 (ref *editing*)
                 (find :portal (apply #'cell (editing-cell *editing*)) :key #'car))
        (press-button (0 3 "Set portal destination")
          (let ((cell (find :portal (apply #'cell (editing-cell *editing*)) :key #'car)))
            (setf (cdr cell) (ref *editing*)))))
      (press-button (1 0 "DELETE ALL")
        (setf (cells (current-room)) (make-hash-table :test #'equal)))
      (when (editing-cell *editing*)
        (press-button (1 1 "Save ref")
          (setf (ref *editing*)
                (cons *room* (editing-cell *editing*)))))
      (press-button (1 2 "Next room")
        (setf (game-room *game*) (next-room))))))

(defun draw-status (width height)
  (s+:with-fit (800 800 width height)
    (s:with-font (s:make-font :size 40 :align :center)
      (s:text (format nil "#<MODE: ~S>" (mode *editing*))
              400 0)
      (s:text (format nil "#<CAMERA: ~S ~S>" (x *camera*) (y *camera*))
              400 50)
      (when (ref *editing*)
        (s:text (format nil "#<REF: ~S>" (ref *editing*))
                400 100))
      (s:text (format nil "#<ROOM: ~S>" *room*)
              400 150))
    (s:with-font (s:make-font :size 35 :align :center)
      (when (editing-cell *editing*)
        (s:text (format nil "#<CELL ~a:~%(~{~S~^~%~})>"
                        (editing-cell *editing*)
                        (loop for (thing . arg) in (apply #'cell (editing-cell *editing*))
                              unless (eq thing :text) collect (cons thing arg)
                              else collect thing and collect arg))
                400 250)))))

(defun draw-editing (game width height)
  (declare (ignorable game))
  (with-split (width height :vertical)
    (1                                  ; buttons
     (with-split (width height :horizontal)
       (1 (draw-selectors width height))
       (1 (draw-action-buttons width height))))
    (2                                  ; text & game fiels
     (with-split (width height :horizontal)
       (1 (draw-status width height))
       (2 (draw-room width height))
       (.1)))
    (.1)))
