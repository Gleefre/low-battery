(in-package #:low-battery)

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
