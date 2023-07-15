(in-package #:low-battery)

(defvar *game*)

(s:defsketch game ((camera (make-instance 'camera))
                   (room :main)
                   (editing nil)
                   (hero (make-hero))
                   (animating nil))
  (let ((*editing* editing)
        (*room* room)
        (*camera* camera)
        (*hero* hero)
        (*game* s::*sketch*))
    (if *editing*
        (draw-editing s::*sketch* s:width s:height)
        (draw-room s:width s:height))))

(defun go-to-room (room)
  (setf (game-room *game*) room
        (hero-room *hero*) room))

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
       (decf (height (game-camera game))))
      ((:scancode-w :scancode-up)
       (move-up))
      ((:scancode-s :scancode-down)
       (move-down))
      ((:scancode-a :scancode-left)
       (move-left))
      ((:scancode-d :scancode-right)
       (move-right))
      ((:scancode-e :scancode-space)
       (interact)))))

(defmethod kit.sdl2:mousebutton-event :around ((game game) state ts button x y)
  (let ((*editing* (game-editing game))
        (*room* (game-room game))
        (*camera* (game-camera game))
        (*hero* (game-hero game))
        (*game* game))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event :around ((game game) state ts rep? keysym)
  (let ((*editing* (game-editing game))
        (*room* (game-room game))
        (*camera* (game-camera game))
        (*hero* (game-hero game))
        (*game* game))
    (call-next-method)))

(s:define-start-function (start) game (:resizable t)
  (:start (load-rooms)))
