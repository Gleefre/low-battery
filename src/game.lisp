(in-package #:low-battery)

(defvar *game*)

(s:defsketch game ((camera (make-instance 'camera))
                   (room :main)
                   (editing nil)
                   (hero (make-hero))
                   (animating nil)
                   (clock (sc:make-clock :speed 3/2)))
  (let ((*editing* editing)
        (*room* room)
        (*camera* camera)
        (*hero* hero)
        (*game* s::*sketch*))
    (unless *editing*
      (s:background s:+black+))
    (if *editing*
        (draw-editing s::*sketch* s:width s:height)
        (draw-room s:width s:height))))

(defun go-to-room (room)
  (setf (game-room *game*) room
        (hero-room *hero*) room
        *room* room))

(defmethod kit.sdl2:keyboard-event ((game game) state ts rep? keysym)
  (when (eq state :keydown)
    (case (sdl2:scancode keysym)
      #-deploy
      (:scancode-kp-plus
       (setf (game-editing game) (make-instance 'editing)))
      #-deploy
      (:scancode-kp-minus
       (setf (game-editing game) nil))
      (:scancode-kp-8
       (when (game-editing game)
         (decf (y (game-camera game)) 1/2)))
      (:scancode-kp-2
       (when (game-editing game)
         (incf (y (game-camera game)) 1/2)))
      (:scancode-kp-4
       (when (game-editing game)
         (decf (x (game-camera game)) 1/2)))
      (:scancode-kp-6
       (when (game-editing game)
         (incf (x (game-camera game)) 1/2)))
      (:scancode-kp-divide
       (when (game-editing game)
         (save-room (room-filename *room*))))
      (:scancode-kp-multiply
       (when (game-editing game)
         (load-room (room-filename *room*))))
      (:scancode-kp-7
       (when (game-editing game)
         (incf (width (game-camera game)))
         (incf (height (game-camera game)))))
      (:scancode-kp-9
       (when (game-editing game)
         (decf (width (game-camera game)))
         (decf (height (game-camera game)))))
      ((:scancode-w :scancode-up)
       (move-up))
      ((:scancode-s :scancode-down)
       (move-down))
      ((:scancode-a :scancode-left)
       (move-left))
      ((:scancode-d :scancode-right)
       (move-right))
      ((:scancode-e :scancode-space)
       (unless (game-animating game)
         (interact)))
      (:scancode-r
       (unless (game-animating game)
         (when (equal '(:main 4 16) (list *room* (x *hero*) (y *hero*)))
           (restart-game))))
      (:scancode-m
       (toggle-sfx)))))

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

(s:define-start-function (start) game (:resizable t :width 800 :height 1000)
  (:start (load-rooms)
          (music-init))
  (:quit (stop-melody)))
