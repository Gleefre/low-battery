(in-package #:low-battery)

(defun note-file (x)
  (probe-file
   (data-path (format nil "notes/note-~a.ogg" x))))

(defparameter *notes* (make-hash-table))
(defparameter *soundtrack* NIL)

(defun note (x)
  (gethash x *notes*))

(defun (setf note) (note x)
  (setf (gethash x *notes*) note))

(defun create-note (x)
  (setf (note x) (h:create (note-file x) :mixer :effect :volume 0.2)))

(defun create-notes (&aux (min -21) (max 27))
  (loop for x from min to max
        do (create-note x)))

(defparameter *sfx-mute* T)

(defun sfx (&rest notes)
  (unless *sfx-mute*
    (dolist (x notes)
      (h:play (note x) :reset T))))

(defparameter *melody* nil)

(defun pattern-at (p)
  (case p
    (:zero '())
    (:one '(0))
    (:one++ '(18))
    (:two '(0 12))
    (:two- '(0 6))
    (:two+ '(0 18))
    (:three '(0 8 16))
    (:three-seq '(0 6 18))
    (:four '(0 6 12 18))))

(defun play-one (seq base pattern dt)
  (let ((at (make-array 24 :initial-element nil)))
    (dolist (time (pattern-at pattern))
      (dolist (note base)
        (push note (aref at time))))
    (loop for note in seq
          for time from 0 to 23 by 6
          do (push note (aref at time)))
    (loop for i below 24
          for notes across at
          do (mapc #'sfx notes)
             (sleep (/ dt 6)))))

(defun melody ()
  (setf *sfx-mute* nil)
  (unless *melody*
    (setf *melody*
          (bt:make-thread
           (lambda ()
             (loop while *melody*
                   for z in '#1=(7 11 19 19 7 11 . #1#)
                   for y = 1 then (mod (1+ (expt y 3)) z)
                   for (p1 p2) in '#2=((:one :one)
                                       (:one :one)
                                       (:two :two)
                                       (:two+ :two-)
                                       (:four :one)
                                       (:four :one)
                                       (:one :zero)
                                       (:zero :zero)
                                       (:zero :one++)
                                       . #2#)
                   for seq = (loop for x from y to (+ y 12) by 4 collect x)
                   do (play-one seq (list (- y 5) (- y 9)) p1 .4)
                      (play-one seq (list (- y 5) (- y 9)) p2 .4)))
           :name "MELODY"))))

(defun stop-melody ()
  (when *melody*
    (bt:destroy-thread *melody*)
    (setf *melody* nil
          *sfx-mute* t)))

(defun toggle-sfx ()
  (if *sfx-mute*
      (progn (melody))
      (progn (stop-melody))))

(defun music-init ()
  (unless h:*server*
    (h:maybe-start-simple-server :mixers '((:effect m:basic-mixer))
                                 :name "Low Battery")
    (create-notes)))
