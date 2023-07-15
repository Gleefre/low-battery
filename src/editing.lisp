(in-package #:low-battery)

(defparameter *editing* nil)

(defparameter *cell* nil)

(defclass editing ()
  ((modeline :initform (list (cons :platform nil)
                             (cons :ice nil)
                             (cons :battery 1)
                             (cons :portal nil)
                             (cons :update 1)
                             (cons :home nil))
             :accessor modeline)
   (mode :initform :select :accessor mode)
   (cell :initform nil :accessor editing-cell)
   (ref :initform nil :accessor ref)))

(defun ecell ()
  (with-room ((car *cell*))
    (apply #'cell (cdr *cell*))))

(defun (setf ecell) (new-cell)
  (with-room ((car *cell*))
    (apply #'(setf cell) new-cell (cdr *cell*))))

(defun edit-cell (x y button)
  (declare (ignorable button))
  (when *editing*
    (if (eq (mode *editing*) :select)
        (setf (editing-cell *editing*) (list x y)
              *cell* (list *room* x y))
        (let ((mode (nth (mode *editing*) (modeline *editing*))))
          (case (car mode)
            ((:platform :ice :portal :home)
             (if (member (car mode) (cell x y) :key #'car)
                 (alexandria:deletef (cell x y) (car mode) :key #'car)
                 (push (cons (car mode) nil) (cell x y))))
            (:update
             (let ((cell (find (car mode) (cell x y) :key #'car)))
               (if cell
                   (if (= 9 (cdr cell))
                       (alexandria:deletef (cell x y) (car mode) :key #'car)
                       (incf (cdr cell)))
                   (push (cons (car mode) 1) (cell x y)))))
            (:battery
             (let ((cell (find (car mode) (cell x y) :key #'car)))
               (if cell
                   (if (= 3 (cdr cell))
                       (alexandria:deletef (cell x y) (car mode) :key #'car)
                       (incf (cdr cell)))
                   (push (cons (car mode) 1) (cell x y))))))))))

(defun update-editing-mode (n button)
  (declare (ignorable button))
  (when *editing*
    (if (eql n (mode *editing*))
        (setf (mode *editing*) :select)
        (setf (mode *editing*) n))))
