(in-package #:low-battery)

(defclass room ()
  ((cells :initform (make-hash-table :test #'equal) :accessor cells)
   (name  :initform (gensym "UNNAMED-ROOM") :initarg :name :reader name)))

(defparameter *rooms-table* (make-hash-table))

(defmethod initialize-instance :after ((room room) &key &allow-other-keys)
  (setf (gethash (name room) *rooms-table*) room))

(defvar *room*)

(defun current-room ()
  (etypecase *room*
    (symbol (gethash *room* *rooms-table*))
    (room *room*)))

(defun list-all-rooms ()
  (loop for (key . value) in (alexandria:hash-table-alist *rooms-table*)
        when (keywordp key) collect value))

(defmacro with-room ((&optional (room '*room*)) &body body)
  `(let ((*room* ,room))
     ,@body))

(defun cell (x y)
  (gethash (cons x y) (cells (current-room))))

(defun (setf cell) (new-cell x y)
  (setf (gethash (cons x y) (cells (current-room))) new-cell))

(defun save-room (file &optional (*room* *room*))
  (alexandria:with-output-to-file (out file :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-readably* T))
        (print `(:room (:cells ,(alexandria:hash-table-alist (cells (current-room)))
                        :name ,(name (current-room))))
               out)))))

(defun load-room (file)
  (alexandria:with-input-from-file (in file)
    (with-standard-io-syntax
      (let ((*read-eval* NIL))
        (destructuring-bind (&key room) (read in)
          (when room
            (destructuring-bind (&key cells name) room
              (make-instance 'room :cells (alexandria:alist-hash-table cells)
                                   :name name))))))))
