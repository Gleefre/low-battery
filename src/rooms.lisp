(in-package #:low-battery)

(defclass room ()
  ((cells :initform (make-hash-table :test #'equal) :initarg :cells :accessor cells)
   (name  :initform (gensym "UNNAMED-ROOM") :initarg :name :reader name)))

(defvar *rooms-table* (make-hash-table))

(defvar *known-rooms* ())

(defmethod initialize-instance :after ((room room) &key &allow-other-keys)
  (when (keywordp (name room))
    (setf (gethash (name room) *rooms-table*) room)
    (pushnew (name room) *known-rooms*)))

(defvar *room*)

(defun current-room ()
  (etypecase *room*
    (symbol (gethash *room* *rooms-table*))
    (room *room*)))

(defun list-all-rooms ()
  (mapcar (lambda (key) (gethash key *rooms-table*))
          *known-rooms*))

(defun next-room ()
  (or (cadr (member *room* *known-rooms*))
      (car *known-rooms*)))

(defmacro with-room ((&optional (room '*room*)) &body body)
  `(let ((*room* ,room))
     ,@body))

(defun cell (x y)
  (gethash (cons x y) (cells (current-room))))

(defun (setf cell) (new-cell x y)
  (setf (gethash (cons x y) (cells (current-room))) new-cell))

(defun save-room (file &optional (*room* *room*))
  ;; FIXME: This is an ugly workaround
  (shift-room (current-room) 0 0)
  (alexandria:with-output-to-file (out file :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-readably* T))
        (format out "~S~%" `(:room (:cells ,(alexandria:hash-table-alist (cells (current-room)))
                                    :name ,(name (current-room)))))))))

(defun load-room (file)
  (alexandria:with-input-from-file (in file)
    (with-standard-io-syntax
      (let ((*read-eval* NIL))
        (destructuring-bind (&key room) (read in)
          (when room
            (destructuring-bind (&key cells name) room
              (make-instance 'room :cells (alexandria:alist-hash-table cells :test #'equal)
                                   :name name))))))))

(defun reset-room (room)
  (let ((*room* room))
    (maphash (lambda (key value)
               (declare (ignorable key))
               (alexandria:when-let ((battery (find :battery value :key #'car)))
                 (setf (cadr battery) (caddr battery))))
             (cells (current-room)))))

(defun room-filename (name)
  (data-path (format nil "rooms/~a.room" (string name))))

(defun load-rooms ()
  (loop for file in (uiop:directory-files (data-path "rooms/"))
        do (load-room file)))

(defun shift-room (room x->0 y->0)
  (let ((new-cells (make-hash-table :test #'equal)))
    (loop for (key . cell) in (alexandria:hash-table-alist (cells room))
          when cell
          do (destructuring-bind (x . y) key
               (if (and (numberp x) (numberp y))
                   (setf (gethash (cons (- x x->0) (- y y->0)) new-cells) cell)
                   (warn "Something went wrong, got malformed key:~%  C(~S) = ~S" key cell))))
    (setf (cells room) new-cells)))

(defun touch-room (name)
  (save-room (room-filename name) (make-instance 'room :name name)))
