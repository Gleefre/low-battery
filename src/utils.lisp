(in-package #:low-battery)

;; data-path to get resource's path
(defparameter *data-location* "res/")

(let ((data-folder nil))
  (defun data-path (relative-path)
    (setf data-folder
          (or data-folder
              (if (member :deploy *features*)
                  (let ((deploy:*data-location* *data-location*))
                    (deploy:data-directory))
                  (asdf:system-relative-pathname "low-battery" *data-location*))))
    (format nil "~a" (merge-pathnames relative-path data-folder))))

(defun pic (name)
  (uiop:native-namestring (merge-pathnames name (data-path "icons/"))))

(defmacro with-split ((width-var height-var &optional (orientation :horizontal))
                      &body size-body)
  (declare (type (member :horizontal :vertical) orientation))
  (let (($sizes (loop for clause in size-body collect (gensym "size")))
        ($sum-of-sizes (gensym "sum-of-sizes")))
    `(let (,@(loop for (size . body) in size-body
                   for $size in $sizes
                   collect `(,$size ,size)))
       (let ((,$sum-of-sizes (+ ,@$sizes)))
         (s:with-current-matrix
           ,@(loop for (size . body) in size-body
                   for $size in $sizes
                   collect `(let (,@(if (eq orientation :horizontal)
                                        `((,height-var ,height-var)
                                          (,width-var (* (/ ,$size ,$sum-of-sizes) ,width-var)))
                                        `((,width-var ,width-var)
                                          (,height-var (* (/ ,$size ,$sum-of-sizes) ,height-var)))))
                              ,@body)
                   collect (if (eq orientation :horizontal)
                               `(s:translate (* (/ ,$size ,$sum-of-sizes) ,width-var) 0)
                               `(s:translate 0 (* (/ ,$size ,$sum-of-sizes) ,height-var))))))
       nil)))
