(in-package #:see)
(annot:enable-annot-syntax)

@export
(defclass window ()
  ((name :initarg :name :reader window-name)))

@export
(defun make-window (name)
  (let ((window (make-instance 'window :name name)))
    (cv-named-window name 0)
    window))

(defmethod release ((window window))
  (cv-destroy-window (window-name window)))

@export
(defun destroy-all-windows ()
  (cv-destroy-all-windows))

@export
(defun show-image (window mat)
  (cv-imshow (window-name window)
             (peer mat)))

@export
(defun wait-key (&key (delay 0))
  (handler-case
      (progn
        #+:sbcl (sb-int:set-floating-point-modes :traps '(:overflow :invalid))
        (cv-wait-key delay))
    ;; Happens inside cv::waitKey, no matter what.
    (division-by-zero ()
      (format t "division by zero caught!~%")
      -1)))

@export
(defun open-window (on-update &key (delay 50) (name "") (blocking t))
  (trivial-main-thread:with-body-in-main-thread (:blocking blocking)
    (handler-case
        (let ((win (make-window name)))
          (unwind-protect
               (loop for key = -1 then (wait-key :delay delay)
                     while (funcall on-update win key))
            (release win)))
      (error (e)
        (progn
          (format t "ERROR: ~s~%" e)
          (trivial-backtrace:print-backtrace e))))))

@export
(defmacro with-open-window (((handle key) &key (name "") (delay 50) (blocking t))
                            &body forms)
  `(open-window (lambda (,handle ,key) ,@forms)
                :name ,name
                :delay ,delay
                :blocking ,blocking))

@export
(defun view-image (img)
  (with-open-window ((win key) :name "Image")
    (show-image win img)
    (< key 0)))
