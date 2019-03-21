;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)
(annot:enable-annot-syntax)

;; TODO: shouldn't it be a weak hash?
(defvar *windows* (make-hash-table :test 'equal))

@export
(defun set-mouse-callback (window fn)
  (with-slots (on-mouse) window
    (setf on-mouse fn)))

@export
(defclass window ()
  ((name :initarg :name :reader window-name)
   (id :initarg :id)
   (on-mouse :initarg :on-mouse :initform nil)))

(cffi:defcallback %on-mouse :bool ((event :int) (x :int) (y :int)
                                   (flags :int) (userdata :pointer))
  (let* ((name   (cffi:foreign-string-to-lisp userdata))
         (window (gethash name *windows*)))
    (when window
      (when-let ((on-mouse (slot-value window 'on-mouse)))
        (let ((event-kw (cffi:foreign-enum-keyword 'cv-mouse-event-types event)))
          (funcall on-mouse window event-kw x y flags))))))

@export
(defun make-window (name &key on-mouse)
  (assert (null (gethash name *windows*)))
  (let* ((id (cffi:foreign-string-alloc name))
         (window (make-instance 'window :name name :id id)))
    (cv-call cv-named-window name 0)
    (cv-call cv-set-mouse-callback name (cffi:callback %on-mouse) id)
    (set-mouse-callback window on-mouse)
    (setf (gethash name *windows*) window)
    window))

(defmethod release ((window window))
  (with-slots (id name) window
    (when id
      (cffi:foreign-string-free id)
      (setf id nil)
      (set-mouse-callback window nil)
      (cv-call cv-destroy-window name))
    (remhash name *windows*)))

@export
(defun destroy-all-windows ()
  (loop for w in (hash-table-values *windows*)
        do (release w))
  (clrhash *windows*)
  (cv-call cv-destroy-all-windows))

@export
(defun show-image (window image)
  (cv-call cv-imshow
           (window-name window)
           (peer image)))

@export
(defun wait-key (&key (delay 0))
  (handler-case
      (progn
        #+:sbcl (sb-int:set-floating-point-modes :traps '(:overflow :invalid))
        (cffi:with-foreign-object (ckey :int)
          (cv-call cv-wait-key delay ckey)
          (cffi:mem-ref ckey :int)))
    ;; Happens inside cv::waitKey, no matter what.
    (division-by-zero ()
      (format t "division by zero caught!~%")
      -1)))

@export
(defun open-window (on-update &key (delay 50) (name "") (blocking t) on-mouse)
  (trivial-main-thread:with-body-in-main-thread (:blocking blocking)
    (handler-case
        (let ((win (make-window name :on-mouse on-mouse)))
          (unwind-protect
               (loop for key = -1 then (wait-key :delay delay)
                     while (funcall on-update win key))
            (release win)))
      (error (e)
        (progn
          (format t "ERROR: ~s~%" e)
          (trivial-backtrace:print-backtrace e))))))

@export
(defmacro with-open-window (((handle key) &key (name "") (delay 50) (blocking t) on-mouse)
                            &body forms)
  `(open-window (lambda (,handle ,key) ,@forms)
                :name ,name
                :delay ,delay
                :blocking ,blocking
                :on-mouse ,on-mouse))

@export
(defun view-image (img)
  (with-open-window ((win key) :name "Image")
    (show-image win img)
    (< key 0)))
