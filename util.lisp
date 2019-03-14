(in-package #:see)
(annot:enable-annot-syntax)

@export
(defgeneric release (x))

@export
(defgeneric opened-p (x))

@export
(defgeneric empty-p (x))

(defclass ref ()
  ((resource :initarg :resource)
   (release :initarg :release)))

(defun ref (resource release)
  (make-instance 'ref :resource resource :release release))

(defun deref (ref)
  (slot-value ref 'resource))

(defmethod release ((ref ref))
  (with-slots (resource release) ref
    (when resource
      (funcall release resource)
      (setf resource nil))))

(defclass proxy ()
  ((ref :reader peer-ref)))

(defun peer (proxy)
  (deref (peer-ref proxy)))

(defmethod release ((proxy proxy))
  (release (peer-ref proxy)))

(defmethod initialize-instance :after ((proxy proxy) &key peer release)
  (let ((ref (ref peer release)))
    (setf (slot-value proxy 'ref) ref)
    (trivial-garbage:finalize proxy
                              (lambda () (release ref))))
  proxy)

@export
(defun xor (a b)
  (and (or a b)
       (not (and a b))))

(defun free-foreign-array-contents (arr count)
  (dotimes (i count)
    (let ((val (cffi:mem-aref arr :pointer i)))
      (when (not (cffi:null-pointer-p val))
        (cffi:foreign-free val)))))

(defmacro with-foreign-array ((sym type
                               &key
                                 count
                                 count-var
                                 (initial-contents nil initial-contents-p)
                                 (initial-element nil initial-element-p)
                                 null-terminated-p
                                 free-contents-p)
                              &body
                                body)
  (let* ((%count-var (or count-var (gensym)))
         (%contents  (gensym)))
    `(let* ((,%contents ,initial-contents)
            (,%count-var (or ,count
                             (length ,%contents)))
            (,sym (cffi:foreign-alloc ,type
                                      :count ,%count-var
                                      ,@(concatenate 'list
                                                     (when initial-contents-p
                                                       (list :initial-contents %contents))
                                                     (when initial-element-p
                                                       (list :initial-element initial-element)))
                                      :null-terminated-p ,null-terminated-p)))
       (unwind-protect
            (progn ,@body)
         (progn
           ,(when free-contents-p
              `(see::free-foreign-array-contents ,sym ,%count-var))
           (cffi:foreign-free ,sym))))))

@export
(define-condition cv-error ()
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream) (format stream (message condition)))))

(defun check-cv-error (predicate value)
  (if (funcall predicate value)
      (let ((error (cv-get-error)))
        (cv-clear-error)
        (error 'cv-error :message error))
      value))

(defmacro with-foreign-resource ((name init &key (free 'cffi:foreign-free)) &body body)
  `(let ((,name ,init))
     (unwind-protect
          (progn ,@body)
       (funcall (symbol-function (quote ,free)) ,name))))

@export
(defmacro with-resource ((var init) &body body)
  `(let ((,var ,init))
     (unwind-protect
          (progn ,@body)
       (release ,var))))

@export
(defun as-single-float (x)
  (coerce x 'single-float))

@export
(defun as-double-float (x)
  (coerce x 'double-float))

@export
(defun array-shape (array)
  (loop for d from 0 below (array-rank array)
        collect (array-dimension array d)))

@export
(defun flatten-array (array)
  (make-array (list (array-total-size array))
              :displaced-to array
              :displaced-index-offset 0
              :element-type (array-element-type array)))

@export
(defun array-transpose (array)
  (let* ((m (array-dimension array 0))
         (n (array-dimension array 1))
         (result (make-array `(,n ,m)
                             :initial-element (coerce 0 (array-element-type array))
                             :element-type (array-element-type array))))
    (loop for i from 0 below m do
      (loop for j from 0 below n do
        (setf (aref result j i)
              (aref array i j))))
    result))

@export
(defun array-reshape (array shape)
  (make-array shape
              :displaced-to array
              :element-type (array-element-type array)))
