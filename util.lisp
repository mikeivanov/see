;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

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
  (let ((resource (deref (peer-ref proxy))))
    (assert (not (null resource)))
    resource))

(defmethod release ((proxy proxy))
  (release (peer-ref proxy)))

(defmethod initialize-instance :after ((proxy proxy) &key peer release)
  (let ((ref (ref peer release)))
    (setf (slot-value proxy 'ref) ref)
    (trivial-garbage:finalize proxy (lambda () (release ref))))
  proxy)

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
  ((code :initarg :code :reader error-code)
   (message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
             (format stream "ERROR(~a): ~a"
                     (error-code condition)
                     (error-message condition)
                     ))))

(defun check-cv-error (predicate value)
  (if (funcall predicate value)
      (let ((error (cv-get-error-message))
            (code (cv-get-error-code)))
        (cv-clear-error)
        (error 'cv-error :code code :message error))
      value))

(defmacro cv-call (fn &rest args)
  `(check-cv-error (lambda (x) (not (= 0 x)))
                   (funcall (symbol-function (quote ,fn)) ,@args)))

(defmacro cv-call-out (fn &rest args)
  (let* ((type (car (last args)))
         (args (butlast args)))
    (check-type type keyword)
    (let ((out (gensym)))
      `(cffi:with-foreign-object (,out ,type)
         (cv-call ,fn ,@args ,out)
         (cffi:mem-ref ,out ,type)))))

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

@export
(defun string-strip (string)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab
                 #\Linefeed #\Page #\Return #\Rubout)
               string))

@export
(defun fit-image (img box &key target)
  (let* ((iw (cols img))
         (ih (rows img))
         (k  (max 1e0
                  (/ iw (width box))
                  (/ ih (height box))))
         (nw (/ iw k))
         (nh (/ ih k)))
    (resize-image img (size nw nh) :target target)))

@export
(defun parse-boolean (string)
  (some (curry #'string-equal string)
        '("yes" "y" "true" "t" "1")))
