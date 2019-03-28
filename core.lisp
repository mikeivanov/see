;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)
(annot:enable-annot-syntax)

@export
(defgeneric at (container idx &key &allow-other-keys))

;; Mat ------------------------------------------

@export
(defclass mat (proxy) ())

(defun mat-from-cv (peer)
  (make-instance 'mat
                 :peer peer
                 :release (lambda (self)
                            (cv-call cv-mat-free self))))

(defun make-type (depth channels)
  (let ((cdepth (cffi:foreign-enum-value 'cv-depths depth)))
    (cv-call-out cv-make-type cdepth channels :int)))

(defun mat-new-empty ()
  (mat-from-cv (cv-call-out cv-mat-new :pointer)))

(defun mat-new-copy (mat)
  (mat-from-cv (cv-call-out cv-mat-new-copy (peer mat) :pointer)))

(defun mat-new-with-scalar (shape scalar depth channels)
  (with-foreign-resource (cshape (ints-to-cv shape) :free cv-ints-free)
    (with-foreign-resource (cscalar (scalar-to-cv scalar) :free cv-scalar-free)
      (let ((ctype (make-type depth channels)))
        (mat-from-cv (cv-call-out cv-mat-new-with-scalar cshape ctype cscalar :pointer))))))

(defun mat-new-with-ptr (ptr shape depth channels)
  (let ((ctype (make-type depth channels)))
    (with-foreign-resource (cshape (ints-to-cv shape) :free cv-ints-free)
      (mat-from-cv (cv-call-out cv-mat-new-with-data cshape ctype ptr :pointer)))))

(defun mat-new-with-array (array depth channels)
  (let* ((shape (array-shape array))
         (ctype (depth-cffi-type depth)))
    (with-foreign-array (cdata ctype
                         :initial-contents (flatten-array array))
      (with-resource (mat (mat-new-with-ptr cdata shape depth channels))
        ;; Need a deep clone b/c the `cdata` array scope
        (mat-clone mat)))))

@export
(defun mat (&key
              shape initial-element initial-contents
              (depth :depth-8-u) (channels 1))
  (cond ((and (not shape) (not initial-element) (not initial-contents))
         (mat-new-empty))
        ((and shape (not initial-contents))
         (mat-new-with-scalar shape
                              (as-scalar (or initial-element (scalar)))
                              depth channels))
        ((and (not shape) initial-contents (typep initial-contents 'array))
         (mat-new-with-array initial-contents depth channels))
        (t (error 'cv-error :message "Invalid combination of parameters"))))

@export
(defun mat-clone (mat)
  (let ((clone (mat)))
    (cv-call cv-mat-copy-to (peer mat) (peer clone))
    clone))

(defmethod empty-p ((mat mat))
  (cv-call-out cv-mat-empty (peer mat) :bool))

@export
(defun dims (mat)
  (cv-call-out cv-mat-dims (peer mat) :int))

@export
(defun rows (mat)
  (cv-call-out cv-mat-rows (peer mat) :int))

@export
(defun cols (mat)
  (cv-call-out cv-mat-cols (peer mat) :int))

@export
(defun shape (mat)
  (with-foreign-resource (cshape (ints-to-cv nil) :free cv-ints-free)
    (cv-call cv-mat-size (peer mat) cshape)
    (ints-from-cv cshape)))

@export
(defun depth (mat)
  (let ((val (cv-call-out cv-mat-depth (peer mat) :int)))
    (cffi:foreign-enum-keyword 'cv-depths val)))

@export
(defun channels (mat)
  (cv-call-out cv-mat-channels (peer mat) :int))

@export
(defun element-count (mat)
  (cv-call-out cv-mat-total (peer mat) :int))

@export
(defun convert-to (mat depth &key (alpha 1d0) (beta 0d0))
  (let ((dst   (mat))
        (ctype (make-type depth (channels mat))))
    (cv-call cv-mat-convert-to
             (peer mat)
             (peer dst)
             ctype
             (as-double-float alpha)
             (as-double-float beta))
    dst))

(defun mat-ptr (mat index)
  (with-foreign-array (idxarr :int
                       :count (dims mat)
                       :initial-element 0)
    (loop for i from 0
          for x in index
          do (setf (cffi:mem-aref idxarr :int i) x))
    (cv-call-out cv-mat-get-ptr (peer mat) idxarr :pointer)))

(defun depth-cffi-type (depth)
  (ecase depth
    (:depth-8-u  :uint8)
    (:depth-8-s  :int8)
    (:depth-16-u :uint16)
    (:depth-16-s :int16)
    (:depth-32-s :int32)
    (:depth-32-f :float)
    (:depth-64-f :double)))

(defun depth-lisp-type (depth)
  (ecase depth
    (:depth-8-u  '(unsigned-byte 8))
    (:depth-8-s  '(signed-byte 8))
    (:depth-16-u '(unsigned-byte 16))
    (:depth-16-s '(signed-byte 16))
    (:depth-32-s '(signed-byte 32))
    (:depth-32-f 'single-float)
    (:depth-64-f 'double-float)))

(defun mat-get-value (mat index channel)
  (let ((ptr  (mat-ptr mat index))
        (type (depth-cffi-type (depth mat))))
    (cffi:mem-aref ptr type channel)))

(defun mat-get-scalar (mat index)
  (let ((ptr  (mat-ptr mat index))
        (type (depth-cffi-type (depth mat))))
    (apply #'scalar
           (loop for i from 0 below (channels mat)
                 collect (cffi:mem-aref ptr type i)))))

(defmethod at ((mat mat) (index number) &key channel)
  (at mat (list index) :channel channel))

(defmethod at ((mat mat) (index list) &key channel)
  (assert (<= (length index) (dims mat)))
  (loop for i in index
        for n in (shape mat)
        do (assert (< i n)))
  (if (null channel)
      (mat-get-scalar mat index)
      (progn
        (assert (< channel (channels mat)))
        (mat-get-value mat index channel))))

@export
(defun mat-to-array (mat)
  (let* ((ptr   (mat-ptr mat nil))
         (shape (shape mat))
         (depth (depth mat))
         (chans (channels mat))
         (ctype (depth-cffi-type depth))
         (ltype (depth-lisp-type depth))
         (size  (apply #'* (cons chans shape)))
         (array (make-array (list size) :element-type ltype)))
    (dotimes (i size)
      (setf (aref array i) (cffi:mem-aref ptr ctype i)))
    (make-array shape :element-type ltype :displaced-to array)))

;; Mats (vector) ------------------------------

(defun mats-to-cv (mats)
  (let ((cmats (cv-call-out cv-mats-new :pointer)))
    (loop for m in mats
          do (cv-call cv-mats-add cmats (peer m)))
    cmats))

(defun mats-from-cv (cmats)
  (let ((count (cv-call-out cv-mats-count cmats :int)))
    (loop for i below count
          collect (let ((cref (cv-call-out cv-mats-get cmats i :pointer)))
                    ;; mats.get returns a ref, not a new instance
                    ;; therefore a c-level copy has to be created
                    (mat-from-cv (cv-call-out cv-mat-new-copy cref :pointer))))))

;; Ticks ---------------------------------------

@export
(defun tick-count ()
  (cv-call-out cv-get-tick-count :long))

@export
(defun tick-frequency ()
  (cv-call-out cv-get-tick-frequency :double))

;; Arrays ----------------------------------

@export
(defun extract-channel (mat channel &key target)
  (let ((dst (or target (mat))))
    (cv-call cv-extract-channel
             (peer mat)
             (peer dst)
             channel)
    dst))

@export
(defun slice (mat idx &key (copy-data-p t)) ;; TODO: does it have to be `t`?
  (let* ((shape (shape mat))
         (n (length shape))
         (m (length idx)))
    (assert (< m n))
    (let* ((index (append idx (make-list (- n m) :initial-element 0)))
           (ptr   (mat-ptr mat index))
           (size  (subseq shape m n))
           (mat   (mat-new-with-ptr ptr size (depth mat) (channels mat))))
      (if copy-data-p
          (mat-clone mat)
          mat))))

@export
(defun merge-channels (mats &key destination)
  (assert (< 1 (length mats)))
  (let ((mat (or destination (mat))))
    (with-foreign-resource (cmats (mats-to-cv mats) :free cv-mats-free)
      (cv-call cv-merge cmats (peer mat))
      mat)))

;; Math ops ----------------------------------

@export
(defgeneric add (object addendum)) ;; TODO: add (:in-place-p t)?

@export
(defmethod add ((mat mat) (addendum mat))
  (cv-call cv-mat-add-mat (peer mat) (peer addendum))
  mat)

@export
(defmethod add ((mat mat) (addendum scalar))
  (with-foreign-resource (cs (scalar-to-cv addendum) :free cv-scalar-free)
    (cv-call cv-mat-add-scalar (peer mat) cs)
    mat))

@export
(defmethod add ((mat mat) (addendum number))
  (add mat (scalar addendum)))

@export
(defmethod mul ((mat mat) (multiplier number))
  (cv-call cv-mat-mul-const (peer mat) multiplier)
  mat)

@export
(defun dot-product (mat1 mat2)
  (cv-call-out cv-mat-dot (peer mat1) (peer mat2) :double))

;; Images ------------------------------

@export
(defun image-size (img)
  (assert (= 2 (dims img)))
  (size (cols img) (rows img)))

@export
(defun image-roi (img roi)
  (assert (= 2 (dims img)))
  (with-foreign-resource (croi (rect-to-cv roi) :free cv-rect-free)
    (mat-from-cv (cv-call-out cv-mat-new-with-roi (peer img) croi :pointer))))
