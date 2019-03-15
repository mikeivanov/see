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
                 :release #'cv-mat-free))

(defun make-type (depth channels)
  (let ((cdepth (cffi:foreign-enum-value 'cv-depths depth)))
    (cv-make-type cdepth channels)))

(defun mat-new-empty ()
  (mat-from-cv (cv-mat-new)))

(defun mat-new-with-scalar (shape scalar depth channels)
  (with-foreign-resource (cshape (ints-to-cv shape))
    (with-foreign-resource (cscalar (scalar-to-cv scalar))
      (let ((ctype (make-type depth channels)))
        (mat-from-cv (cv-mat-new-with-scalar cshape ctype cscalar))))))

(defun mat-new-with-ptr (ptr shape depth channels copy-data-p)
  (let ((ctype (make-type depth channels)))
    (with-foreign-resource (cshape (ints-to-cv shape)
                            :free cv-ints-free)
      (if copy-data-p
          (with-foreign-resource (cmat (cv-mat-new-with-data cshape ctype ptr)
                                  :free cv-mat-free)
            (let ((result (mat)))
              (check-cv-error #'null (cv-mat-copy-to cmat (peer result)))
              result))
          (mat-from-cv (cv-mat-new-with-data cshape ctype ptr))))))

(defun mat-new-with-array (array depth channels)
  (let* ((shape (array-shape array))
         (ctype (depth-cffi-type depth)))
    (with-foreign-array (cdata ctype
                         :initial-contents (flatten-array array))
      (mat-new-with-ptr cdata shape depth channels t))))

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

(defmethod empty-p ((mat mat))
  (cv-mat-empty (peer mat)))

@export
(defun dims (mat)
  (cv-mat-dims (peer mat)))

@export
(defun rows (mat)
  (cv-mat-rows (peer mat)))

@export
(defun cols (mat)
  (cv-mat-cols (peer mat)))

@export
(defun shape (mat)
  (with-foreign-resource (shape (cv-mat-size (peer mat)))
    (check-cv-error #'cffi:null-pointer-p shape)
    (ints-from-cv shape)))

@export
(defun depth (mat)
  (cffi:foreign-enum-keyword 'cv-depths (cv-mat-depth (peer mat))))

@export
(defun channels (mat)
  (cv-mat-channels (peer mat)))

@export
(defun convert-to (mat depth &key (alpha 1d0) (beta 0d0))
  (let ((dst   (mat))
        (ctype (make-type depth (channels mat))))
    (cv-mat-convert-to (peer mat)
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
    (cv-mat-get-ptr (peer mat) idxarr)))

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
    (loop for i from 0 below (channels mat)
          collect (cffi:mem-aref ptr type i))))

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
  (let ((cm (cv-mats-new)))
    (loop for m in mats
          do (cv-mats-add cm (peer m)))
    cm))

(defun mats-from-cv (cmats)
  (loop for i below (cv-mats-count cmats)
     collect (mat-from-cv (cv-mat-copy (cv-mats-get cmats i)))))

;; Ticks ---------------------------------------

@export
(defun tick-count ()
  (cv-get-tick-count))

@export
(defun tick-frequency ()
  (cv-get-tick-frequency))

;; Arrays ----------------------------------

@export
(defun extract-channel (mat channel &key target)
  (let ((dst (or target (mat))))
    (check-cv-error #'null
                    (cv-extract-channel (peer mat)
                                        (peer dst)
                                        channel))
    dst))

@export
(defun slice (mat idx &key (copy-data-p t))
  (let* ((shape (shape mat))
         (n (length shape))
         (m (length idx)))
    (assert (< m n))
    (let* ((index (append idx (make-list (- n m) :initial-element 0)))
           (ptr   (mat-ptr mat index))
           (size  (subseq shape m n)))
      (mat-new-with-ptr ptr size (depth mat) (channels mat) copy-data-p))))

@export
(defun merge-channels (&rest mats)
  (let ((n (length mats)))
    (assert (< 1 n))
    (with-foreign-resource (cmats (mats-to-cv mats))
      (mat-from-cv (check-cv-error #'cffi:null-pointer-p
                                   (cv-merge cmats))))))

;; Math ops ----------------------------------

@export
(defgeneric add (object addendum))

@export
(defmethod add ((mat mat) (addendum scalar))
  (with-foreign-resource (cs (scalar-to-cv addendum)
                          :free cv-scalar-free)
    (check-cv-error #'null
                    (cv-mat-add-scalar (peer mat) cs)))
  mat)

@export
(defmethod add ((mat mat) (addendum number))
  (add mat (scalar addendum)))

@export
(defmethod mul ((mat mat) (multiplier number))
  (check-cv-error #'null
                  (cv-mat-mul-const (peer mat) multiplier))
  mat)
