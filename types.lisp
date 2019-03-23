;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)
(annot:enable-annot-syntax)

;; TODO: DRY

;; Floats ----------------------------

(defun floats-to-cv (floats)
  ;; TODO: use arrays
  (let ((cfloats (cv-call-out cv-floats-new :pointer)))
    (loop for f in floats
          do (cv-floats-add cfloats (as-single-float f)))
    cfloats))

;; Doubles ----------------------------

(defun doubles-from-cv (cdoubles)
  ;; TODO: use arrays
  (loop with count = (cv-call-out cv-doubles-count cdoubles :int)
        for i from 0 below count
        collect (cv-call-out cv-doubles-get cdoubles i :double)))

;; Ints --------------------------------

(defun ints-to-cv (values)
  (let ((cints (cv-call-out cv-ints-new :pointer)))
    (etypecase values
      (array (loop for v across values do (cv-call cv-ints-add cints v)))
      (list  (loop for v in     values do (cv-call cv-ints-add cints v))))
    cints))

(defun ints-from-cv (cints)
  (loop with count = (cv-call-out cv-ints-count cints :int)
        for i from 0 below count
        collect (cv-call-out cv-ints-get cints i :int)))

(defun int-array-from-cv (cints)
  (loop with count = (cv-call-out cv-ints-count cints :int)
        with array = (make-array count :element-type 'integer)
        for i from 0 below count
        do (setf (elt array i) (cv-call-out cv-ints-get cints i :int))
        finally (return array)))

;; Strings ---------------------------------

(defun strings-to-cv (strings)
  (let ((cstrings (cv-call-out cv-strings-new :pointer)))
    (loop for string in strings
          do (cv-call cv-strings-add cstrings string))
    cstrings))

(defun strings-from-cv (cstrings)
  (loop with count = (cv-call-out cv-strings-count cstrings :int)
        for i from 0 below count
        collect (cv-call-out cv-strings-get cstrings i :string)))

;; Scalar ---------------------------

@export
(defclass scalar ()
  ((v0 :initarg :v0)
   (v1 :initarg :v1)
   (v2 :initarg :v2)
   (v3 :initarg :v3)))

@export
(defun scalar (&optional v0 v1 v2 v3)
  (make-instance 'scalar
                 :v0 (as-double-float (or v0 0d0))
                 :v1 (as-double-float (or v1 0d0))
                 :v2 (as-double-float (or v2 0d0))
                 :v3 (as-double-float (or v3 0d0))))

(defmethod print-object ((s scalar) out)
  (print-unreadable-object (s out :type t)
    (with-slots (v0 v1 v2 v3) s
      (format out "(~s, ~s, ~s, ~s)" v0 v1 v2 v3))))

(defun scalar-to-cv (scalar)
  (with-slots (v0 v1 v2 v3) scalar
    (cv-call-out cv-scalar-new v0 v1 v2 v3 :pointer)))

(defun scalar-from-cv (cscalar)
  (let ((data (cv-call-out cv-scalar-data cscalar :pointer)))
    (scalar (cffi:mem-aref data :double 0)
            (cffi:mem-aref data :double 1)
            (cffi:mem-aref data :double 2)
            (cffi:mem-aref data :double 3))))

(defun as-scalar (x)
  (etypecase x
    (scalar x)
    (number (scalar x))
    (list (apply #'scalar x))))

;; Size ------------------------------

@export
(defclass size ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)))

(export 'width)
(export 'height)

@export
(defun size (&optional width height)
  (make-instance 'size
                 :width (as-single-float (or width 0s0))
                 :height (as-single-float (or height width 0s0))))

(defmethod print-object ((s size) out)
  (print-unreadable-object (s out :type t)
    (format out "(~s x ~s)" (width s) (height s))))

(defun size-to-cv (size)
  (with-slots (width height) size
    (cv-call-out cv-size-new width height :pointer)))

(defun size-from-cv (csize)
  (size (cv-call-out cv-size-width  csize :float)
        (cv-call-out cv-size-height csize :float)))

;; Point -----------------------------------

@export
(defclass point ()
  ((x :initarg :x :reader x)
   (y :initarg :y :reader y)))

(export 'x)
(export 'y)

@export
(defun point (&optional x y)
  (make-instance 'point
                 :x (as-single-float (or x 0s0))
                 :y (as-single-float (or y x 0s0))))

(defmethod print-object ((p point) out)
  (print-unreadable-object (p out :type t)
    (format out "(~s, ~s)" (x p) (y p))))

(defun point-to-cv (point)
  (with-slots (x y) point
    (cv-call-out cv-point-new x y :pointer)))

(defun point-from-cv (cpoint)
  (point (cv-call-out cv-point-x cpoint :float)
         (cv-call-out cv-point-y cpoint :float)))

;; Points (vector) -----------------------

(defun points-from-cv (cpoints)
  (loop with count = (cv-call-out cv-points-count cpoints :int)
        for i from 0 below count
        collect (point-from-cv (cv-call-out cv-points-get cpoints i :pointer))))

;; Rect -----------------------------------

@export
(defclass rect ()
  ((x :initarg :x :reader x)
   (y :initarg :y :reader y)
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)))

@export
(defun rect (&optional x y w h)
  (make-instance 'rect
                 :x (as-single-float (or x 0s0))
                 :y (as-single-float (or y x 0s0))
                 :width (as-single-float (or w 0s0))
                 :height (as-single-float (or h w 0s0))
                 ))

(defmethod print-object ((r rect) out)
  (print-unreadable-object (r out :type t)
    (with-slots (x y width height) r
      (format out "((~s, ~s) (~s x ~s))" x y width height))))

(defun rect-to-cv (rect)
  (with-slots (x y width height) rect
    (cv-call-out cv-rect-new x y width height :pointer)))

(defun rect-from-cv (crect)
  (rect (cv-call-out cv-rect-x crect :float)
        (cv-call-out cv-rect-y crect :float)
        (cv-call-out cv-rect-width crect :float)
        (cv-call-out cv-rect-height crect :float)))

;; RotatedRect -------------------------

@export
(defclass rotated-rect ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (angle :initarg :angle :accessor angle)))

@export
(defun rotated-rect (x y w h angle)
  (make-instance 'rotated-rect
                 :x x
                 :y y
                 :width w
                 :height h
                 :angle angle))

(defun rotated-rect-to-cv (rr)
  (with-slots (x y width height angle) rr
    (cv-call-out cv-rotated-rect-new
                 (as-single-float x)
                 (as-single-float y)
                 (as-single-float width)
                 (as-single-float height)
                 (as-single-float angle)
                 :pointer)))

(defmethod print-object ((r rotated-rect) out)
  (print-unreadable-object (r out :type t)
    (with-slots (x y width height angle) r
      (format out "((~s, ~s) (~s x ~s) @~s)" x y width height angle))))

@export
(defun rotated-rect-points (rect)
  (with-foreign-resource (cr (rotated-rect-to-cv rect)
                          :free cv-rotated-rect-free)
    (with-foreign-resource (cpoints (cv-call-out cv-rotated-rect-points cr :pointer)
                            :free cv-points-free)
      (points-from-cv cpoints))))

;; RotatedRects (vector) -------------------------------------

(defun rotated-rects-to-cv (rects)
  (let ((crects (cv-call-out cv-rotated-rects-new :pointer)))
    (loop for rect in rects
          do (with-foreign-resource (crect (rotated-rect-to-cv rect) :free cv-rotated-rect-free)
               (cv-call cv-rotated-rects-add crects crect)))
    crects))
