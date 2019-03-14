(in-package #:see)
(annot:enable-annot-syntax)

;; Floats ----------------------------

(defun floats-to-cv (floats)
  (let ((cf (cv-floats-new)))
    (loop for f in floats
          do (cv-floats-add cf (as-single-float f)))
    cf))

;; Doubles ----------------------------

(defun doubles-from-cv (cdoubles)
  (loop for i from 0 below (cv-doubles-count cdoubles)
        collect (cv-doubles-get cdoubles i)))

;; Ints --------------------------------

(defun ints-to-cv (values)
  (let ((cv (cv-ints-new)))
    (etypecase values
      (array (loop for v across values do (cv-ints-add cv v)))
      (list  (loop for v in     values do (cv-ints-add cv v))))
    cv))

(defun ints-from-cv (cints)
  (loop for i from 0 below (cv-ints-count cints)
        collect (cv-ints-get cints i)))

(defun int-array-from-cv (cints)
  (let* ((count (cv-ints-count cints))
         (arr (make-array count :element-type 'integer)))
    (dotimes (i count)
      (setf (elt arr i) (cv-ints-get cints i)))
    arr))

(defun int-array-to-cv (values)
  (let ((cv (cv-ints-new)))
    (loop for v across values
          do (cv-ints-add cv v))
    cv))

;; Strings ---------------------------------

(defun strings-to-cv (strings)
  (let ((cs (cv-strings-new)))
    (loop for string in strings
          do (cv-strings-add cs string))
    cs))

(defun strings-from-cv (cstrings)
  (loop for i from 0 below (cv-strings-count cstrings)
        collect (cv-strings-get cstrings i)))

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
    (cv-scalar-new v0 v1 v2 v3)))

(defun scalar-from-cv (cscalar)
  (let ((ptr (cv-scalar-values cscalar)))
    (scalar (cffi:mem-aref ptr :double 0)
            (cffi:mem-aref ptr :double 1)
            (cffi:mem-aref ptr :double 2)
            (cffi:mem-aref ptr :double 3))))

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
    (cv-size-new width height)))

(defun size-from-cv (csize)
  (size (cv-size-width csize)
        (cv-size-height csize)))

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
    (cv-point-new x y)))

(defun point-from-cv (cpoint)
  (point (cv-point-x cpoint)
         (cv-point-y cpoint)))

;; Points (vector) -----------------------

(defun points-from-cv (cpoints)
  (loop for i from 0 below (cv-points-count cpoints)
        collect (point-from-cv (cv-points-get cpoints i))))

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
    (cv-rotated-rect-new (as-single-float x)
                         (as-single-float y)
                         (as-single-float width)
                         (as-single-float height)
                         (as-single-float angle))))

(defmethod print-object ((r rotated-rect) out)
  (print-unreadable-object (r out :type t)
    (with-slots (x y width height angle) r
      (format out "((~s, ~s) (~s x ~s) @~s)" x y width height angle))))

@export
(defun rotated-rect-points (rect)
  (with-foreign-resource (cr (rotated-rect-to-cv rect)
                          :free cv-rotated-rect-free)
    (with-foreign-resource (cpoints (cv-rotated-rect-points cr)
                            :free cv-points-free)
      (check-cv-error #'cffi:null-pointer-p cpoints)
      (points-from-cv cpoints))))


;; RotatedRects (vector) -------------------------------------

(defun rotated-rects-to-cv (rects)
  (let ((cr (cv-rotated-rects-new)))
    (loop for r in rects
          do (cv-rotated-rects-add cr (rotated-rect-to-cv r)))
    cr))
