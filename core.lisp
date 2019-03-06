(in-package #:see)
(annot:enable-annot-syntax)

@export
(defgeneric at (container idx))

;; Mat ------------------------------------------

@export
(defclass mat (proxy) ())

@export
(defun mat (&key peer)
  (make-instance 'mat
                 :peer (or peer (cv-mat-new))
                 :release #'cv-mat-free))

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
    (int-array-from-cv shape)))

@export
(defun depth (mat)
  (cv-mat-depth (peer mat)))

@export
(defun channels (mat)
  (cv-mat-channels (peer mat)))

@export
(defun convert-to-type (mat type &key (alpha 1d0) (beta 0d0))
  (let ((dst (mat)))
    (cv-mat-convert-to (peer mat) (peer dst) type alpha beta)
    dst))

(defmethod at ((mat mat) index)
  ;; TODO: it's slow, consider using direct pointers
  (let ((n (dims mat)))
    (assert (= n (length index)))
    (with-foreign-array (idxarr :int
                                :initial-contents index)
      (cv-mat-get-double (peer mat) idxarr))))

;; Mats (vector) ------------------------------

(defun mats-to-cv (mats)
  (let ((cm (cv-mats-new)))
    (loop for m in mats
          do (cv-mats-add cm (peer m)))
    cm))

(defun mats-from-cv (cmats)
  (loop for i below (cv-mats-count cmats)
     collect (mat :peer (cv-mat-copy (cv-mats-get cmats i)))))

;; Ticks ---------------------------------------

@export
(defun tick-count ()
  (cv-get-tick-count))

@export
(defun tick-frequency ()
  (cv-get-tick-frequency))
