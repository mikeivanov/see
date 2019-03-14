;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)
(annot:enable-annot-syntax)

@export
(defclass net (proxy) ())

(defun net-from-cv (cnet)
  (make-instance 'net
                 :peer cnet
                 :release #'cv-dnn-net-free))

(defmethod empty-p ((net net))
  (cv-dnn-net-empty (peer net)))

(defun configure-net (net preferable-backend preferable-target)
  (when preferable-backend
    (check-cv-error #'null
                    (cv-dnn-net-set-preferable-backend
                       net
                       (cffi:foreign-enum-value 'cv-backend
                                                preferable-backend))))
  (when preferable-target
    (check-cv-error #'null
                    (cv-dnn-net-set-preferable-target
                       net
                       (cffi:foreign-enum-value 'cv-target
                                                preferable-target)))))

@export
(defun read-net (path &key (config "") (framework "") preferable-backend preferable-target)
  (let ((net (see::cv-dnn-read-net (namestring path) (namestring config) framework)))
    (check-cv-error #'cffi:null-pointer-p net)
    (configure-net net preferable-backend preferable-target)
    (net-from-cv net)))

@export
(defun blob-from-images (images &key
                                  (scale 1d0)
                                  (size (size))
                                  (mean (scalar))
                                  swap-rb crop
                                  (depth :depth-32-f))
  (with-foreign-resource (cimages (mats-to-cv images)
                          :free cv-mats-free)
    (with-foreign-resource (cmean (scalar-to-cv mean)
                            :free cv-scalar-free)
      (with-foreign-resource (csize (size-to-cv size)
                            :free cv-size-free)
        (let ((blob (mat))
              (cdepth (cffi:foreign-enum-value 'cv-depths depth)))
          (check-cv-error #'null
                          (cv-dnn-blob-from-images cimages
                                                   (peer blob)
                                                   (float scale)
                                                   csize
                                                   cmean
                                                   swap-rb
                                                   crop
                                                   cdepth))
          blob)))))

@export
(defun set-net-input (net blob &key
                                 (name "")
                                 (scale 1d0)
                                 (mean (scalar)))
  (assert (eq :depth-32-f (depth blob)))
  (with-foreign-resource (cmean (scalar-to-cv mean)
                          :free cv-scalar-free)
    (check-cv-error #'null
                    (cv-dnn-net-set-input (peer net)
                                          (peer blob)
                                          name
                                          scale
                                          cmean)))
  net)

(defun forward-layer (net name)
  (let ((blob (cv-dnn-net-forward (peer net) name)))
    (check-cv-error #'cffi:null-pointer-p blob)
    (mat-from-cv blob)))

(defun forward-layers (net names)
  (with-foreign-resource (cnames (strings-to-cv names)
                          :free cv-strings-free)
    (let ((cblobs (cv-dnn-net-forward-layers (peer net) cnames)))
      (check-cv-error #'cffi:null-pointer-p cblobs)
      (with-foreign-resource (cblobs cblobs
                              :free cv-mats-free)
        (mats-from-cv cblobs)))))

@export
(defun forward (net &key names)
  (if (< 1 (length names))
      (forward-layers net names)
      (forward-layer net (or (first names) ""))))

@export
(defun nms-boxes (boxes scores score-threshold nms-threshold &key (eta 1.0) (top-k 0))
  (assert (= (length boxes) (length scores)))
  (with-foreign-resource (cboxes (rotated-rects-to-cv boxes)
                          :free cv-rotated-rects-free)
    (with-foreign-resource (cscores (floats-to-cv scores)
                            :free cffi:foreign-free)
      (with-foreign-resource (cindices (cv-dnn-nms-boxes cboxes
                                                         cscores
                                                         (as-single-float score-threshold)
                                                         (as-single-float nms-threshold)
                                                         eta
                                                         top-k)
                              :free cv-ints-free)
        (check-cv-error #'cffi:null-pointer-p cindices)
        (ints-from-cv cindices)))))

(defclass layer (proxy) ())

(defun layer-from-cv (clayer)
  (make-instance 'layer
                 :peer clayer
                 :release #'cv-dnn-layer-free))

@export
(defun layer-by-name (net name)
  (let ((clayer (cv-dnn-net-get-layer-with-name (peer net) name)))
    (check-cv-error #'cffi:null-pointer-p clayer)
    (layer-from-cv clayer)))

@export
(defun layer-by-id (net id)
  (let ((clayer (cv-dnn-net-get-layer-with-id (peer net) id)))
    (check-cv-error #'cffi:null-pointer-p clayer)
    (layer-from-cv clayer)))

@export
(defun layer-blobs (layer)
  (mats-from-cv (cv-dnn-layer-blobs (peer layer))))

@export
(defun add-blob (layer blob)
  (check-cv-error #'null
                  (cv-dnn-layer-add-blob (peer layer) (peer blob))))

@export
(defun layer-names (net)
  (with-foreign-resource (cstrings (cv-dnn-net-get-layer-names (peer net))
                                  :free cv-strings-free)
    (check-cv-error #'cffi:null-pointer-p cstrings)
    (strings-from-cv cstrings)))

@export
(defun performance-profile (net)
  (with-foreign-resource (ctimes (cv-doubles-new)
                          :free cv-doubles-free)
    (let ((result (cv-dnn-net-get-perf-profile (peer net)
                                               ctimes)))
      (values result
              (doubles-from-cv ctimes)))))
