;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)
(annot:enable-annot-syntax)

(defenum cv-dnn-backend-enum)
(defenum cv-dnn-target-enum)

@export
(defclass net (proxy) ())

(defun net-from-cv (cnet)
  (make-instance 'net
                 :peer cnet
                 :release (lambda (self) (cv-call cv-dnn-net-free self))))

(defmethod empty-p ((net net))
  (cv-call cv-dnn-net-empty (peer net)))

(defun configure-net (net &key preferable-backend preferable-target)
  (when preferable-backend
    (cv-call cv-dnn-net-set-preferable-backend
             (peer net)
             (cffi:foreign-enum-value 'cv-backend preferable-backend)))
  (when preferable-target
    (cv-call cv-dnn-net-set-preferable-target
             (peer net)
             (cffi:foreign-enum-value 'cv-target preferable-target)))
  net)

@export
(defun read-net (path &key (config "") (framework "") preferable-backend preferable-target)
  (let* ((cnet (cv-call-out cv-dnn-read-net
                            (namestring path)
                            (namestring config)
                            framework
                            :pointer))
         (net (net-from-cv cnet)))
    (configure-net net
                   :preferable-backend preferable-backend
                   :preferable-target preferable-target)))

@export
(defun blob-from-images (images &key
                                  (scale 1d0)
                                  (size (size))
                                  (mean (scalar))
                                  swap-rb crop
                                  (depth 'depth-32f))
  (with-foreign-resource (cimages (mats-to-cv images)
                          :free cv-mats-free)
    (with-foreign-resource (cmean (scalar-to-cv mean)
                            :free cv-scalar-free)
      (with-foreign-resource (csize (size-to-cv size)
                              :free cv-size-free)
        (let ((blob (mat))
              (cdepth (cffi:foreign-enum-value 'cv-depth-enum depth)))
          (cv-call cv-dnn-blob-from-images
                   cimages
                   (peer blob)
                   (float scale)
                   csize
                   cmean
                   swap-rb
                   crop
                   cdepth)
          blob)))))

@export
(defun set-net-input (net blob &key
                                 (name "")
                                 (scale 1d0)
                                 (mean (scalar)))
  (assert (eq 'depth-32f (depth blob)))
  (with-foreign-resource (cmean (scalar-to-cv mean)
                          :free cv-scalar-free)
    (cv-call cv-dnn-net-set-input
             (peer net)
             (peer blob)
             name
             scale
             cmean)
    net))

(defun forward-layer (net name blob)
  (let ((blob (or blob (mat))))
    (check-type blob mat)
    (cv-call cv-dnn-net-forward (peer net) name (peer blob))
    blob))

(defun forward-layers (net names blobs)
  (check-type blobs list)
  (with-foreign-resource (cblobs (mats-to-cv blobs)
                          :free cv-mats-free)
    (with-foreign-resource (cnames (strings-to-cv names)
                            :free cv-strings-free)
      (cv-call cv-dnn-net-forward-layers (peer net) cnames cblobs)
        (mats-from-cv cblobs))))

@export
(defun forward (net &key names input destination)
  (when input
    (set-net-input net input))
  (if (< 1 (length names))
      (forward-layers net names destination)
      (forward-layer net (or (first names) "") destination)))

@export
(defun nms-boxes (boxes scores score-threshold nms-threshold &key (eta 1.0) (top-k 0))
  (assert (= (length boxes) (length scores)))
  (with-foreign-resource (cboxes (rotated-rects-to-cv boxes)
                          :free cv-rotated-rects-free)
    (with-foreign-resource (cscores (floats-to-cv scores)
                            :free cv-floats-free)
      (with-foreign-resource (cindices (ints-to-cv nil)
                              :free cv-ints-free)
        (cv-call cv-dnn-nms-boxes
                 cboxes
                 cscores
                 (as-single-float score-threshold)
                 (as-single-float nms-threshold)
                 eta
                 top-k
                 cindices)
        (ints-from-cv cindices)))))

(defclass layer (proxy) ())

(defun layer-from-cv (clayer)
  (make-instance 'layer
                 :peer clayer
                 :release (lambda (self) (cv-dnn-layer-free self))))

@export
(defun layer-by-name (net name)
  (layer-from-cv (cv-call-out cv-dnn-net-get-layer-with-name
                              (peer net)
                              name
                              :pointer)))

@export
(defun layer-by-id (net id)
  (layer-from-cv (cv-call-out cv-dnn-net-get-layer-with-id
                              (peer net)
                              id
                              :pointer)))

@export
(defun layer-blobs (layer)
  (with-foreign-resource (cmats (mats-to-cv nil) :free cv-free-mats)
    (cv-call cv-dnn-layer-blobs (peer layer) cmats)
    (mats-from-cv cmats)))

@export
(defun add-blob (layer blob)
  (cv-call cv-dnn-layer-add-blob (peer layer) (peer blob))
  blob)

@export
(defun layer-names (net)
  (with-foreign-resource (cstrings (strings-to-cv nil)
                          :free cv-strings-free)
    (cv-call cv-dnn-net-get-layer-names (peer net) cstrings)
    (strings-from-cv cstrings)))

@export
(defun performance-profile (net)
  (with-foreign-resource (ctimes (cv-call-out cv-doubles-new :pointer)
                          :free cv-doubles-free)
    (cffi:with-foreign-object (ctime :int)
      (cv-call cv-dnn-net-get-perf-profile
               (peer net)
               ctimes
               ctime)
      (values (cffi:mem-ref ctime :int)
              (doubles-from-cv ctimes)))))
