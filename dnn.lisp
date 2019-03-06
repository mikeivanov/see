(in-package #:see)
(annot:enable-annot-syntax)

@export
(defclass net (proxy) ())

@export
(defun net (&key peer)
  (assert peer)
  (make-instance 'net
                 :peer peer
                 :release #'cv-dnn-net-free))

(defmethod empty-p ((net net))
  (cv-dnn-net-empty (peer net)))

@export
(defun read-net (path &key (config "") (framework ""))
  (let ((net (see::cv-dnn-read-net path config framework)))
    (check-cv-error #'cffi:null-pointer-p net)
    (net :peer net)))

@export
(defun blob-from-images (images &key
                                  (scale 1d0)
                                  (size (size))
                                  (mean (scalar))
                                  swap-rb crop
                                  (depth +CV-32-F+))
  (with-foreign-resource (cimages (mats-to-cv images)
                          :free cv-mats-free)
    (with-foreign-resource (cmean (scalar-to-cv mean)
                            :free cv-scalar-free)
      (with-foreign-resource (csize (size-to-cv size)
                            :free cv-size-free)
        (let ((blob (mat)))
          (check-cv-error #'null
                          (cv-dnn-blob-from-images cimages
                                                   (peer blob)
                                                   (float scale)
                                                   csize
                                                   cmean
                                                   swap-rb
                                                   crop
                                                   depth))
          blob)))))

@export
(defun set-net-input (net blob &key
                                 (name "")
                                 (scale 1d0)
                                 (mean (scalar)))
  (assert (= +CV-32-F+ (depth blob)))
  (with-foreign-resource (cmean (scalar-to-cv mean)
                          :free cv-scalar-free)
    (check-cv-error #'null
                    (cv-dnn-net-set-input (peer net)
                                          (peer blob)
                                          name
                                          scale
                                          cmean)))
  net)

@export
(defun forward (net &key names)
  (with-foreign-resource (cnames (strings-to-cv names)
                          :free cv-strings-free)
    (with-foreign-resource (cblobs (cv-mats-new)
                            :free cv-mats-free)
      (check-cv-error #'null
                      (cv-dnn-net-forward-layers (peer net)
                                                 cnames
                                                 cblobs))
      (mats-from-cv cblobs))))

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
