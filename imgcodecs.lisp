;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)
(annot:enable-annot-syntax)

(defun imread-flags-to-cv (mode load-gdal-p ignore-orientation-p)
  (+ (cffi:foreign-enum-value 'cv-imread-modes mode)
     (if load-gdal-p 8 0)
     (if ignore-orientation-p 128 0)))

(defun read-image-single (path flags destination)
  (check-type destination mat)
  (cv-call cv-imread path flags (peer destination))
  destination)

(defun read-image-multi (path flags destination)
  (check-type destination list)
  (with-foreign-resource (cmats (mats-to-cv destination)
                          :free cv-mats-free)
    (cv-call cv-imread-multi path cmats flags)
    (let ((mats (mats-from-cv cmats)))
      (nconc destination (nthcdr (length destination) mats)))))

@export
(defun read-image (path &key
                          (mode :imread-anycolor)
                          load-gdal-p ignore-orientation-p
                          multi-p destination)
  (let ((flags (+ (cffi:foreign-enum-value 'cv-imread-modes mode)
                  (if load-gdal-p 8 0)
                  (if ignore-orientation-p 128 0))))
    (if multi-p
        (read-image-multi  path flags (or destination (list)))
        (read-image-single path flags (or destination (mat))))))

(defun imwrite-flags-to-cv (flags)
  (ints-to-cv
   (loop for (k v) on flags by #'cddr while k
         append (let* ((cf (cffi:foreign-enum-value 'cv-imwrite-flags k))
                       (cv (etypecase v
                             (number  (round v))
                             (boolean (if v 1 0)))))
                  (list cf cv)))))

@export
(defun write-image (path image &rest flags)
  (with-foreign-resource (cflags (imwrite-flags-to-cv flags)
                          :free cv-ints-free)
    (cv-call cv-imwrite path image cflags)))
