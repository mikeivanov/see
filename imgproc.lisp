;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)
(annot:enable-annot-syntax)

;; Transforms ---------------------------

@export
(defun convert-colorspace (img code &key target (channels 0))
  (let ((dst (or target (mat))))
    (check-cv-error #'null
                    (cv-cvt-color (peer img)
                                  (peer dst)
                                  (cffi:foreign-enum-value 'cv-color-conversion-codes code)
                                  channels))
    dst))

@export
(defun resize-image (img size &key
                                target (fx 0d0) (fy 0d0)
                                (interpolation :inter-linear)
                                (warp :warp-none))
  (assert (= 2 (dims img)))
  (let ((dst (or target (mat)))
        (flags (+ (cffi:foreign-enum-value 'cv-interpolation-flags
                                           interpolation)
                  (cffi:foreign-enum-value 'cv-interpolation-warp
                                           warp))))
    (with-foreign-resource (csize (size-to-cv size)
                            :free cv-size-free)
      (check-cv-error #'null
                      (cv-resize (peer img)
                                 (peer dst)
                                 csize
                                 (as-double-float fx)
                                 (as-double-float fy)
                                 flags)))
    dst))

;; Drawing ------------------------

@export
(defun draw-line (img a b &key
                            (color (scalar))
                            (thickness 1)
                            (line-type 8)
                            (shift 0))
  (with-foreign-resource (ca (point-to-cv a))
    (with-foreign-resource (cb (point-to-cv b))
      (with-foreign-resource (cc (scalar-to-cv color))
        (cv-line (peer img)
                 ca cb
                 cc
                 thickness
                 line-type
                 shift)))))

@export
(defun draw-text (mat text &key
                             (origin (point 0 0))
                             (font-face :font-hershey-plain)
                             (font-scale 1.0)
                             (color (scalar))
                             (thickness 1)
                             (line-type :line-8)
                             (bottom-left-origin nil))
  (with-foreign-resource (corigin (point-to-cv origin)
                          :free cv-point-free)
    (with-foreign-resource (ccolor (scalar-to-cv color)
                            :free cv-scalar-free)
      (cv-put-text (peer mat)
                   text
                   corigin
                   (cffi:foreign-enum-value 'cv-hershey-fonts font-face)
                   (as-double-float font-scale)
                   ccolor
                   thickness
                   (cffi:foreign-enum-value 'cv-line-types line-type)
                   bottom-left-origin)
      mat)))
