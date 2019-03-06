(in-package #:see)
(annot:enable-annot-syntax)

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