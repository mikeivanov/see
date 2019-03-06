(in-package #:see)
(annot:enable-annot-syntax)

@export
(defun imread (path &key (flags (list :imread-color)))
  (let ((cflags (reduce #'+ (mapcar (lambda (flag)
                                      (cffi:foreign-enum-value 'cv-imread-modes flag))
                                    flags))))
    (mat :peer
         (check-cv-error #'cffi:null-pointer-p
                         (cv-imread path cflags)))))
