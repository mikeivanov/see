;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)
(annot:enable-annot-syntax)

@export
(defun read-image (path &key (flags (list :imread-color)))
  (let ((cflags (reduce #'+ (mapcar (lambda (flag)
                                      (cffi:foreign-enum-value 'cv-imread-modes flag))
                                    flags))))
    (mat-from-cv
         (check-cv-error #'cffi:null-pointer-p
                         (cv-imread path cflags)))))
