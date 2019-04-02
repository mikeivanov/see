;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)

(defun get-foreign-enum-defs (var)
  (let ((var-ptr (cffi:get-var-pointer var)))
    (loop for i from 0
          for ptr = (cffi:mem-aptr var-ptr '(:struct cv-enum) i)
          for pair = (cffi:with-foreign-slots ((name value) ptr (:struct cv-enum))
                       (when (not (null name))
                         (list name value)))
          while pair
          collect pair)))

(defun make-enum-symbol (name)
  (symbolicate (string-upcase (substitute #\- #\_ name))))

(defmacro defenum (symbol)
  (let* ((name (symbol-name symbol))
         (var  (make-enum-symbol (format nil "*~a*" name)))
         (defs (get-foreign-enum-defs var)))
    `(progn
       (cffi:defcenum ,symbol
         ,@(loop for (n v) in defs
                 for s = (make-enum-symbol n)
                 collect `(,s ,v)))
       ,@(loop for d in defs
               for s = (make-enum-symbol (car d))
               collect `(export (quote ,s))))))
