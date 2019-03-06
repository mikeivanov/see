%module libcvc

%typemap(cin) bool ":bool";
%typemap(cout) bool ":bool";
%typemap(cin) size_t ":long";
%typemap(cout) size_t ":long";
%typemap(cin) int64_t ":long";
%typemap(cout) int64_t ":long";
%typemap(cin) int32_t ":int";
%typemap(cout) int32_t ":int";

%feature("intern_function", "cl-swig-lispify");

%insert("lisphead") %{
(in-package :see)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (progn
   (cffi:define-foreign-library libcvc
    (:darwin (:or "libcvc.0.dylib" "libcvc.dylib"))
    (:unix (:or "libcvc.so.0.1" "libcvc.so"))
    (:windows "libcvc.dll")
    (t "libcvc"))
   (cffi:use-foreign-library libcvc)))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'cl-swig-lispify)
    (cl:defun cl-swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        (constant "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))
%}

%include "libcvc/version.h"
%include "libcvc/types.h"
%include "libcvc/core.h"
%include "libcvc/imgcodecs.h"
%include "libcvc/imgproc.h"
%include "libcvc/highgui.h"
%include "libcvc/videoio.h"
%include "libcvc/dnn.h"
