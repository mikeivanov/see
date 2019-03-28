;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

;; Use this script to run classification deep learning networks using OpenCV.

;; Based on:
;;   https://github.com/opencv/opencv/blob/master/samples/dnn/classification.py
;; See also:
;;   https://docs.opencv.org/4.0.1/d5/de7/tutorial_dnn_googlenet.html

(ql:quickload "see-examples")
(in-package #:see.examples)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter *model-path* "classification_models/squeezenet_v1.1.caffemodel")
(defparameter *config-path* "classification_models/squeezenet_v1.1.deploy.prototxt")
(defparameter *model-input-size* (size 227 227))
(defparameter *classes-path* "classification_models/classification_classes_ILSVRC2012.txt")
(defparameter *use-opencl* nil)
(defparameter *confidence-threhsold* 0.5)
(defparameter *view-box-size* (size 800 800))

(defvar *model* nil)
(defvar *classes* nil)

(defun read-classes (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil 'eof)
          until (eq line 'eof)
          collect line into lines
          finally (return
                    (make-array (length lines)
                                :initial-contents lines)))))

(defun load-model ()
  (setf *model* (read-net *model-path*
                          :config *config-path*
                          :preferable-target *use-opencl*)
        *classes* (read-classes *classes-path*))
  nil)

(defun array-order (array predicate &key (key #'identity))
  (let ((index (iota (length array))))
    (sort index predicate
          :key (lambda (i) (funcall key (aref array i))))))

(defun classify-image (img)
  (let* ((blob (blob-from-images (list img)
                                 :size *model-input-size*
                                 :swap-rb nil))
         (out  (forward *model* :input blob))
         (conf (flatten-array (mat-to-array out)))
         (idx  (array-order conf #'>)))
    (loop for k below 10
          for i in idx
          for c = (aref conf i)
          while (< *confidence-threhsold* c)
          for n = (aref *classes* i)
          collect (list i n c))))

(defun process-frame (frame)
  (let* ((classes (classify-image frame))
         (class (if classes
                    (first classes)
                    (list "Unknown" 0 0e0)))
         (time (performance-profile *model*))
         (freq (/ (tick-frequency) 1000e0))
         (inft (/ time freq))
         (view (fit-image frame *view-box-size*)))
    (draw-text view (format nil "Inference time: ~,3fms" inft)
               :origin (point 20 25)
               :font-scale 1.0
               :color (scalar 0 255 0 0))
    (destructuring-bind (id name conf) class
      (draw-text view (format nil "~s [~s]: ~,2f" name id conf)
                 :origin (point 20 40)
                 :font-scale 1.0
                 :color (scalar 0 255 0 0)))
    view))

(defun classification (input)
  (load-model)
  (with-resource (capture (if input
                              (video-capture :uri input)
                              (video-capture :device 0)))
    (with-open-window ((win key)
                       :name "Image Classification"
                       :delay 50)
      (with-resource (frame (read-video-frame capture))
        (when (not (empty-p frame))
          (with-resource (frame (process-frame frame))
            (show-image win frame)))
        (trivial-garbage:gc :full (= 0 (random 10))))
      (< key 0))))

(opts:define-opts
  (:name :help
   :short #\h
   :long "help"
   :description "Print help message.")
  (:name :input
   :short #\i
   :long "input"
   :arg-parser #'identity
   :description (concatenate 'string
                             "Path to input image or video file. "
                             "Skip this argument to capture frames from a camera."))
  (:name :model
   :short #\m
   :long "model"
   :arg-parser #'identity
   :description "Path to image or video. Skip to capture frames from camera.")
  (:name :use-opencl
   :long "use-opencl"
   :arg-parser (lambda (v) (or (null v) (string-equal "yes" v)))
   :description "(yes/no) Use OpenCL if available."))

(defparameter +about+ "Use this script to run classification deep learning networks using OpenCV.")

(defun main (&optional args)
  (let ((options (opts:get-opts (or args (opts:argv)))))
    (if (getf options :help)
        (opts:describe :prefix +about+)
        (macrolet ((setparameter (param key)
                     (let ((%val (gensym)))
                       `(when-let (,%val (getf options ,key))
                          (setf ,param ,%val)))))
          (setparameter *model-path* :model)
          (setparameter *use-opencl* :use-opencl)
          (classification (getf options :input))))))

(eval-when (:execute)
  (main))
