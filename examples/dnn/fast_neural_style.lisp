;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

;; This script is used to run style transfer models from
;; https://github.com/jcjohnson/fast-neural-style
;; Download the models using the fast_neural_style_model_download.sh script.

(ql:quickload "see-examples")
(in-package #:see.examples)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter *model-path* "fast_neural_style_models/instance_norm/candy.t7")
(defparameter *use-opencl* nil)
(defparameter *blob-mean* (scalar 103.939 116.779 123.68 0))
(defparameter *median-filter* 0)
(defparameter *image-width* nil)
(defparameter *image-height* nil)

(defvar *model* nil)

(defun load-model ()
  (setf *model*
        (read-net *model-path*
                  :preferable-target (when *use-opencl*
                                       :dnn-target-opencl))))

(defun stylize-image (img)
  (let* ((size (size (or *image-width*  (cols img))
                     (or *image-height* (rows img))))
         (input (blob-from-images (list img)
                                  :size size
                                  :mean *blob-mean*
                                  :swap-rb nil
                                  :crop nil)))
    (let* ((out (forward *model* :input input))
           ;; TODO: this can be done in one step
           (b   (add (slice out '(0 0)) 103.939))
           (g   (add (slice out '(0 1)) 116.779))
           (r   (add (slice out '(0 2)) 123.68))
           (bgr (merge-channels (list b g r)))
           (res (mul bgr (/ 255d0))))
      (if (= 0 *median-filter*)
          res
          (median-blur res *median-filter*)))))

(defun print-net-perf (net)
  (let ((time (performance-profile net))
        (freq (/ (tick-frequency) 1000d0)))
    (format t "~sms~%" (/ time freq))))

(defun fast-neural-style (input)
  (load-model)
  (with-resource (capture (if input
                              (video-capture :uri input)
                              (video-capture :device 0)))
    (with-open-window ((win key)
                       :name "Image Style"
                       :delay 100)
      (with-resource (img (read-video-frame capture))
        (when (not (empty-p img))
          (with-resource (styled (stylize-image img))
            ;;(print-net-perf *model*)
            (show-image win styled)))
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
  (:name :width
   :long "width"
   :arg-parser #'parse-integer
   :description "Resize input to specific width.")
  (:name :height
   :long "height"
   :arg-parser #'parse-integer
   :description "Resize input to specific height.")
  (:name :use-opencl
   :long "use-opencl"
   :arg-parser (lambda (v) (or (null v) (string-equal "yes" v)))
   :description "(yes/no) Use OpenCL if available.")
  (:name :median-filter
   :short #\f
   :long "median-filter"
   :arg-parser #'parse-integer
   :description "Kernel size of postprocessing blurring."))

(defparameter +about+ "This script is used to run style transfer models.")

(defun main (&optional args)
  (let ((options (opts:get-opts (or args (opts:argv)))))
    (if (getf options :help)
        (opts:describe :prefix +about+)
        (macrolet ((setparameter (param key)
                     (let ((%val (gensym)))
                       `(when-let (,%val (getf options ,key))
                          (setf ,param ,%val)))))
          (setparameter *model-path* :model)
          (setparameter *image-width* :width)
          (setparameter *image-height* :height)
          (setparameter *median-filter* :median-filter)
          (setparameter *use-opencl* :use-opencl)
          (fast-neural-style (getf options :input))))))

(eval-when (:execute)
  (main))
