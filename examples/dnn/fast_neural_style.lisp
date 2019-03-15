;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

;; This script is used to run style transfer models from
;; https://github.com/jcjohnson/fast-neural-style
;; Download the models using the fast_neural_style_model_download.sh script.

(ql:quickload "see-examples")
(in-package #:see.examples)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter +model-path+ "fast_neural_style_models/instance_norm/candy.t7")

(defun prepare-model (model-path use-open-cl)
  (read-net model-path
            :preferable-target (when use-open-cl :dnn-target-opencl)))

(defun stylize-image (net img width height median-filter)
  (let* ((size (size (if (= -1 width) (cols img) width)
                     (if (= -1 height) (rows img) height)))
         (input (blob-from-images (list img)
                                  :size size
                                  :mean (scalar 103.939 116.779 123.68 0)
                                  :swap-rb nil
                                  :crop nil)))
    (set-net-input net input)
    (let* ((out (forward net))
           (b (add (slice out '(0 0)) 103.939))
           (g (add (slice out '(0 1)) 116.779))
           (r (add (slice out '(0 2)) 123.68))
           (bgr (merge-channels b g r))
           (res (mul bgr (/ 255d0))))
      (if (= 0 median-filter)
          res
          (median-blur res median-filter)))))

(defun print-net-perf (net)
  (let ((time (performance-profile net))
        (freq (/ (tick-frequency) 1000d0)))
    (format t "~sms~%" (/ time freq))))

(defun stylize-stream (net capture width height median-filter)
  (with-open-window ((win key)
                     :name "Stylizing"
                     :delay 100)
    (with-resource (img (read-video-frame capture))
      (when (not (empty-p img))
        (let ((styled (stylize-image net img width height median-filter)))
          (print-net-perf net)
          (show-image win styled)
          (trivial-garbage:gc))))
    (< key 0)))

(defun fast-neural-style (&key
                            input
                            (model +model-path+)
                            (width -1)
                            (height -1)
                            (median-filter 0)
                            (use-open-cl t))
  (with-resource (net (prepare-model model use-open-cl))
    (with-resource (capture (if input
                                (video-capture :uri input)
                                (video-capture :device 0)))
      (stylize-stream net capture width height median-filter))))

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
   :short #\w
   :long "width"
   :arg-parser #'parse-integer
   :description "Resize input to specific width.")
  (:name :height
   :short #\h
   :long "height"
   :arg-parser #'parse-integer
   :description "Resize input to specific height.")
  (:name :use-open-cl
   :short #\c
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
        (apply #'fast-neural-style options))))

(eval-when (:execute)
  (main))
