;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

;; Script is based on
;; https://docs.opencv.org/4.0.1/d6/d39/samples_2dnn_2colorization_8cpp-example.html
;; Which in turn is based on
;; https://github.com/richzhang/colorization/blob/master/colorization/colorize.py
;; To download the caffemodel and the prototxt, see:
;; https://github.com/richzhang/colorization/tree/master/colorization/models
;; To download pts_in_hull.npy, see:
;; https://github.com/richzhang/colorization/blob/master/colorization/resources/pts_in_hull.npy

(ql:quickload "see-examples")
(in-package #:see.examples)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter +model-path+ "colorization_release_v2.caffemodel")
(defparameter +proto-path+ "colorization_deploy_v2.prototxt")
(defparameter +kernel-path+ "pts_in_hull.csv")
(defparameter +model-in-size+ (size 224 224))

;; the 313 ab cluster centers from pts_in_hull.csv
(defun read-hull-points (path)
  (let* ((points (cl-csv:read-csv (pathname path)
                                  :data-map-fn (lambda (datum &key &allow-other-keys)
                                                 (parse-number:parse-real-number datum))))
         (points (make-array '(313 2)
                             :initial-contents points
                             :element-type 'single-float))
         (points (array-transpose points))
         (points (array-reshape points '(2 313 1 1))))
    (mat :initial-contents points
         :depth :depth-32-f)))

(defun prepare-model (model-path proto-path kernel-path use-open-cl)
  (let ((net (read-net model-path
                       :config (pathname proto-path)
                       :preferable-target (when use-open-cl :dnn-target-opencl))))
    (let ((hull (read-hull-points kernel-path))
          (conv (mat :shape '(1 313)
                     :depth :depth-32-f
                     :initial-element 2.606))
          (class8-ab    (layer-by-name net "class8_ab"))
          (conv8-313-rh (layer-by-name net "conv8_313_rh")))
      (add-blob class8-ab hull)
      (add-blob conv8-313-rh conv))
    net))

(defun image-luminance (img)
  (let* ((img (convert-to img :depth-32-f :alpha (/ 255.0)))
         (lab (convert-colorspace img :color-bgr-2-lab)))
    ;; Extract luminance
    (extract-channel lab 0)))

(defun colorize-grayscale-image (net img)
  (assert (= 1 (channels img)))
  (assert (eq :depth-32-f (depth img)))
  (let* ((input (resize-image img +model-in-size+))
         (input (add input -50.0))
         (input (blob-from-images (list input))))
    (set-net-input net input)
    (let* ((output (forward net))
           (size (size (cols img) (rows img)))
           (a (resize-image (slice output '(0 0)) size))
           (b (resize-image (slice output '(0 1)) size))
           ;; merge, and convert back to BGR
           (c (merge-channels img a b)))
      (convert-colorspace c :color-lab-2-bgr))))

(defun colorize-stream (net capture)
  (with-open-window ((win key)
                     :name "Coloring"
                     :delay 100)
    (with-resource (img (read-video-frame capture))
      (when (not (empty-p img))
        (let* ((gray (image-luminance img))
               (color (colorize-grayscale-image net gray)))
          (show-image win color)
          (trivial-garbage:gc))))
    (< key 0)))

(defun colorization (&key
                       input
                       (model +model-path+)
                       (proto +proto-path+)
                       (kernel +kernel-path+)
                       (use-open-cv t))
  (with-resource (net (prepare-model model proto kernel use-open-cv))
    (with-resource (capture (if input
                                (video-capture :uri input)
                                (video-capture :device 0)))
      (colorize-stream net capture))))

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
  (:name :proto
   :short #\p
   :long "proto"
   :arg-parser #'identity
   :description "Path to colorization_deploy_v2.prototxt.")
  (:name :model
   :short #\m
   :long "model"
   :arg-parser #'identity
   :description "Path to colorization_release_v2.caffemodel.")
  (:name :kernel
   :short #\k
   :long "kernel"
   :arg-parser #'identity
   :description "Path to pts_in_hull.csv.")
  (:name :use-open-cl
   :short #\c
   :long "use-opencl"
   :arg-parser (lambda (v) (or (null v) (string-equal "yes" v)))
   :description "(yes/no) Use OpenCL if available."))

(defparameter +about+ "iColor: deep interactive colorization")

(defun main (&optional args)
  (let ((options (opts:get-opts (or args (opts:argv)))))
    (if (getf options :help)
        (opts:describe :prefix +about+)
        (apply #'colorization options))))

(eval-when (:execute)
  (main))
