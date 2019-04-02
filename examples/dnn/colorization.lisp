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

(defparameter *model-path* "colorization_release_v2.caffemodel")
(defparameter *proto-path* "colorization_deploy_v2.prototxt")
(defparameter *kernel-path* "pts_in_hull.csv")
(defparameter *use-opencl* nil)
(defparameter *model-input-size* (size 224 224))

(defvar *model* nil)

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
         :depth 'depth-32f)))

(defun load-model ()
  (let ((net (read-net *model-path*
                       :config (pathname *proto-path*)
                       :preferable-target (when *use-opencl* :dnn-target-opencl))))
    (let ((hull (read-hull-points *kernel-path*))
          (conv (mat :shape '(1 313)
                     :depth 'depth-32f
                     :initial-element 2.606))
          (class8-ab    (layer-by-name net "class8_ab"))
          (conv8-313-rh (layer-by-name net "conv8_313_rh")))
      (add-blob class8-ab hull)
      (add-blob conv8-313-rh conv))
    (setf *model* net)
    net))

(defun image-luminance (img)
  (let* ((img (convert-to img 'depth-32f :alpha (/ 255.0)))
         (lab (convert-colorspace img 'color-bgr2lab)))
    ;; Extract luminance
    (extract-channel lab 0)))

(defun colorize-image (img)
  (let* ((lum   (image-luminance img))
         (input (resize-image lum *model-input-size*))
         (input (add input -50.0))
         (input (blob-from-images (list input)))
         (output (forward *model* :input input))
         (size (size (cols img) (rows img)))
         ;; TODO: do it in one step
         (a (resize-image (slice output '(0 0)) size))
         (b (resize-image (slice output '(0 1)) size))
         ;; merge and convert back to BGR
         (c (merge-channels (list lum a b))))
      (convert-colorspace c 'color-lab2bgr)))

(defun colorization (input)
  (load-model)
  (with-resource (capture (if input
                              (video-capture :uri input)
                              (video-capture :device 0)))
    (with-open-window ((win key)
                       :name "Coloring"
                       :delay 100)
      (with-resource (img (read-video-frame capture))
        (when (not (empty-p img))
          (with-resource (out (colorize-image img))
            (show-image win out)))
        (trivial-garbage:gc :full (= 0 (random 5))))
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
  (:name :proto
   :long "proto"
   :arg-parser #'identity
   :description "Path to colorization_deploy_v2.prototxt.")
  (:name :model
   :long "model"
   :arg-parser #'identity
   :description "Path to colorization_release_v2.caffemodel.")
  (:name :kernel
   :long "kernel"
   :arg-parser #'identity
   :description "Path to pts_in_hull.csv.")
  (:name :use-opencl
   :long "use-opencl"
   :arg-parser (lambda (v) (or (null v) (string-equal "yes" v)))
   :description "(yes/no) Use OpenCL if available."))

(defparameter +about+ "iColor: deep interactive colorization")

(defun main (&optional args)
  (let ((options (opts:get-opts (or args (opts:argv)))))
    (if (getf options :help)
        (opts:describe :prefix +about+)
        (macrolet ((setparameter (param key)
                     (let ((%val (gensym)))
                       `(when-let (,%val (getf options ,key))
                          (setf ,param ,%val)))))
          (setparameter *model-path* :model)
          (setparameter *proto-path* :proto)
          (setparameter *kernel-path* :kernel)
          (setparameter *use-opencl* :use-opencl)
          (colorization (getf options :input))))))

(eval-when (:execute)
  (main))
