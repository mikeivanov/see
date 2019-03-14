;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(ql:quickload "see-examples")
(in-package #:see.examples)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter +model-file-name+ "frozen_east_text_detection.pb")
(defparameter +model-input-size+ (size 320 320))
(defparameter +model-output-layers+ '("feature_fusion/Conv_7/Sigmoid"
                                      "feature_fusion/concat_3"))
(defparameter +model-input-mean+ (scalar 123.68d0 116.78d0 103.94d0 0d0))
(defparameter +confidence-threshold+ 0.5)
(defparameter +nms-threshold+ 0.4)

(defun region-box (x y left top right bottom alpha sx sy)
  (let* ((h (+ left right))
         (w (+ top bottom))
         (cos-a (cos alpha))
         (sin-a (sin alpha))
         (off-x (+ (* x sx)
                   (* cos-a top)
                   (* sin-a right)))
         (off-y (+ (* y sy)
                   (* -1 sin-a top)
                   (* cos-a right)))
         (p1-x (+ (* (- sin-a) h) off-x))
         (p1-y (+ (* (- cos-a) h) off-y))
         (p3-x (+ (* (- cos-a) w) off-x))
         (p3-y (+ (* sin-a w) off-y))
         (cx (* 0.5 (+ p1-x p3-x)))
         (cy (* 0.5 (+ p1-y p3-y)))
         (angle (/ (* -180 alpha) pi)))
    (rotated-rect cx cy w h angle)))

(defun decode-boxes (scores geometry threshold sx sy)
  (let* ((scores (mat-to-array (slice scores '(0 0))))
         (geometry (mat-to-array (slice geometry '(0))))
         (boxes nil)
         (confs nil))
    (loop for y from 0 below (array-dimension scores 1)
          do (loop for x from 0 below (array-dimension scores 0)
                   for score = (aref scores y x)
                   when (< threshold score)
                     do (let (;; four offsets from the center point (x,y)
                              (left   (aref geometry 0 y x))
                              (top    (aref geometry 1 y x))
                              (right  (aref geometry 2 y x))
                              (bottom (aref geometry 3 y x))
                                  ;; rotation angle
                                  (alpha  (aref geometry 4 x y)))
                              (push (region-box x y left top right bottom alpha sx sy) boxes)
                              (push score confs))))
    (values (nreverse boxes)
            (nreverse confs))))

(defun detect-text (net frame &key
                        (confidence-threshold +confidence-threshold+)
                        (nms-threshold +nms-threshold+))
  (with-resource (blob (blob-from-images (list frame)
                                         :size +model-input-size+
                                         :mean +model-input-mean+
                                         :swap-rb t))
    (set-net-input net blob)
    (destructuring-bind (scores geometry)
        (forward net :names +model-output-layers+)
      (let* ((sz (shape scores))
             (sh (elt sz 2))
             (sw (elt sz 3))
             (mw (width +model-input-size+))
             (mh (height +model-input-size+))
             (sx (float (/ mw sw)))
             (sy (float (/ mh sh)))
             (fx (float (/ (cols frame) mw)))
             (fy (float (/ (rows frame) mh))))
        (multiple-value-bind (boxes confs)
            (decode-boxes scores geometry confidence-threshold sx sy)
          (let ((indices (nms-boxes boxes confs confidence-threshold nms-threshold)))
            (loop for i from 0
                  for box in boxes
                  when (member i indices)
                    collect (mapcar (lambda (bp)
                                      (point (* fx (x bp))
                                             (* fy (y bp))))
                                    (rotated-rect-points box)))))))))

(defun draw-regions (img regions)
  (loop for reg in regions
        do (loop for (a . rest) on reg
                 for b = (or (car rest) (car reg))
                 do (progn
                      (draw-line img a b
                                 :color (scalar 0 255 0 0)
                                 :thickness 2)))))

(defun draw-perf-info (img label)
  (draw-text img label
             :origin (point 0 15)
             :font-face :font-hershey-simplex
             :font-scale 0.5
             :color (scalar 0 255 0 0)))

(defun text-detection (&key
                         input
                         (model +model-file-name+)
                         (confidence-threshold +confidence-threshold+)
                         (nms-threshold +nms-threshold+))
  (with-resource (net (read-net model))
    (with-resource (capture (if input
                                (video-capture :uri input)
                                (video-capture :device 0)))
      (with-open-window ((win key)
                         :name "Text Recognition"
                         :delay 50)
        (with-resource (img (read-video-frame capture))
          (when (not (empty-p img))
            (let* ((regions (detect-text net img
                                         :confidence-threshold confidence-threshold
                                         :nms-threshold nms-threshold))
                   (freq (/ (tick-frequency) 1000.0))
                   (time (/ (performance-profile net) freq)))
              (draw-regions img regions)
              (draw-perf-info img (format nil "Inference time: ~,2f ms" time))
              (show-image win img))))
        (trivial-garbage:gc)
        (< key 0)))))

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
   :description "Path to a binary .pb file contains trained network.")
  (:name :confidence-threshold
   :long "thr"
   :arg-parser #'parse-number:parse-positive-real-number
   :description "Confidence threshold. Default value is 0.5.")
  (:name :nms-threshold
   :long "nms"
   :arg-parser #'parse-number:parse-positive-real-number
   :description "Non-maximum suppression threshold. Default value is 0.4."))

(defparameter +about+
  (concatenate 'string
               "Use this script to run TensorFlow implementation "
               "(https://github.com/argman/EAST) of EAST: An Efficient "
               "and Accurate Scene Text Detector (https://arxiv.org/abs/1704.03155v2). "
               "Download the net file from here: "
               "https://www.dropbox.com/s/r2ingd0l3zt8hxs/frozen_east_text_detection.tar.gz?dl=1"))

(defun main (&optional args)
  (let ((options (opts:get-opts (or args (opts:argv)))))
    (if (getf options :help)
        (opts:describe :prefix +about+)
        (apply #'text-detection options))))

(eval-when (:execute)
  (main))
