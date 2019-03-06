(ql:quickload "see-examples")
(in-package #:see.examples)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter +model-file-name+ "frozen_east_text_detection.pb")
(defparameter +model-input-width+ 320)
(defparameter +model-input-height+ 320)
(defparameter +model-output-layers+ '("feature_fusion/Conv_7/Sigmoid"
                                      "feature_fusion/concat_3"))
(defparameter +model-input-mean+ (scalar 123.68d0 116.78d0 103.94d0 0d0))
(defparameter +confidence-threshold+ 0.5)
(defparameter +nms-threshold+ 0.4)

(defun region-box (geometry x y sx sy)
  (let* ((input-x (* x sx))
         (input-y (* y sy))
         ;; four offsets from the center point (input-x, input-y)
         (left   (at geometry (vector 0 0 y x)))
         (top    (at geometry (vector 0 1 y x)))
         (right  (at geometry (vector 0 2 y x)))
         (bottom (at geometry (vector 0 3 y x)))
         (alpha  (at geometry (vector 0 4 y x)))
         (h (+ left right))
         (w (+ top bottom))
         (cos-a (cos alpha))
         (sin-a (sin alpha))
         (off-x (+ input-x
                   (* cos-a top)
                   (* sin-a right)))
         (off-y (+ input-y
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
  (let* ((shape (shape scores))
         (sh    (elt shape 2))
         (sw    (elt shape 3))
         (boxes nil)
         (confs nil))
    (loop for y from 0 below sh
          append (loop for x from 0 below sw
                       for score = (at scores (vector 0 0 y x))
                       when (< threshold score)
                         do (progn
                              (push (region-box geometry x y sx sy) boxes)
                              (push score confs))))
    (values (nreverse boxes)
            (nreverse confs))))

(defun detect-text (net frame &key
                        (input-size (size +model-input-width+
                                          +model-input-height+))
                        (confidence-threshold +confidence-threshold+)
                        (nms-threshold +nms-threshold+))
  (with-resource (blob (blob-from-images (list frame)
                                         :size input-size
                                         :mean +model-input-mean+
                                         :swap-rb t))
    (set-net-input net blob)
    (destructuring-bind (scores geometry)
        (forward net :names +model-output-layers+)
      (let* ((sz (shape scores))
             (sh (elt sz 2))
             (sw (elt sz 3))
             (mw (width input-size))
             (mh (height input-size))
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
                         (width +model-input-width+)
                         (height +model-input-height+)
                         (confidence-threshold +confidence-threshold+)
                         (nms-threshold +nms-threshold+))
  (let ((input-size (size width height)))
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
                                           :input-size input-size
                                           :confidence-threshold confidence-threshold
                                           :nms-threshold nms-threshold))
                     (freq (/ (tick-frequency) 1000.0))
                     (time (/ (performance-profile net) freq)))
                (draw-regions img regions)
                (draw-perf-info img (format nil "Inference time: ~,2f ms" time))
                (show-image win img))))
          (trivial-garbage:gc)
          (< key 0))))))

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
  (:name :width
   :short #\w
   :long "width"
   :arg-parser #'parse-integer
   :description (concatenate 'string
                             "Preprocess input image by resizing to a specific width. "
                             "It should be multiple by 32. "
                             "Default value is 320."))
  (:name :height
   :short #\h
   :long "height"
   :arg-parser #'parse-integer
   :description (concatenate 'string
                             "Preprocess input image by resizing to a specific height. "
                             "It should be multiple by 32. "
                             "Default value is 320."))
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
