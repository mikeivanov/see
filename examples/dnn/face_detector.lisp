;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

;; This script is used to run style transfer models from
;; https://github.com/jcjohnson/fast-neural-style
;; Download the models using the fast_neural_style_model_download.sh script.

;; NOTE: this script assumes `zenity` being installed on your system.

(ql:quickload "see-examples")
(in-package #:see.examples)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter *detect-model-path*
  "face_detector_models/res10_300x300_ssd_iter_140000_fp16.caffemodel")
(defparameter *detect-proto-path*
  "face_detector_models/deploy.prototxt")
(defparameter *recogn-model-path*
  "face_detector_models/openface.nn4.small2.v1.t7")
(defparameter *use-opencl* nil)
(defparameter *confidence-threhsold* 0.5d0)
(defparameter *match-threshold* 0.4d0)
(defparameter *detect-model-input-size* (size 300 300))
(defparameter *detect-model-input-mean* (scalar 104 177 123 0))
(defparameter *recogn-model-input-size* (size 96 96))

(defvar *detect-model* nil)
(defvar *recogn-model* nil)
(defvar *people* (make-hash-table :test 'equal))
(defvar *capture* nil)
(defvar *lock* (bordeaux-threads:make-lock))

(defun load-models ()
  (setf *detect-model* (read-net *detect-model-path*
                                  :config *detect-proto-path*
                                  :preferable-target *use-opencl*)
        *recogn-model* (read-net *recogn-model-path*
                                 :preferable-target *use-opencl*)))

(defun detect-faces (img)
  (assert (not (null *detect-model*)))
  (bordeaux-threads:with-lock-held (*lock*)
    (let* ((img  (convert-colorspace img :color-rgba-2-bgr))
           (fit  img)
           (blob (blob-from-images (list fit)
                                   :size *detect-model-input-size*
                                   :mean *detect-model-input-mean*
                                   :swap-rb nil))
           (out  (forward *detect-model* :input blob))
           (out  (slice out '(0 0)))
           (arr  (mat-to-array out)))
      (labels ((coord (x dim)
                 (clamp (* x dim) 0 (- dim 1))))
        (loop with faces = (list)
              with rows = (rows img)
              with cols = (cols img)
              for i from 0 below (array-dimension arr 0)
              do (let ((conf   (aref arr i 2))
                       (left   (coord (aref arr i 3) cols))
                       (top    (coord (aref arr i 4) rows))
                       (right  (coord (aref arr i 5) cols))
                       (bottom (coord (aref arr i 6) rows)))
                   (when (and (< *confidence-threhsold* conf)
                              (< left right)
                              (< top bottom))
                     (push (rect left top (- right left) (- bottom top))
                           faces)))
              finally (return faces))))))

(defun face-to-vec (face)
  (let ((blob (blob-from-images (list face)
                                :scale (/ 255d0)
                                :size *recogn-model-input-size*
                                :swap-rb t)))
    (mat-clone (forward *recogn-model* :input blob))))

(defun tag-person (face name)
  (bordeaux-threads:with-lock-held (*lock*)
    (let* ((vec (face-to-vec face))
           (old (gethash name *people*))
           (new (if old
                    (mul (add old vec) 0.5d0)
                    vec)))
      (setf (gethash name *people*) new))))

(defun recognize-face (face)
  (assert (not (null *recogn-model*)))
  (bordeaux-threads:with-lock-held (*lock*)
    (loop with vec = (face-to-vec face)
          with match = "Unknown"
          with best = 0
          for name being the hash-keys of *people* using (hash-value v)
          for score = (dot-product vec v)
          do (when (and (< *match-threshold* score)
                        (< best score))
               (setf best score
                     match name))
          finally (return match))))

(defun get-person-name ()
  (let* ((cmd (concatenate 'string
                           "zenity --title \"Person Name\" "
                           "--entry --text \"Enter the person's name\""))
         (name (uiop:run-program cmd
                                 :ignore-error-status t
                                 :force-shell t
                                 :output :string))
         (name (string-strip name)))
    name))

(defun inside-of (point rect)
  (and (< (x rect) (x point))
       (< (x point) (+ (x rect) (width rect)))
       (< (y rect) (y point))
       (< (y point) (+ (y rect) (height rect)))))

(defun capture-next-frame ()
  (when (and *capture*
             (opened-p *capture*))
    (let ((frame (read-video-frame *capture*)))
      (when (not (empty-p frame))
        frame))))

(defun process-frame (frame)
  (let ((face-rects (detect-faces frame)))
    (loop for rect in face-rects
          do (let* ((face (image-roi frame rect))
                    (name (recognize-face face)))
               (draw-rectangle frame rect :color (scalar 0 255 0 255))
               (draw-text frame name :origin (point (x rect) (y rect))
                                     :color (scalar 0 255 0 255)))))
  frame)

(defun on-click (x y)
  (when-let ((frame (capture-next-frame)))
    (loop with click = (point x y)
          for rect in (detect-faces frame)
          when (inside-of click rect)
            do (progn
                 (when-let ((name (get-person-name)))
                   (tag-person (image-roi frame rect) name))
                 (return)))))

(defun start-capture (&optional input)
  (assert (null *capture*))
  (setf *capture* (if input
                      (video-capture :uri input)
                      (video-capture :device 0))))

(defun stop-capture ()
  (when *capture*
    (release *capture*)
    (setf *capture* nil)))

(defun face-detector (input)
  (load-models)
  (start-capture input)
  (unwind-protect
       (with-open-window ((win key)
                          :name "Face Detection & Recognition"
                          :delay 100
                          :on-mouse (lambda (win event x y flags)
                                      (declare (ignore win) (ignore flags))
                                      (when (eq :event-lbuttonup event)
                                        (on-click x y))))
         (when-let ((frame (capture-next-frame)))
           (let ((recognized (process-frame frame)))
             (show-image win recognized)
             (release frame)
             (trivial-garbage:gc :full (= 0 (random 10)))))
         (not (or (= key 32)
                  (= key 27))))
    (stop-capture)))

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
  (:name :detection-model
   :long "detection-model"
   :arg-parser #'identity
   :description "Path to the detection model file.")
  (:name :detection-config
   :long "detection-config"
   :arg-parser #'identity
   :description "Path to the detection model config file.")
  (:name :recognition-model
   :long "recognition-model"
   :arg-parser #'identity
   :description "Path to the recognition model file.")
  (:name :use-opencl
   :long "use-opencl"
   :arg-parser (lambda (v) (or (null v) (string= "yes" v)))
   :description "(yes/no) Use OpenCL if available.")
  (:name :confidence-threshold
   :long "confidence-threshold"
   :arg-parser #'parse-integer
   :description "Face detection confidence threshold.")
  (:name :match-threshold
   :long "match-threshold"
   :arg-parser #'parse-integer
   :description "Face recognition match threshold."))

(defconstant +about+ "Face detection and recognition example.")

(defun main (&optional args)
  (let ((options (opts:get-opts (or args (opts:argv)))))
    (if (getf options :help)
        (opts:describe :prefix +about+)
        (macrolet ((setparameter (param key)
                     (let ((%val (gensym)))
                       `(when-let (,%val (getf options ,key))
                          (setf ,param ,%val)))))
          (setparameter *detect-model-path* :detection-model)
          (setparameter *detect-proto-path* :detection-config)
          (setparameter *recogn-model-path* :recognition-model)
          (setparameter *use-opencl* :use-opencl)
          (setparameter *confidence-threhsold* :confidence-threshold)
          (setparameter *match-threshold* :match-threshold)
          (face-detector (getf options :input))))))

(eval-when (:execute)
  (main))
