;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)
(annot:enable-annot-syntax)

@export
(defclass video-capture (proxy) ())

(defun video-capture-new ()
  (make-instance 'video-capture
                 :peer (cv-call-out cv-video-capture-new :pointer)
                 :release (lambda (x) (cv-call cv-video-capture-free x))))

@export
(defun video-capture (&key uri device (api-preference :cap-any))
  (assert (xor uri device))
  (let* ((cap (video-capture-new))
         (api (cffi:foreign-enum-value 'cv-video-capture-apis api-preference)))
    (if uri
        (cv-call cv-video-capture-open-uri (peer cap) uri api)
        (cv-call cv-video-capture-open-device (peer cap) device api))
    cap))

(defmethod opened-p ((video-capture video-capture))
  (cv-call-out cv-video-capture-is-opened
               (peer video-capture)
               :bool))

@export
(defun retrieve-video-frame (cap &key image (flag 0))
  (let* ((mat     (or image (mat)))
         (success (cv-call-out cv-video-capture-retrieve
                               (peer cap)
                               (peer mat)
                               flag
                               :bool)))
      (values mat success)))

@export
(defun read-video-frame (cap &key image)
  (let* ((mat     (or image (mat)))
         (success (cv-call-out cv-video-capture-read
                               (peer cap)
                               (peer mat)
                               :bool)))
    (values mat success)))

@export
(defun grab-video-frame (cap)
  (cv-call-out cv-video-capture-grab (peer cap) :bool))
