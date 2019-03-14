;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(in-package #:see)
(annot:enable-annot-syntax)

@export
(defclass video-capture (proxy) ())

(defun video-capture-new ()
  (make-instance 'video-capture
                 :peer (cv-video-capture-new)
                 :release #'cv-video-capture-free))

@export
(defun video-capture (&key uri device (api-preference :cap-any))
  (assert (xor uri device))
  (let* ((cap (video-capture-new))
         (api (cffi:foreign-enum-value 'cv-video-capture-apis api-preference))
         (res (if uri
                  (cv-video-capture-open-uri (peer cap) uri api)
                  (cv-video-capture-open-device (peer cap) device api))))
    (check-cv-error #'null res)
    cap))

(defmethod opened-p ((video-capture video-capture))
  (cv-video-capture-is-opened (peer video-capture)))

@export
(defun retrieve-video-frame (cap &key image (flag 0))
  (let* ((mat (or image (mat)))
         (res (cv-video-capture-retrieve (peer cap) (peer mat) flag)))
    ;; No error handling b/c an empty image (and res=false) is a valid outcome.
    (values mat res)))

@export
(defun read-video-frame (cap &key image)
  (let* ((mat (or image (mat)))
         (res (cv-video-capture-read (peer cap) (peer mat))))
    ;; No error handling b/c an empty image (and res=false) is a valid outcome.
    (values mat res)))

@export
(defun grab-video-frame (cap)
  (cv-video-capture-grab (peer cap)))
