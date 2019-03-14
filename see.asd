;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(defsystem :see
  :name :see
  :version "0.0.1"
  :author "Mike Ivanov"
  :license "MIT"
  :description "A binding of the OpenCV library."
  :depends-on (:cffi
               :trivial-features
               :trivial-main-thread
               :trivial-garbage
               :trivial-backtrace
               :cl-annot)
  :components ((:file "package")
               (:file "libcvc" :depends-on ("package"))
               (:file "util")
               (:file "types")
               (:file "core" :depends-on ("libcvc" "util" "types"))
               (:file "imgcodecs" :depends-on ("core"))
               (:file "imgproc" :depends-on ("core"))
               (:file "highgui" :depends-on ("core"))
               (:file "videoio" :depends-on ("core"))
               (:file "dnn" :depends-on ("core"))))

