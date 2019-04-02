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
               :cl-annot
               :alexandria)
  :components ((:file "package")
               (:file "libcvc" :depends-on ("package"))
               (:file "util" :depends-on ("package"))
               (:file "enums" :depends-on ("libcvc"))
               (:file "types" :depends-on ("package"))
               (:file "core" :depends-on ("libcvc" "util" "types" "enums"))
               (:file "imgcodecs" :depends-on ("core"))
               (:file "imgproc" :depends-on ("core"))
               (:file "highgui" :depends-on ("core"))
               (:file "videoio" :depends-on ("core"))
               (:file "dnn" :depends-on ("core"))))
