;; This file is a part of the SEE project.
;; It is subject to the license terms in the LICENSE file found
;; in the top-level directory of this distribution.

(defsystem :see-examples
  :name :see-examples
  :version "0.0.1"
  :author "Mike Ivanov"
  :license "MIT"
  :description "SEE Examples."
  :depends-on (:see
               :unix-opts
               :parse-number
               :cl-csv
               :alexandria)
  :components ((:file "examples")))
