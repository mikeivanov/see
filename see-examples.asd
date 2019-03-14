(defsystem :see-examples
  :name :see-examples
  :version "0.0.1"
  :author "Mike Ivanov"
  :license "BSD"
  :description "SEE Examples."
  :depends-on (:see
               :unix-opts
               :parse-number
               :cl-csv)
  :components ((:file "examples")))
