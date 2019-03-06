(defsystem :see-examples
  :name :see-examples
  :version "0.0.1"
  :author "Mike Ivanov"
  :license "BSD"
  :description "SEE Examples."
  :depends-on (:see
               :unix-opts
               :parse-number)
  :components ((:file "examples")))
