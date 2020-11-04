(defsystem :qly-test
  :description "Qly Bootstrap Compiler Tests"
  :author "Yifang Ma <yifangma93@gmail.com>, Bo Yao <icerove@gmail.com>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :fiveam
               :qly)
  :components ((:module "test"
                :serial t
                :components
                ((:file "test-parser")))))
