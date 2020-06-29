;;;; qly.asd

(defsystem :qly
  :description "Qly Programming Language Bootstrap Compiler"
  :author "Yifang Ma <yifangma93@gmail.com>, Bo Yao <icerove@gmail.com>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :trivia
               :esrap
               :parse-float
               :apply-argv)
  :components ((:file "parser")
               (:file "sem")
               (:file "translator")
               (:file "compiler")
               (:file "qly")))
