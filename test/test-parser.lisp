(in-package :cl-user)
(defpackage :qly.test-parser
  (:use :cl :fiveam :qly.parser))
(in-package :qly.test-parser)

(def-suite parse-atoms)
(in-suite parse-atoms)

(defmacro qly-is (expect actual)
  `(is (equal ,expect (to-sexp ,actual))))

(test parse-integer
  (qly-is '(1) (parse-qly-text "1")))
