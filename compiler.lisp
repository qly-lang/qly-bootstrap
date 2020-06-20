(in-package :cl-user)
(defpackage :qly.compiler
  (:use :cl)
  (:import-from
   :qly.parser

   :parse-qly-file))
(in-package :qly.compiler)

(defun compile-qly-file (filepath)
  (compile-qly-exprs
   (parse-qly-file filepath)))

(defun complie-qly-exprs (exprs))
