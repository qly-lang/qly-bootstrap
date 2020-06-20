;;;; qly.lisp
(in-package :cl-user)
(defpackage :qly
  (:use :cl)
  (:import-from :qly.compiler :compile-qly-file)
  (:import-from :apply-argv :get-argv :parse-argv)
  (:export :main))
(in-package :qly)

(defun main ()
  (compile-qly-file (car (car (parse-argv (get-argv))))))
