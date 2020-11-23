(in-package :cl-user)
(defpackage :qly.util
  (:use :cl :alexandria)
  (:export
   :list1p))
(in-package :qly.util)

(defun list1p (list)
  "Return t if length of list i s 1"
  (and list (not (cdr list))))
