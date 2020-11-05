(in-package :cl-user)
(defpackage :qly.test-parser
  (:use :cl :fiveam :qly.parser))
(in-package :qly.test-parser)

(def-suite parse-atoms)
(in-suite parse-atoms)

(defmacro qly-is (expect actual)
  `(is (equal ,expect (to-sexp ,actual))))

(test parse-integer
  (qly-is '(1) (parse-qly-text "1"))
  (qly-is '(0) (parse-qly-text "0"))
  (qly-is '(-1) (parse-qly-text "-1"))
  (qly-is '(15) (parse-qly-text "+15"))
  (qly-is `(,(expt 2 129)) (parse-qly-text (princ-to-string (expt 2 129))))

  (defun integer-type-test (int type)
    (let* ((i (princ-to-string int))
           (i-parse (car (qly-ast-mexp* (parse-qly-text i)))))
      (is (= int (qly-integer-value i-parse)))
      (is (eql type (qly-integer-type i-parse)))))

  (integer-type-test 0 :i32)
  (integer-type-test (1- (expt 2 31)) :i32)
  (integer-type-test (- (expt 2 31)) :i32)

  (integer-type-test (expt 2 31) :i64)
  (integer-type-test (1- (- (expt 2 31))) :i64)
  (integer-type-test (1- (expt 2 63)) :i64)
  (integer-type-test (- (expt 2 63)) :i64)

  (integer-type-test (expt 2 63) :i128)
  (integer-type-test (1- (- (expt 2 63))) :i128)
  (integer-type-test (1- (expt 2 127)) :i128)
  (integer-type-test (- (expt 2 127)) :i128)

  (integer-type-test (expt 2 127) :bigint)
  (integer-type-test (1- (- (expt 2 127))) :bigint))

(test parse-unsigned
  (qly-is '(0) (parse-qly-text "0x0"))
  (qly-is '(0) (parse-qly-text "0o0"))
  (qly-is '(0) (parse-qly-text "0b0"))
  (qly-is '(255) (parse-qly-text "0xff"))
  (qly-is '(255) (parse-qly-text "0o377"))
  (qly-is '(255) (parse-qly-text "0b11111111"))

  (defun unsigned-type-test (uint base type)
    (let* ((*print-base* base)
           (u (esrap:text "0"
                          (ecase base
                            (2 "b")
                            (8 "o")
                            (16 "x"))
                          (princ-to-string uint)))
           (u-parse (car (qly-ast-mexp* (parse-qly-text u)))))
      (is (= uint (qly-unsigned-value u-parse)))
      (is (eql base (qly-unsigned-base u-parse)))
      (is (eql type (qly-unsigned-type u-parse)))))

  (unsigned-type-test 0 16 :u32)
  (unsigned-type-test 0 8 :u32)
  (unsigned-type-test 0 2 :u32)
  (unsigned-type-test (1- (expt 2 32)) 16 :u32)
  (unsigned-type-test (expt 2 32) 16 :u64)
  (unsigned-type-test (1- (expt 2 64)) 8 :u64)
  (unsigned-type-test (expt 2 64) 8 :u128)
  (unsigned-type-test (1- (expt 2 128)) 2 :u128)
  (unsigned-type-test (expt 2 128) 2 :bigunsigned))

(test parse-real
  (qly-is '(0.0) (parse-qly-text "0.0"))
  (qly-is '(0.0) (parse-qly-text ".0"))
  (qly-is '(0.0) (parse-qly-text "0."))
  (qly-is '(1.5) (parse-qly-text "1.5"))
  (qly-is '(-1.5) (parse-qly-text "-1.5"))
  (qly-is '(1.5) (parse-qly-text "+1.5"))
  (qly-is '(1.5) (parse-qly-text "1.5e0"))
  (qly-is '(1.5) (parse-qly-text "1.5E0"))
  (qly-is '(1.5) (parse-qly-text "0.15e+1"))
  (qly-is '(1.5) (parse-qly-text "150.0e-2"))
  (qly-is '(1.5) (parse-qly-text "150e-2")))
