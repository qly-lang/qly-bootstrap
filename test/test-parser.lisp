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

(test parse-string
  (qly-is '("a") (parse-qly-text "\"a\""))
  (qly-is '("ab") (parse-qly-text "\"ab\""))
  (qly-is '("a
b") (parse-qly-text "\"a
b\""))
  (qly-is '("a\"") (parse-qly-text (string '|"a\\""|)))
  (qly-is '("an") (parse-qly-text (string '|"a\\n"|)))
  (qly-is '("") (parse-qly-text (string '|""|)))
  (signals esrap:esrap-parse-error (parse-qly-text (string '|aa"|)))
  (signals esrap:esrap-parse-error (parse-qly-text (string '|"aaa|))))

(test parse-atom
  (qly-is '(:|a|) (parse-qly-text "a"))
  (qly-is '(:|1a|) (parse-qly-text "1a"))
  ;; These (dot with number literal) operations are not recommended as they are hard
  ;; to reason. But at least they should not error
  (qly-is '((:\. 1 :|3a|)) (parse-qly-text "1 . 3a"))
  (qly-is '((:\. :|1| :|3a|)) (parse-qly-text "1.3a"))
  (qly-is '(:|1e2e3|) (parse-qly-text "1e2e3"))
  (qly-is '(:|15e-2x|) (parse-qly-text "15e-2x"))
  (qly-is '((:|.| :|1| :|5e-2x|)) (parse-qly-text "1.5e-2x"))

  (qly-is '(:-) (parse-qly-text "-"))
  (qly-is '(:1) (parse-qly-text "\\1"))
  (qly-is '(:1.5\e2) (parse-qly-text "\\1\\.5e2"))
  (qly-is '(:|a| :|bbb|) (parse-qly-text "a bbb"))
  (qly-is '(:|a bbb|) (parse-qly-text "a\\ bbb"))
  (qly-is '(:|a
b|) (parse-qly-text "a\\
b"))
  (qly-is '(:|a#b|) (parse-qly-text "a\\#b"))
  (qly-is '(:|,.'|) (parse-qly-text "\\,\\.\\'")))

(def-suite parse-mexp)
(in-suite parse-mexp)

(test parse-comment
  (qly-is '() (parse-qly-text "#"))
  (qly-is '() (parse-qly-text "#aaaa"))
  (qly-is '(3) (parse-qly-text "#aaaa
3"))
  (qly-is '(3) (parse-qly-text "3 #aaaa"))
  (qly-is '(3 4) (parse-qly-text "3#aaaa
4"))
  (qly-is '("aaa#aaa") (parse-qly-text "\"aaa#aaa\""))
  (qly-is '("aaa") (parse-qly-text "\"aaa\"#bbb")))

(test parse-array
  (qly-is '((:array)) (parse-qly-text "[]"))
  (qly-is '((:array :|a| 1)) (parse-qly-text "[ a 1]"))
  (qly-is '((:array :|a| 1)) (parse-qly-text "[ a 1 ]"))
  (qly-is '((:array :|a| 1)) (parse-qly-text "[a 1 ]"))
  (qly-is '((:array :|a| 1)) (parse-qly-text "[a 1]"))
  (qly-is '((:array (:array :|a|) 1)) (parse-qly-text "[[a] 1]")))

(test parse-quote-unquote-splice
  (qly-is '((:\' :|aaa|)) (parse-qly-text "'aaa"))
  (qly-is '((:\' (:\' :|aaa|))) (parse-qly-text "''aaa"))
  (signals esrap:esrap-parse-error (parse-qly-text "'"))
  (qly-is '((:\' (:\, :|aaa|))) (parse-qly-text "',aaa"))
  (qly-is '((:\' (:@ (:array :|aaa|)))) (parse-qly-text "'@[aaa]")))

(test parse-dot-exp
  (qly-is '((:\. :|aaa| :|bbb|)) (parse-qly-text "aaa.bbb"))
  (qly-is '((:\. :|aaa| :|bbb|)) (parse-qly-text "aaa. bbb"))
  (qly-is '((:\. :|aaa| :|bbb|)) (parse-qly-text "aaa .bbb"))
  (qly-is '((:\. :|aaa| :|bbb|)) (parse-qly-text "aaa . bbb"))
  (qly-is '((:\. (:\. :|aaa| :|bbb|) :|ccc|)) (parse-qly-text "aaa.bbb.ccc"))
  (qly-is '((:\. ((:\. :|a| :|b|)) :|c|)) (parse-qly-text "a.b[].c"))
  (qly-is '((:\. ((:\. :|aaa| :|bbb|) :|x| (:\. :|y| :|ddd|)) :|ccc|)) (parse-qly-text "aaa.bbb[x y.ddd].ccc"))
  (qly-is '(((:|.| (((:|.| :|a| :|b|) :|d| :|e|) :|x|) :|f|))) (parse-qly-text "a.b[d e][x].f[]"))
  (qly-is '((:\. :|aaa| :|b|) :|ccc|) (parse-qly-text "aaa.b ccc")))

(test parse-call-exp
  (qly-is '((:|a| :|b| :|c| 3 "str")) (parse-qly-text "a[b c 3 \"str\"]"))
  (qly-is '((:|a|)) (parse-qly-text "a[]"))
  (qly-is '(((:|a| :|b| :|c|) :|d|)) (parse-qly-text "a[b c][d]")))

(test parse-colon-exp
  (qly-is '((:|:| :|a| :|b|)) (parse-qly-text "a:b"))
  (qly-is '((:|:| :|a| (:|:| :|b| :|c|))) (parse-qly-text "a:b:c"))
  (qly-is '((:|:| (:\. :|a| :|b|) :|c|)) (parse-qly-text "a.b:c"))
  (qly-is '((:|:| :|a| (:\. :|b| :|c|))) (parse-qly-text "a:b.c"))
  (qly-is '((:|:| :|a| (:|b|))) (parse-qly-text "a:b[]"))
  (qly-is '((:|:| (:|a|) :|b|)) (parse-qly-text "a[]:b"))
  (qly-is '((:|f|
             (:|:| (:|foo| (:|:| :|a| :|int|) (:|:| :|b| (:|array| :|string|)))
              :|type-a|)))
          (parse-qly-text "f[foo[a:int b:array[string]]:type-a]")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\" 'let-over-lambda:|#"-reader|))

(test parse-mexps
  (qly-is
   '((:|a|)
     (:|b|)
     (:\. :|c| :|d|))

   (parse-qly-text
    #"a[]
b[]
c.d"#)))
