(in-package :cl-user)
(defpackage :qly.parser
  (:use :cl :esrap)
  (:import-from :alexandria
   :if-let :read-file-into-string
   :when-let)
  (:import-from :trivia
   :match)
  (:import-from :parse-float
   :parse-float)
  (:export
   :parse-qly-file
   :parse-qly-text

   :qly-ast :qly-ast-path :qly-ast-mexp* :qly-ast-text
   :mexp-start :mexp-end :mexp-value :mexp-p
   :dot-exp-prop :dot-exp-p :dot-exp-value
   :colon-exp-colon :colon-exp-p :colon-exp-value
   :call-exp-args :call-exp-p :call-exp-value
   :quote-exp-p :quote-exp-value
   :unquote-exp-p :unquote-exp-value
   :splice-exp-p :splice-exp-value
   :qly-array-p :qly-array-value
   :qly-atom-p :qly-atom-value
   :qly-string-p :qly-string-value
   :qly-real-p :qly-real-value :qly-real-type
   :qly-unsigned-p :qly-unsigned-value :qly-unsigned-base :qly-unsigned-type
   :qly-integer-p :qly-integer-value :qly-integer-type
   :qly-symbol-p :qly-symbol-value

   :to-sexp
   :format-print))
(in-package :qly.parser)

(defun parse-qly-file (filepath)
  (let ((qly-ast (parse-qly-text (read-file-into-string filepath))))
    (setf (qly-ast-path qly-ast) filepath)
    qly-ast))

(defun parse-qly-text (text)
  (make-qly-ast :mexp* (parse 'mexp* text)
                :text text))

(defstruct qly-ast path mexp* text)
(defstruct mexp start end value)

;;; Possible mexp:
(defstruct (dot-exp (:include mexp)) prop)
(defstruct (colon-exp (:include mexp)) colon)
(defstruct (call-exp (:include mexp)) args)
(defstruct (quote-exp (:include mexp)))
(defstruct (unquote-exp (:include mexp)))
(defstruct (splice-exp (:include mexp)))
(defstruct (qly-array (:include mexp)))
(defstruct (qly-atom (:include mexp)))

;;; Possible atom:
(defstruct (qly-string (:include qly-atom)))
(defstruct (qly-real (:include qly-atom)) type)
(defstruct (qly-unsigned (:include qly-atom)) type base)
(defstruct (qly-integer (:include qly-atom)) type)
(defstruct (qly-symbol (:include qly-atom)))

;;; Utility functions

(defun not-doublequote-p (char)
  (not (eql #\" char)))

(defun whitespace-char-p (char)
  (member char '(#\space #\tab #\newline)))

(defun rule-char-p (char)
  (member char '(#\. #\[ #\: #\] #\" #\\ #\' #\, #\@ #\# )))

(defun raw-symbol-char-p (char)
  (and (not (whitespace-char-p char)) (not (rule-char-p char))))

(defun bin-char-p (char)
  (digit-char-p char 2))

(defun oct-char-p (char)
  (digit-char-p char 8))

(defun hex-char-p (char)
  (digit-char-p char 16))

;;; Utility rules

(defrule escape-char (and #\\ character)
  (:function second))

(defrule string-char (or escape-char not-doublequote))

(defrule symbol-char (or escape-char raw-symbol-char))

(defrule digit-char (digit-char-p character))

(defrule not-doublequote (not-doublequote-p character))

(defrule rule-char (rule-char-p character))

(defrule raw-symbol-char (raw-symbol-char-p character))

(defrule bin-char (bin-char-p character))

(defrule oct-char (oct-char-p character))

(defrule hex-char (hex-char-p character))

(defrule whitespace-char (whitespace-char-p character))

(defrule eof (! character))

(defrule qly-comment (and "#" (* (not #\newline)) (or #\newline eof))
  (:constant nil))

(defrule whitespace (+ (or whitespace-char qly-comment))
  (:constant nil))

(defrule mexp-whitespace (and mexp (? whitespace))
  (:function first))

(defrule mexp* (and (? whitespace) (* mexp-whitespace))
  (:function second))

;;; Atoms

(defrule qly-atom (or qly-string qly-real qly-unsigned qly-integer qly-symbol))

(defrule qly-string (and #\" (* string-char) #\")
  (:destructure (q1 string q2 &bounds start end)
                (declare (ignore q1 q2))
                (make-qly-string :start start
                                 :end end
                                 :value (text string))))

(defrule qly-integer (and (? (or "+" "-")) (+ digit-char)
                          (! (or symbol-char ".")))
  (:lambda (list)
    (parse-integer (text list) :radix 10))
  (:lambda (integer)
    (cond ((<= (- (expt 2 31)) integer (1- (expt 2 31))) `(:i32 ,integer))
          ((<= (- (expt 2 63)) integer (1- (expt 2 63))) `(:i64 ,integer))
          ((<= (- (expt 2 127)) integer (1- (expt 2 127))) `(:i128 ,integer))
          (t `(:bigint ,integer))))
  (:lambda (typed-integer &bounds start end)
    (make-qly-integer :start start
                      :end end
                      :value (second typed-integer)
                      :type (first typed-integer))))

(defrule qly-real
    (and (? (or "+" "-"))
         (or (and (+ digit-char) "." (* digit-char)
                  (? (and (or "e" "E") (? (or "+" "-")) (+ digit-char))))
             (and (* digit-char) "." (+ digit-char)
                  (? (and (or "e" "E") (? (or "+" "-")) (+ digit-char))))
             (and (+ digit-char)
                  (or "e" "E") (? (or "+" "-")) (+ digit-char)))
         (! symbol-char))
  (:lambda (list &bounds start end)
    (make-qly-real :start start
                   :end end
                   :value (parse-float (text list))
                   :type :f64)))

(defrule qly-unsigned (and (or (and "0x" (+ hex-char))
                               (and "0o" (+ oct-char))
                               (and "0b" (+ bin-char)))
                           (! symbol-char))
  (:lambda (list)
    (let ((radix (case (aref (caar list) 1)
                   (#\x 16)
                   (#\o 8)
                   (#\b 2))))
      (list (parse-integer (text (cdar list)) :radix radix) radix)))
  (:destructure (integer radix)
    (cond ((< integer (expt 2 32)) `(:u32 ,integer ,radix))
          ((< integer (expt 2 64)) `(:u64 ,integer ,radix))
          ((< integer (expt 2 128)) `(:u128 ,integer ,radix))
          (t `(:bigunsigned ,integer ,radix))))
  (:destructure (type value base &bounds start end)
    (make-qly-unsigned :start start
                       :end end
                       :value value
                       :type type
                       :base base)))

(defrule qly-symbol (+ symbol-char)
  (:lambda (list &bounds start end)
    (make-qly-symbol :start start
                     :end end
                     :value (intern (text list) :keyword))))

;;; Expressions

(defrule mexp colon-exp-and-higher)

(defrule colon-exp-and-higher
    (or colon-exp
        call-dot-exp-and-higher))

(defrule call-dot-exp-and-higher
    (or call-or-dot-exp
        primary-exp))

(defrule primary-exp
    (or quote-exp unquote-exp splice-exp
        qly-array qly-atom))

(defrule call-or-dot-exp (or call-exp dot-exp))

(defrule mexp-except-colon (or call-or-dot-exp quote-exp unquote-exp splice-exp qly-array qly-atom))

(defrule mexp-except-colon-dot-call (or quote-exp unquote-exp splice-exp
                                        qly-array qly-atom))

(defrule dot-exp (and call-dot-exp-and-higher (? whitespace) "." (? whitespace)
                      primary-exp)
  (:destructure (value w d w prop &bounds start end)
    (make-dot-exp :start start
                  :end end
                  :value value
                  :prop prop)))

(defrule call-exp (and call-dot-exp-and-higher qly-array)
  (:destructure (value args &bounds start end)
    (make-call-exp :start start
                   :end end
                   :value value
                   :args args)))

(defrule colon-exp (and call-dot-exp-and-higher (? whitespace) ":" (? whitespace)
                        colon-exp-and-higher)
  (:destructure (value w c w colon &bounds start end)
    (make-colon-exp :start start
                    :end end
                    :value value
                    :colon colon)))

(defrule quote-exp (and "'" (? whitespace) mexp)
  (:function third)
  (:lambda (exp &bounds start end)
    (make-quote-exp :start start
                    :end end
                    :value exp)))

(defrule unquote-exp (and "," (? whitespace) mexp)
  (:function third)
  (:lambda (exp &bounds start end)
    (make-unquote-exp :start start
                      :end end
                      :value exp)))

(defrule splice-exp (and "@" (? whitespace) mexp)
  (:function third)
  (:lambda (exp &bounds start end)
    (make-splice-exp :start start
                     :end end
                     :value exp)))

(defrule qly-array (and "[" mexp* "]")
  (:destructure (p1 mexp* p2 &bounds start end)
    (declare (ignore p1 p2))
    (make-qly-array :start start
                    :end end
                    :value mexp*)))

;;; Debug util

(defgeneric to-sexp (qly-obj))

(defmethod to-sexp ((obj qly-ast))
  (mapcar 'to-sexp (qly-ast-mexp* obj)))

(defmethod to-sexp ((obj dot-exp))
  (list :\. (to-sexp (dot-exp-value obj))
        (to-sexp (dot-exp-prop obj))))

(defmethod to-sexp ((obj colon-exp))
  (list :\: (to-sexp (colon-exp-value obj))
        (to-sexp (colon-exp-colon obj))))

(defmethod to-sexp ((obj call-exp))
  (cons (to-sexp (call-exp-value obj))
        (mapcar 'to-sexp (qly-array-value (call-exp-args obj)))))

(defmethod to-sexp ((obj quote-exp))
  (list :\' (to-sexp (quote-exp-value obj))))

(defmethod to-sexp ((obj unquote-exp))
  (list :\, (to-sexp (unquote-exp-value obj))))

(defmethod to-sexp ((obj splice-exp))
  (list :@ (to-sexp (splice-exp-value obj))))

(defmethod to-sexp ((obj qly-array))
  (cons :array (mapcar 'to-sexp (qly-array-value obj))))

(defmethod to-sexp ((obj qly-atom))
  (qly-atom-value obj))

(defun format-print (qly-ast stream)
  (format-print-mexp* (qly-ast-text qly-ast) (qly-ast-mexp* qly-ast) stream 0 (list -1) 0))

(defun format-print-mexp* (text mexp* stream last-end last-bracket-col col)
  (when mexp*
    (multiple-value-setq (last-end last-bracket-col col)
      (format-print-mexp text (car mexp*) stream last-end last-bracket-col col))
    (when (cdr mexp*)
      (princ " " stream)
      (incf col)
      (multiple-value-setq (last-end last-bracket-col col)
        (format-print-mexp* text (cdr mexp*) stream last-end last-bracket-col col))))
  (values last-end last-bracket-col col))

(defun format-print-mexp (text mexp stream last-end last-bracket-col col)
  (when-let (last-newline (search (string #\newline) text :start2 last-end :end2 (mexp-start mexp)))
    (setf last-end (1+ last-newline))
    (terpri stream)
    (loop for i to (car last-bracket-col) do (princ " " stream))
    (setf col (1+ (car last-bracket-col))))
  (typecase mexp
    (dot-exp
     (princ ".[" stream)
     (incf col 2)
     (push (1- col) last-bracket-col)
     (multiple-value-setq (last-end last-bracket-col col)
       (format-print-mexp* text (list (dot-exp-value mexp) (dot-exp-prop mexp)) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col))
    (call-exp
     (multiple-value-setq (last-end last-bracket-col col)
       (format-print-mexp text (call-exp-value mexp) stream last-end last-bracket-col col))
     (multiple-value-setq (last-end last-bracket-col col)
       (format-print-mexp text (call-exp-args mexp) stream last-end last-bracket-col col)))
    (qly-array
     (princ "[" stream)
     (push col last-bracket-col)
     (incf col)
     (multiple-value-setq (last-end last-bracket-col col)
       (format-print-mexp* text (qly-array-value mexp) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col))
    (quote-exp
     (princ "'[" stream)
     (incf col 2)
     (push (1- col) last-bracket-col)
     (multiple-value-setq (last-end last-bracket-col col)
       (format-print-mexp text (quote-exp-value mexp) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col))
    (unquote-exp
     (princ ",[" stream)
     (incf col 2)
     (push (1- col) last-bracket-col)
     (multiple-value-setq (last-end last-bracket-col col)
       (format-print-mexp text (unquote-exp-value mexp) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col))
    (splice-exp
     (princ "@[" stream)
     (incf col 2)
     (push (1- col) last-bracket-col)
     (multiple-value-setq (last-end last-bracket-col col)
       (format-print-mexp text (splice-exp-value mexp) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col))
    (colon-exp
     (princ ":[" stream)
     (incf col 2)
     (push (1- col) last-bracket-col)
     (multiple-value-setq (last-end last-bracket-col col)
       (format-print-mexp* text (list (colon-exp-value mexp) (colon-exp-colon mexp)) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col))
    (qly-symbol
     (princ (qly-symbol-value mexp) stream)
     (incf col (length (string (qly-symbol-value mexp)))))
    (qly-string
     (prin1 (qly-string-value mexp) stream)
     (incf col (+ 2 (length (qly-string-value mexp)))))
    (qly-real
     (princ (mexp-value mexp) stream)
     (incf col (length (princ-to-string (mexp-value mexp)))))
    (qly-unsigned
     (princ (mexp-value mexp) stream)
     (incf col (length (princ-to-string (mexp-value mexp)))))
    (qly-integer
     (princ (mexp-value mexp) stream)
     (incf col (length (princ-to-string (mexp-value mexp))))))
  (values last-end last-bracket-col col))
