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
   :qly-unsigned-p :qly-unsigned-value :qly-unsigned-base :qly-unsigned-base
   :qly-integer-p :qly-integer-value :qly-integer-type
   :qly-symbol-p :qly-symbol-value

   :debug-print))
(in-package :qly.parser)

(defun parse-qly-file (filepath)
  (let ((qly-ast (parse-qly-text (read-file-into-string filepath))))
    (setf (qly-ast-path qly-ast) filepath)
    qly-ast))

(defun parse-qly-text (text)
  (make-qly-ast :mexp* (parse 'mexp* text)
                :text text))

;;; AST nodes

(defstruct mexp start end value)

(defstruct qly-ast path mexp* text)

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

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun whitespace-char (char)
  (member char '(#\space #\tab #\newline)))

(defun rule-char (char)
  (member char '(#\. #\[ #\: #\] #\" #\\ #\' #\, #\@ #\# )))

(defun raw-symbol-char (char)
  (and (not (whitespace-char char)) (not (rule-char char))))

(defun bin-char (char)
  (digit-char-p char 2))

(defun oct-char (char)
  (digit-char-p char 8))

(defun hex-char (char)
  (digit-char-p char 16))

;;; Utility rules

(defrule whitespace (+ (whitespace-char character))
  (:constant nil))

(defrule string-char (or (and #\\ character) (not-doublequote character)))

(defrule symbol-char (or (and #\\ character) (raw-symbol-char character)))

(defrule digit-char (digit-char-p character))

(defrule eof (! character))

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
         (or (and (+ (digit-char-p character)) "." (* (digit-char-p character))
                  (? (and (or "e" "E") (? (or "+" "-")) (+ (digit-char-p character)))))
             (and (* (digit-char-p character)) "." (+ (digit-char-p character))
                  (? (and (or "e" "E") (? (or "+" "-")) (+ (digit-char-p character)))))
             (and (+ (digit-char-p character))
                  (or "e" "E") (? (or "+" "-")) (+ (digit-char-p character))))
         (! symbol-char))
  (:lambda (list &bounds start end)
    (make-qly-real :start start
                   :end end
                   :value (parse-float (text list))
                   :type :f64)))

(defrule qly-unsigned (and (or (and "0x" (+ (hex-char character)))
                               (and "0o" (+ (oct-char character)))
                               (and "0b" (+ (bin-char character))))
                           (! symbol-char))
  (:lambda (list)
    (let ((radix (case (aref (first list) 1)
                   (#\x 16)
                   (#\o 8)
                   (#\b 2))))
      (list (parse-integer (text list) :radix radix) radix)))
  (:destructure (integer radix)
    (cond ((< integer (expt 2 32)) `(:u32 ,integer ,radix))
          ((< integer (expt 2 64)) `(:u64 ,integer ,radix))
          ((< integer (expt 2 128)) `(:u128 ,integer ,radix))
          (t `(:bigint ,integer ,radix))))
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

(defrule mexp (or dot-exp colon-exp call-exp
                  quote-exp unquote-exp splice-exp
                  qly-array qly-atom))

(defrule dot-exp (and mexp (? whitespace) "." (? whitespace)
                      (or call-exp quote-exp unquote-exp splice-exp
                          qly-array qly-atom))
  (:destructure (value w d w prop &bounds start end)
    (make-dot-exp :start start
                  :end end
                  :value value
                  :prop prop)))

(defrule colon-exp (and mexp (? whitespace) ":" (? whitespace)
                        (or call-exp quote-exp unquote-exp splice-exp qly-array qly-atom))
  (:destructure (value w c w colon &bounds start end)
    (make-colon-exp :start start
                    :end end
                    :value value
                    :colon colon)))

(defrule call-exp (and mexp qly-array)
  (:destructure (value args &bounds start end)
    (make-call-exp :start start
                   :end end
                   :value value
                   :args args)))

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

(defrule qly-comment (and "#" (* (not #\newline)) #\newline)
  (:constant nil))

(defrule qly-array (and "[" mexp* "]")
  (:destructure (p1 mexp* p2 &bounds start end)
    (declare (ignore p1 p2))
    (make-qly-array :start start
                    :end end
                    :value mexp*)))

(defrule mexp-or-comment (and (? whitespace) (or mexp qly-comment))
  (:function second))

(defrule mexp* (and (* mexp-or-comment) (? whitespace))
  (:function car))

;;; Debug print

(defmethod print-object :around ((object mexp) stream)
  (if *print-readably*
      (call-next-method)
      (princ (mexp-value object) stream)))

(defmethod print-object :around ((object qly-unsigned) stream)
  (if *print-readably*
      (call-next-method)
      (let ((*print-base* (qly-unsigned-base object)))
        (princ (qly-unsigned-value object) stream))))

(defmethod print-object :around ((object qly-array) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "#~a" (qly-array-value object))))

(defmethod print-object :around ((object quote-exp) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "(' ~a)" (quote-exp-value object))))

(defmethod print-object :around ((object splice-exp) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "(@ ~a)" (splice-exp-value object))))

(defmethod print-object :around ((object unquote-exp) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "(, ~a)" (unquote-exp-value object))))

(defmethod print-object :around ((object dot-exp) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "(. ~a ~a)" (dot-exp-value object) (dot-exp-prop object))))

(defmethod print-object :around ((object call-exp) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "(~a ~{~a~^ ~})" (call-exp-value object) (qly-array-value (call-exp-args object)))))

(defmethod print-object :around ((object colon-exp) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "(: ~a ~a)" (colon-exp-value object) (colon-exp-colon object))))

(defmethod print-object :around ((object qly-ast) stream)
  (if *print-readably*
      (call-next-method)
      (debug-print object stream)))

(defun debug-print (qly-ast stream)
  (debug-print-mexp* (qly-ast-text qly-ast) (qly-ast-mexp* qly-ast) stream 0 (list -1) 0))

(defun debug-print-mexp* (text mexp* stream last-end last-bracket-col col)
  (when mexp*
    (multiple-value-setq (last-end last-bracket-col col)
      (debug-print-mexp text (car mexp*) stream last-end last-bracket-col col))
    (when (cdr mexp*)
      (princ " " stream)
      (incf col)
      (multiple-value-setq (last-end last-bracket-col col)
        (debug-print-mexp* text (cdr mexp*) stream last-end last-bracket-col col))))
  (values last-end last-bracket-col col))

(defun debug-print-mexp (text mexp stream last-end last-bracket-col col)
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
       (debug-print-mexp* text (list (dot-exp-value mexp) (dot-exp-prop mexp)) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col)
     )
    (call-exp
     (multiple-value-setq (last-end last-bracket-col col)
       (debug-print-mexp text (call-exp-value mexp) stream last-end last-bracket-col col))
     (multiple-value-setq (last-end last-bracket-col col)
       (debug-print-mexp text (call-exp-args mexp) stream last-end last-bracket-col col)))
    (qly-array
     (princ "[" stream)
     (push col last-bracket-col)
     (incf col)
     (multiple-value-setq (last-end last-bracket-col col)
       (debug-print-mexp* text (qly-array-value mexp) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col))
    (quote-exp
     (princ "'[" stream)
     (incf col 2)
     (push (1- col) last-bracket-col)
     (multiple-value-setq (last-end last-bracket-col col)
       (debug-print-mexp text (quote-exp-value mexp) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col))
    (unquote-exp
     (princ ",[" stream)
     (incf col 2)
     (push (1- col) last-bracket-col)
     (multiple-value-setq (last-end last-bracket-col col)
       (debug-print-mexp text (unquote-exp-value mexp) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col))
    (splice-exp
     (princ "@[" stream)
     (incf col 2)
     (push (1- col) last-bracket-col)
     (multiple-value-setq (last-end last-bracket-col col)
       (debug-print-mexp text (splice-exp-value mexp) stream last-end last-bracket-col col))
     (princ "]" stream)
     (incf col)
     (pop last-bracket-col))
    (colon-exp
     (princ ":[" stream)
     (incf col 2)
     (push (1- col) last-bracket-col)
     (multiple-value-setq (last-end last-bracket-col col)
       (debug-print-mexp* text (list (colon-exp-value mexp) (colon-exp-colon mexp)) stream last-end last-bracket-col col))
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
