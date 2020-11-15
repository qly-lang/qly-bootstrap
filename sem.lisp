(in-package :cl-user)
(defpackage :qly.sem
  (:use :cl :qly.parser)
  (:import-from :trivia :match)
  (:import-from :alexandria :if-let :when-let :hash-table-keys :hash-table-alist)
  (:export
   :semantic-error
   :unquote-out-of-quote
   :splice-out-of-quote
   :splice-out-of-array

   :qly-sem-qly-ast
   :qly-sem-scopes
   :scope-var-defs
   :analyze-type
   :resolve-var
   :make-qly-sem
   :var-def-type
   :array-type
   :make-array-type
   :array-type-elem-type
   :make-struct-type
   :make-struct-field
   :struct-type
   :struct-type-fields
   :struct-field-name
   :struct-field-type
   :make-or-type
   :or-type-variants
   :or-type
   :fun-type
   :make-fun-type
   :fun-type-params
   :fun-type-return))
(in-package :qly.sem)

;;; Semantic errors

(define-condition semantic-error (error) ())
(define-condition unquote-out-of-quote (semantic-error) ())
(define-condition splice-out-of-quote (semantic-error) ())
(define-condition splice-out-of-array (semantic-error) ())

;;; Chain of environment

(defstruct (env-chain
            (:constructor make-env-chain (&optional %parent)))
  (%env (make-hash-table :test #'equalp))
  %parent)

(defmethod print-object :around ((object env-chain) stream)
  (if *print-readably*
      (call-next-method)
      (debug-print-env-chain object stream)))

(defun debug-print-env-chain (env-chain stream)
  (pprint-logical-block (stream (hash-table-alist (env-chain-%env env-chain)))
    (pprint-exit-if-list-exhausted)
    (loop (let ((kv (pprint-pop)))
            (format stream "~a: ~a" (car kv)
                    (etypecase (cdr kv)
                      (var-def (var-def-type (cdr kv)))
                      (type-def (or (type-def-def (cdr kv)) "<builtin>"))))
            (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory stream)))))

(defun lookup/direct (name env)
  (values (gethash name (env-chain-%env env))))

(defun lookup (name env)
  (or (lookup/direct name env)
      (when-let ((parent (env-chain-%parent env)))
        (lookup name parent))))

(defun lookup-symbol-def (qly-symbol env)
  (if-let (def (lookup/direct (qly-symbol-value qly-symbol) env))
    (if (> (qly-symbol-start qly-symbol)
           (call-exp-end (var-def-mexp def)))
        def
        (when-let ((parent (env-chain-%parent env)))
          (lookup-symbol-def name parent)))
    (when-let ((parent (env-chain-%parent env)))
      (lookup-symbol-def name parent))))

(defun (setf lookup) (new-value name env)
  (setf (gethash name (env-chain-%env env)) new-value))

;;; Qly semantic unit

(defstruct (qly-sem
            (:constructor make-qly-sem (qly-ast)))
  qly-ast
  (scopes (make-hash-table :test 'equalp))
  (symbol-scopes (make-hash-table :test 'equalp)))

(defmethod print-object :around ((object qly-sem) stream)
  (if *print-readably*
      (call-next-method)
      (debug-print-qly-sem object stream)))

(defmethod debug-print-qly-sem (qly-sem stream)
  (format stream "AST:
----~%")
  (format-print (qly-sem-qly-ast qly-sem) stream)
  (format stream "~%~%SCOPES:
-------~%")
  (loop for key being the hash-keys of (qly-sem-scopes qly-sem)
          using (hash-value value)
        do (pprint-logical-block (stream nil)
             (match key
               ((call-exp :value (qly-symbol :value :|f|)
                          :args (qly-array :value args)
                          :start start)
                (match args
                  ((list* (qly-symbol :value function-name) _)
                   (format stream "function ~a at line ~a" function-name (position-to-line-col (qly-sem-qly-ast qly-sem) start)))
                  ((list* _)
                   (format stream "lambda at line ~a" (position-to-line-col (qly-sem-qly-ast qly-sem) start)))))
               (_ (princ key stream)))
             (when (scope-has-content-p value)
               (write-string ":" stream)
               (pprint-indent :block 2 stream)
               (pprint-newline :mandatory stream)
               (princ value stream))
             (pprint-indent :block 0 stream)
             (pprint-newline :mandatory stream))))

(defstruct (scope
            (:constructor %make-scope))
  mexp var-defs type-defs)

(defmethod print-object :around ((object scope) stream)
  (if *print-readably*
      (call-next-method)
      (debug-print-scope object stream)))

(defun scope-has-content-p (scope)
  (or (plusp (hash-table-count (env-chain-%env (scope-var-defs scope))))
      (plusp (hash-table-count (env-chain-%env (scope-type-defs scope))))))

(defun debug-print-scope (scope stream)
  (pprint-logical-block (stream nil)
    (when-let (var-defs (hash-table-alist (env-chain-%env (scope-var-defs scope))))
      (write-string "VAR-DEFS: " stream)
      (pprint-indent :block 2 stream)
      (pprint-newline :mandatory stream)
      (pprint-logical-block (stream var-defs)
        (pprint-exit-if-list-exhausted)
        (loop (let ((kv (pprint-pop)))
                (format stream "~a: ~a" (car kv)
                        (var-def-type (cdr kv)))
                (pprint-exit-if-list-exhausted)
                (pprint-newline :mandatory stream)))))

    (when-let (type-defs (hash-table-alist (env-chain-%env (scope-type-defs scope))))
      (pprint-indent :block 0 stream)
      (pprint-newline :mandatory stream)
      (write-string "TYPE-DEFS: " stream)
      (pprint-indent :block 2 stream)
      (pprint-newline :mandatory stream))))

(defstruct var-def mexp type)
(defstruct occur mexp)
(defstruct type-def mexp def)

(defparameter *builtin-var-defs*
  (let ((var-defs (make-env-chain)))
    (setf
     ;;; std primitives
     (lookup :|true| var-defs) (make-var-def)
     (lookup :|false| var-defs) (make-var-def)

     (lookup :|v| var-defs) (make-var-def)
     (lookup :|f| var-defs) (make-var-def)
     (lookup :|t| var-defs) (make-var-def)
     (lookup :|block| var-defs) (make-var-def)
     (lookup :|if| var-defs) (make-var-def)
     (lookup :|while| var-defs) (make-var-def)
     (lookup :|continue| var-defs) (make-var-def)
     (lookup :|break| var-defs) (make-var-def)
     (lookup :|return| var-defs) (make-var-def)
     (lookup :|set| var-defs) (make-var-def)
     (lookup :|+| var-defs) (make-var-def)
     (lookup :|-| var-defs) (make-var-def)
     (lookup :|*| var-defs) (make-var-def)
     (lookup :|/| var-defs) (make-var-def)
     (lookup :|**| var-defs) (make-var-def)
     (lookup :|is| var-defs) (make-var-def)
     (lookup :|=| var-defs) (make-var-def)
     (lookup :|!=| var-defs) (make-var-def)
     (lookup :|>| var-defs) (make-var-def)
     (lookup :|<| var-defs) (make-var-def)
     (lookup :|>=| var-defs) (make-var-def)
     (lookup :|<=| var-defs) (make-var-def)
     (lookup :|>>| var-defs) (make-var-def)
     (lookup :|<<| var-defs) (make-var-def)
     (lookup :|&| var-defs) (make-var-def)
     (lookup :\| var-defs) (make-var-def)
     (lookup :|^| var-defs) (make-var-def)
     (lookup :|and| var-defs) (make-var-def)
     (lookup :|or| var-defs) (make-var-def)
     (lookup :|not| var-defs) (make-var-def)

     (lookup :|length| var-defs) (make-var-def)
     (lookup :|slice| var-defs) (make-var-def)
     (lookup :|append| var-defs) (make-var-def)
     (lookup :|concat| var-defs) (make-var-def)
     (lookup :|del| var-defs) (make-var-def)
     (lookup :|to-string| var-defs) (make-var-def)
     (lookup :|to-symbol| var-defs) (make-var-def)

     (lookup :|to-i8| var-defs) (make-var-def)
     (lookup :|to-i16| var-defs) (make-var-def)
     (lookup :|to-i32| var-defs) (make-var-def)
     (lookup :|to-i64| var-defs) (make-var-def)
     (lookup :|to-i128| var-defs) (make-var-def)
     (lookup :|to-bignum| var-defs) (make-var-def)
     (lookup :|to-u8| var-defs) (make-var-def)
     (lookup :|to-u16| var-defs) (make-var-def)
     (lookup :|to-u32| var-defs) (make-var-def)
     (lookup :|to-u64| var-defs) (make-var-def)
     (lookup :|to-u128| var-defs) (make-var-def)
     (lookup :|to-f32| var-defs) (make-var-def)
     (lookup :|to-f64| var-defs) (make-var-def)
     (lookup :|to-decimal| var-defs) (make-var-def)

     (lookup :|shallow-copy| var-defs) (make-var-def)
     (lookup :|copy| var-defs) (make-var-def)
     (lookup :|move| var-defs) (make-var-def)
     (lookup :|weak| var-defs) (make-var-def)

     (lookup :|ffi| var-defs) (make-var-def)
     (lookup :|syscall| var-defs) (make-var-def)

     ;;; std extended
     (lookup :|for| var-defs) (make-var-def)
     (lookup :|cond| var-defs) (make-var-def)
     (lookup :|++| var-defs) (make-var-def)
     (lookup :|--| var-defs) (make-var-def))
    var-defs))

(defparameter *builtin-type-defs*
  (let ((type-defs (make-env-chain)))
    (setf (lookup :|nil| type-defs) (make-type-def)
          (lookup :|symbol| type-defs) (make-type-def)
          (lookup :|int| type-defs) (make-type-def)
          (lookup :|uint| type-defs) (make-type-def)
          (lookup :|i8| type-defs) (make-type-def)
          (lookup :|i16| type-defs) (make-type-def)
          (lookup :|i32| type-defs) (make-type-def)
          (lookup :|i64| type-defs) (make-type-def)
          (lookup :|i128| type-defs) (make-type-def)
          (lookup :|bigint| type-defs) (make-type-def)
          (lookup :|biguint| type-defs) (make-type-def)
          (lookup :|u8| type-defs) (make-type-def)
          (lookup :|u16| type-defs) (make-type-def)
          (lookup :|u32| type-defs) (make-type-def)
          (lookup :|u64| type-defs) (make-type-def)
          (lookup :|u128| type-defs) (make-type-def)
          (lookup :|string| type-defs) (make-type-def)
          (lookup :|real| type-defs) (make-type-def)
          (lookup :|f32| type-defs) (make-type-def)
          (lookup :|f64| type-defs) (make-type-def)
          (lookup :|decimal| type-defs) (make-type-def))
    type-defs))
(defvar *builtin-scope*
  (%make-scope :var-defs *builtin-var-defs*
               :type-defs *builtin-type-defs*))

(defun make-scope (&optional (parent-scope *builtin-scope*) mexp)
  (%make-scope :mexp mexp
               :var-defs (make-env-chain (scope-var-defs parent-scope))
               :type-defs (make-env-chain (scope-type-defs parent-scope))))

;;; Main semantic analysis entry

(defun sem-ast (qly-ast)
  (let ((qly-sem (make-qly-sem qly-ast)))
    ;; Passes of semantic analysis
    (analyze-type qly-sem)
    (resolve-var qly-sem)
    ;; (check-error qly-sem)
    ))

;;; Basic type building blocks

(defstruct fun-type params return)
(defstruct array-type elem-type)
(defstruct struct-type fields)
(defstruct struct-field name type)
(defstruct or-type variants)

(defmethod print-object :around ((obj fun-type) stream)
  (if *print-readably*
      (call-next-method)
      (debug-print-fun-type obj stream)))

(defun debug-print-fun-type (fun-type stream)
  (format stream "f[[~{~a~^ ~}]:~a]" (fun-type-params fun-type) (fun-type-return fun-type)))

;;; Analyze type pass, fill var-defs and type-defs in all scopes

(defun analyze-type (qly-sem)
  (let ((scope (make-scope)))
    (setf (gethash :root (qly-sem-scopes qly-sem)) scope)
    (analyze-type-mexp* (qly-ast-mexp* (qly-sem-qly-ast qly-sem))
                        (qly-sem-scopes qly-sem) scope)
    qly-sem))

(defun analyze-type-mexp* (mexp* scopes scope)
  (loop for mexp in mexp* do (analyze-type-mexp-out1 mexp scope))
  (loop for mexp in mexp* do (analyze-type-mexp-out2 mexp scope))
  (loop for mexp in mexp* do (analyze-type-mexp-in mexp scope scopes)))

;; In first pass, insert all type with empty definition in type-table
;; insert all var and fun with its type
(defun analyze-type-mexp-out1 (mexp scope)
  (match mexp
    (;; v[var : type value]
     ;; type must be defined before, value must compatible with type (check in later pass)
     (call-exp :value (qly-symbol :value :|v|) :args
               (qly-array :value
                          (list* (colon-exp :value (qly-symbol :value var)
                                            :colon type)
                                 _)))
     (setf (lookup var (scope-var-defs scope))
           (make-var-def
            :mexp mexp
            :type (process-type type scope))))
    (;; f[fname signature mexp*]
     ;; types in signature must be defined before
     (call-exp :value (qly-symbol :value :|f|)
               :args (qly-array :value (list* (qly-symbol :value fname)
                                              signature
                                              mexp*)))
     (match signature
       (;; [param1:type1 param2 param3:type3 ...]:return-type
        (colon-exp :value (qly-array :value params)
                   :colon return-type)
        (setf (lookup fname (scope-var-defs scope))
              (make-var-def
               :mexp mexp
               :type (make-fun-type :params (process-param-types params scope)
                                    :return (process-type return-type scope)))))
       (;; [param1:type1 param2 param3:type3 ...]
        (qly-array :value params)
        (setf (lookup fname (scope-var-defs scope))
              (make-var-def
               :mexp mexp
               :type (make-fun-type :params (process-param-types params scope)
                                    :return :untyped))))))
    (;; t[type typedef]
     (call-exp :value (qly-symbol :value :|t|) :args
               (qly-array :value (list (qly-symbol :value type)
                                       (mexp :value typedef))))
     (setf (lookup type (scope-type-defs scope)) :unprocessed)))
  ;; TODO: should catch malformed v[] f[] and t[] (maybe in a later pass)
  )

;; In second pass, insert all type with all type definition, for recursive typedef
(defun analyze-type-mexp-out2 (mexp scope)
  (match mexp
    (;; t[type typedef]
     (call-exp :value (qly-symbol :value :|t|) :args
               (qly-array :value (list (qly-symbol :value type)
                                       (mexp :value typedef))))
     (setf (lookup type (scope-type-defs scope)) (process-type type typedef scope)))) )

;; In third pass, go inside fun body, recursively process three passes
;; TODO: we did not go into other mexp that might have f[], such as block[... f[]]
;; We also didn't check any block[...] which should be seen as f[noname [] ...] noname[]
;; We also didn't handle lambda: f[[args]:ret body]
;; We also didn't consider quote and unquote
(defun analyze-type-mexp-in (mexp scope scopes)
  (match mexp
    (;; f[fname signature mexp*]
     (call-exp :value (qly-symbol :value :|f|) :args
               (qly-array :value (list* fname
                                        signature
                                        mexp*)))

     (if (qly-symbol-p fname)
         (let ((scope (make-scope scope mexp)))
           (setf (gethash mexp scopes) scope)
           (analyze-type-mexp* mexp* scope scopes))
         (error "function name must be a symbol")))))

(defun process-param-types (params scope)
  (mapcar (lambda (param)
            (match param
              ((colon-exp :value param
                          :colon type)
               (process-type type scope))
              ((mexp :value param)
               :untyped)))
          params))

(defun process-type (type scope)
  (match type
    ((qly-symbol :value symbol)
     (if (lookup-type symbol scope)
         symbol
         (error "Cannot find type ~a" symbol)))
    ((qly-array :value value)
     (print type)
     (print value)
     (cond
       ((every 'colon-exp-p value) (make-struct-type :fields (process-field-types value scope)))
       (t (match value
            ((list elem-type) (make-array-type :elem-type (process-type elem-type scope)))
            (_ (error "Pattern in [] need to be either a type or a list of field:type"))))))
    ((call-exp :value (qly-symbol :value :|array|)
               :args (qly-array :value (list elem-type)))
     (make-array-type :elem-type (process-type elem-type scope)))
    ((call-exp :value (qly-symbol :value :|struct|) :args (qly-array :value field-types))
     (make-struct-type :fields (process-field-types field-types scope)))
    ((call-exp :value (qly-symbol :value :|or|) :args (qly-array :value variants))
     (make-or-type :variants (mapcar (lambda (variant) (process-type variant scope))
                                     variants)))
    ((call-exp :value (qly-symbol :value :|f|)
               :args (qly-array :value (list (colon-exp :value (qly-array :value types) :colon return-type))))
     (make-fun-type :params (mapcar (lambda (type)
                                      (process-type type scope))
                                    types)
                    :return (process-type return-type scope)))
    (_ (error "Unknown pattern of type"))))

(defun lookup-type (symbol scope)
  (lookup symbol (scope-type-defs scope)))

(defun process-field-types (fields scope)
  (mapcar (lambda (field)
            (match field
              ((colon-exp :value (qly-symbol :value field-name) :colon field-type)
               (make-struct-field :name field-name :type (process-type field-type scope)))
              (_ (error "Field must be symbol:type"))))
          fields))

;;; Resolve var pass, resolve every var to its definition

(defun resolve-var (qly-sem)
  (loop for mexp in (qly-ast-sem* (qly-sem-qly-ast qly-sem))
        do (resolve-var-mexp mexp (qly-sem-scopes qly-sem) (gethash :root (qly-sem-scopes qly-sem)) 0)))

(defun resolve-var-mexp (mexp scopes scope quote)
  (match mexp
    ((dot-exp :value value :prop prop)
     (resolve-var-mexp value scopes scope quote)
     (resolve-var-mexp prop scopes scope quote))
    ((quote-exp :value value)
     (resolve-var-mexp value scopes scope (1+ quote)))
    ((unquote-exp :value value)
     (resolve-var-mexp value scopes scope (1- quote)))
    ((splice-exp :value value)
     (resolve-var-mexp value scopes scope (1- quote)))
    ((call-exp)
     (resolve-var-call-exp mexp scopes scope quote))
    ((qly-symbol)
     (resolve-var-qly-symbol mexp scopes scope quote))
    ((qly-array :value mexp*)
     (loop for mexp in mexp*
           do (resolve-var-mexp mexp scopes scope quote)))))

(defun resolve-var-qly-symbol (qly-symbol scopes scope quote)
  (cond
    ((plusp quote))
    ((minusp quote) (error "Comma not inside a quote"))
    (t
     (if-let (def (lookup-symbol-def qly-symbol (scope-var-defs scope)))
       (setf (lookup qly-symbol scopes) scope
             (lookup qly-symbol (scope-var-occurs scope)) def)
       (error "Cannot resolve var")))))

(defun resolve-var-call-exp (call-exp scopes scope quote)
  (when-let (fdef (resolve-var-qly-symbol (call-exp-value call-exp) scopes scope quote))
    (cond
      ((builtin-fdef-p fdef)
       (resolve-var-builtin-fdef call-exp scopes scope quote))
      (t
       (loop for mexp in (qly-array-value (call-exp-args call-exp))
             do (resolve-var-mexp mexp scopes scope quote))))))

(defun resolve-var-builtin-fdef (call-exp scopes scope quote)
  (match call-exp
    ((call-exp :value (qly-symbol :value :|f|)
               :args (qly-array :value (list* (qly-symbol :value fname) _ mexp*)))
     (loop for mexp in mexp*
           do (resolve-var-mexp mexp scopes (gethash fname scopes) quote)))
    ((call-exp :value (qly-symbol :value :|v|)
               :args (qly-array :value
                                (list _ value)))
     (resolve-var-mexp mexp scopes scope quote))
    ;; TODO: more builtin special ops and fs

    ))


;;; Check error pass, check type error and other semantic erros

(defun check-error (qly-sem))
