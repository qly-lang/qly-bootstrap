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
   :scope-type-defs
   :analyze-type
   :resolve-var
   :make-qly-sem
   :var-def-type
   :var-def-mexp
   :type-def-def
   :type-def-mexp
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
   :fun-type-return
   :lookup))
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
    (if (or (null (var-def-mexp def)) ; pos null is builtin
            (> (mexp-start qly-symbol)
               (mexp-end (var-def-mexp def))))
        def
        (when-let (parent (env-chain-%parent env))
          (lookup-symbol-def qly-symbol parent)))
    (when-let (parent (env-chain-%parent env))
      (lookup-symbol-def qly-symbol parent))))

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
                (pprint-newline :mandatory stream))))
      (pprint-indent :block 0 stream)
      (pprint-newline :mandatory stream))

    (when-let (type-defs (hash-table-alist (env-chain-%env (scope-type-defs scope))))
      (write-string "TYPE-DEFS: " stream)
      (pprint-indent :block 2 stream)
      (pprint-newline :mandatory stream)
      (pprint-logical-block (stream type-defs)
        (loop (let ((kv (pprint-pop)))
                (format stream "~a: ~a" (car kv)
                        (type-def-def (cdr kv)))
                (pprint-exit-if-list-exhausted)
                (pprint-newline :mandatory stream)))))))

(defstruct var-def mexp type)
(defstruct occur mexp)
(defstruct type-def mexp def)

;;; Basic type building blocks

(defstruct fun-type params return)
(defstruct range-type start end)
(defstruct array-type elem-type)
(defstruct struct-type fields)
(defstruct struct-field name type)
(defstruct or-type variants)
(defstruct op-type params return)

(defmethod print-object :around ((obj fun-type) stream)
  (if *print-readably*
      (call-next-method)
      (debug-print-fun-type obj stream)))

(defun debug-print-fun-type (fun-type stream)
  (format stream "f[[~{~a~^ ~}]:~a]" (fun-type-params fun-type) (fun-type-return fun-type)))

(defparameter *builtin-var-defs*
  (let ((var-defs (make-env-chain)))
    (setf
     ;;; std primitives
     (lookup :|true| var-defs) (make-var-def)
     (lookup :|false| var-defs) (make-var-def)

     (lookup :|v| var-defs) (make-var-def :type (make-op-type :return :|symbol|))
     (lookup :|f| var-defs) (make-var-def :type (make-op-type :return :|symbol|))
     (lookup :|t| var-defs) (make-var-def :type (make-op-type :return :|symbol|))
     (lookup :|block| var-defs) (make-var-def)
     (lookup :|if| var-defs) (make-var-def)
     (lookup :|while| var-defs) (make-var-def)
     (lookup :|continue| var-defs) (make-var-def)
     (lookup :|break| var-defs) (make-var-def)
     (lookup :|return| var-defs) (make-var-def)
     (lookup :|set| var-defs) (make-var-def)
     (lookup :|+| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|number|))
     (lookup :|-| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|number|))
     (lookup :|*| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|number|))
     (lookup :|/| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|number|))
     (lookup :|**| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|number|))
     (lookup :|is| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|any|)) :return :|bool|))
     (lookup :|=| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|any|)) :return :|bool|))
     (lookup :|!=| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|any|)) :return :|bool|))
     (lookup :|>| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|bool|))
     (lookup :|<| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|bool|))
     (lookup :|>=| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|bool|))
     (lookup :|<=| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|bool|))
     (lookup :|>>| var-defs) (make-var-def :type (make-fun-type :params (list :|fixnum| :|fixnum| :return :|fixnum|)))
     (lookup :|<<| var-defs) (make-var-def :type (make-fun-type :params (list :|fixnum| :|fixnum|) :return :|fixnum|))
     (lookup :|&| var-defs) (make-var-def :type (make-fun-type :params (list :|fixnum| :|fixnum|) :return :|fixnum|))
     (lookup :\| var-defs) (make-var-def :type (make-fun-type :params (list :|fixnum| :|fixnum|) :return :|fixnum|))
     (lookup :|!| var-defs) (make-var-def :type (make-fun-type :params (list :|fixnum|) :return :|fixnum|))
     (lookup :|^| var-defs) (make-var-def :type (make-fun-type :params (list :|fixnum| :|fixnum|) :return :|fixnum|))
     (lookup :|and| var-defs) (make-var-def :type (make-op-type :params (list (make-array-type :elem-type :|bool|)) :return :|bool|))
     (lookup :|or| var-defs) (make-var-def :type (make-op-type :params (list (make-array-type :elem-type :|bool|)) :return :|bool|))
     (lookup :|not| var-defs) (make-var-def :type (make-fun-type :params (list :|bool|) :return :|bool|))

     (lookup :|length| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|any|) :return :|uint|)))
     (lookup :|slice| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|any|) :return (make-array-type :elem-type :|any|))))
     (lookup :|append| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|any|) :|any|) :return (make-array-type :elem-type :|any|)))
     (lookup :|concat| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|any|) (make-array-type :elem-type :|any|)) :return (make-array-type :elem-type :|any|)))
     (lookup :|del| var-defs) (make-var-def :type (make-fun-type :params (list (make-array-type :elem-type :|any|) :|uint|)))
     (lookup :|to| var-defs) (make-var-def :type (make-fun-type :params (list :|any| :|mexp|) :return :|any|))

     (lookup :|shallow-copy| var-defs) (make-var-def :type (make-fun-type :params (list :|any|) :return :|any|))
     (lookup :|copy| var-defs) (make-var-def :type (make-fun-type :params (list :|any|) :return :|any|))
     (lookup :|r| var-defs) (make-var-def :type (make-fun-type :params (list :|any|) :return :|any|))
     (lookup :|ffi| var-defs) (make-var-def)
     (lookup :|syscall| var-defs) (make-var-def)

     ;;; std extended
     (lookup :|for| var-defs) (make-var-def)
     (lookup :|cond| var-defs) (make-var-def)
     (lookup :|++| var-defs) (make-var-def :type (make-fun-type :params (list :|number|) :return :|number|))
     (lookup :|--| var-defs) (make-var-def :type (make-fun-type :params (list :|number|) :return :|number|)))
    var-defs))

(defparameter *builtin-type-defs*
  (let ((type-defs (make-env-chain)))
    (setf (lookup :|nil| type-defs) (make-type-def)
          (lookup :|any| type-defs) (make-type-def)
          (lookup :|symbol| type-defs) (make-type-def)
          (lookup :|int| type-defs) (make-type-def
                                     :def (make-or-type
                                           :variants
                                           (list :|fixint| :|bigint|)))
          (lookup :|uint| type-defs) (make-type-def
                                      :def (make-or-type
                                            :variants
                                            (list :|fixuint| :|biguint|)))
          (lookup :|int8| type-defs) (make-type-def)
          (lookup :|int16| type-defs) (make-type-def)
          (lookup :|int32| type-defs) (make-type-def)
          (lookup :|int64| type-defs) (make-type-def)
          (lookup :|int128| type-defs) (make-type-def)
          (lookup :|fixint| type-defs) (make-type-def
                                        :def (make-or-type
                                              :variants
                                              (list :|int8| :|int16| :|int32| :|int128|)))
          (lookup :|bigint| type-defs) (make-type-def)
          (lookup :|biguint| type-defs) (make-type-def)
          (lookup :|uint8| type-defs) (make-type-def)
          (lookup :|uint16| type-defs) (make-type-def)
          (lookup :|uint32| type-defs) (make-type-def)
          (lookup :|uint64| type-defs) (make-type-def)
          (lookup :|uint128| type-defs) (make-type-def)
          (lookup :|fixuint| type-defs) (make-type-def
                                         :def (make-or-type
                                               :variants
                                               (list :|uint8| :|uint16| :|uint32| :|uint64| :|uint128|)))
          (lookup :|fixnum| type-defs) (make-type-def
                                        :def (make-or-type
                                              :variants
                                              (list :|fixint| :|fixuint|)))
          (lookup :|char| type-defs) (make-type-def
                                      :def (make-or-type
                                            :variants (list :|uint8| :|uint16| :|uint32|)))
          (lookup :|ascii-char| type-defs) (make-type-def :def (make-range-type :start 0 :end 128))
          (lookup :|char| type-defs) (make-type-def :def (make-or-type
                                                          :variants
                                                          (list :|ascii-char|
                                                                (make-range-type :start 128 :end 2048)
                                                                (make-range-type :start 2048 :end 65536)
                                                                (make-range-type :start 65536 :end 1114112))))
          (lookup :|string| type-defs) (make-type-def :def (make-array-type :elem-type :|char|))
          (lookup :|real| type-defs) (make-type-def
                                      :def (make-or-type
                                            :variants
                                            (list :|f32| :|f64| :|decimal|)))
          (lookup :|f32| type-defs) (make-type-def)
          (lookup :|f64| type-defs) (make-type-def)
          (lookup :|decimal| type-defs) (make-type-def)
          (lookup :|number| type-defs) (make-or-type
                                        :variants
                                        (list :|int| :|uint| :|real|))
          (lookup :|bool| type-defs) (make-type-def)
          (lookup :|mexp| type-defs) (make-type-def))
    type-defs))
(defparameter *builtin-scope*
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

;;; Analyze type pass, fill var-defs and type-defs in all scopes

(defun analyze-type (qly-sem)
  (let ((scope (make-scope)))
    (setf (gethash :root (qly-sem-scopes qly-sem)) scope)
    (analyze-type-mexp* (qly-ast-mexp* (qly-sem-qly-ast qly-sem))
                        scope (qly-sem-scopes qly-sem))
    qly-sem))

(defun analyze-type-mexp* (mexp* scope scopes)
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
                                       typedef)))
     (setf (lookup type (scope-type-defs scope))
           (make-type-def :mexp mexp
                          :def (process-type typedef scope))))) )

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
           (match signature
             (;; [param1:type1 param2 param3:type3 ...]:return-type
              (colon-exp :value (qly-array :value params))
              (process-param-vars params scope))
             (;; [param1:type1 param2 param3:type3 ...]
              (qly-array :value params)
              (process-param-vars params scope))
             (_ (error "function signature must be either [param:type] array or [param:type]:return-type")))
           (analyze-type-mexp* mexp* scope scopes))
         (error "function name must be a symbol")))))

(defun process-param-vars (params scope)
  (loop for param in params do
    (match param
      ((colon-exp :value (qly-symbol :value name)
                  :colon type)
       (setf (lookup name (scope-var-defs scope))
             (make-var-def :mexp param
                           :type (process-type type scope))))
      ((qly-symbol :value name)
       (setf (lookup name (scope-var-defs scope))
             (make-var-def :mexp param
                           :type :untyped)))
      (_ (error "function param must be either var or var:type")))))

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
  (loop for mexp in (qly-ast-mexp* (qly-sem-qly-ast qly-sem))
        do (resolve-var-mexp mexp
                             (qly-sem-symbol-scopes qly-sem)
                             (qly-sem-scopes qly-sem)
                             :root
                             0)))

(defun resolve-var-mexp (mexp symbol-scopes scopes scope quote)
  (match mexp
    ((qly-symbol)
     (resolve-var-qly-symbol mexp symbol-scopes scopes scope quote))

    ;; ((dot-exp :value value :prop prop)
    ;;  (resolve-var-mexp value scopes scope quote)
    ;;  (resolve-var-mexp prop scopes scope quote))
    ;; ((quote-exp :value value)
    ;;  (resolve-var-mexp value scopes scope (1+ quote)))
    ;; ((unquote-exp :value value)
    ;;  (resolve-var-mexp value scopes scope (1- quote)))
    ;; ((splice-exp :value value)
    ;;  (resolve-var-mexp value scopes scope (1- quote)))
    ;; ((call-exp)
    ;;  (resolve-var-call-exp mexp scopes scope quote))
    ;; ((qly-array :value mexp*)
    ;;  (loop for mexp in mexp*
    ;;        do (resolve-var-mexp mexp scopes scope quote)))
    ))

(defun resolve-var-qly-symbol (qly-symbol symbol-scopes scopes scope quote)
  (cond
    ((plusp quote))
    ((minusp quote) (error "Comma not inside a quote"))
    (t
     (if-let (def (lookup-symbol-def qly-symbol (scope-var-defs (gethash scope scopes))))
       (progn (setf (gethash qly-symbol symbol-scopes) scope
                    ;; TODO: also add qly-symbol to def's occurs
                    ))
       (error "Cannot resolve var ~a" qly-symbol)))))

(defun resolve-var-call-exp (call-exp scopes scope quote)
  (when-let (fdef (resolve-var-qly-symbol (call-exp-value call-exp) scopes scope quote))
    (cond
      ((builtin-op-p fdef)
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
