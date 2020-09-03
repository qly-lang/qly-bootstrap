(in-package :cl-user)
(defpackage :qly.sem
  (:use :cl :qly.parser)
  (:import-from :trivia :match)
  (:inport-from :alexandria :if-let :when-let))
(in-package :qly.sem)

;;; Chain of environment

(defstruct (env-chain
            (:constructor make-env-chain (&optional %parent)))
  (%env (make-hash-table :test #'equal))
  %parent)

(defun lookup/direct (name env)
  (values (gethash name (env-chain-%env env))))

(defun lookup (name env)
  (or (lookup/direct name env)
      (when-let ((parent (env-chain-%parent env)))
        (lookup name parent))))

(defun lookup-symbol-def (qly-symbol env)
  (if-let (def (lookup/direct (qly-symbol-value qly-symbol)))
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

(defstruct qly-sem qly-ast scopes)

(defmethod print-object :around ((object qly-sem) stream)
  (if *print-readably*
      (call-next-method)
      (debug-print-qly-sem object stream)))

(defmethod debug-print-qly-sem (qly-sem stream)
  (format stream "AST:~%")
  (debug-print (qly-sem-qly-ast qly-sem) stream)
  (format stream "~%~%SCOPES:~%")
  (loop for key being the hash-keys of (qly-sem-scopes qly-sem)
          using (hash-value value)
        do (format stream "~S: ~S~%" key value)))

(defstruct (scope
            (:constructor %make-scope))
  mexp var-defs var-occurs type-defs type-occurs)

(defstruct var-def mexp type)
(defstruct occur mexp)
(defstruct type-def mexp def)

(defvar *builtin-var-defs*
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
(defvar *builtin-type-defs*
  (let ((type-defs (make-env-chain)))
    (setf (lookup :|symbol| type-defs) (make-type-def)
          (lookup :|integer| type-defs) (make-type-def)
          (lookup :|unsigned| type-defs) (make-type-def)
          (lookup :|i8| type-defs) (make-type-def)
          (lookup :|i16| type-defs) (make-type-def)
          (lookup :|i32| type-defs) (make-type-def)
          (lookup :|i64| type-defs) (make-type-def)
          (lookup :|i128| type-defs) (make-type-def)
          (lookup :|bigint| type-defs) (make-type-def)
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
               :var-occurs  (make-hash-table :test #'equal)
               :type-defs (make-env-chain (scope-type-defs parent-scope))
               :type-occurs (make-hash-table :test #'equal)))

;;; Main semantic analysis entry

(defun sem-ast (qly-ast)
  (let ((qly-sem (make-qly-sem :qly-ast qly-ast
                               :scopes (make-hash-table :test #'equal))))
    ;; Passes of semantic analysis
    (analyze-type qly-sem)
    (resolve-var qly-sem)
    ;; (check-error qly-sem)
    ))

;;; Basic type building blocks

(defstruct funtype params return)
(defstruct array-type elem-type)
(defstruct record-type fields)
(defstruct record-field name type)
(defstruct or-type variants)

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
                          (list (colon-exp :value (qly-symbol :value var)
                                           :colon type)
                                (mexp :value value))))
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
               :type (make-funtype :params (process-param-types params scope)
                                   :return (process-type return-type scope)))))
       (;; [param1:type1 param2 param3:type3 ...]

        (qly-array :value params)
        (setf (lookup fname (scope-var-defs scope))
              (make-var-def
               :mexp mexp
               :type (make-funtype :params (process-param-types params scope)
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

     (when (qly-symbol-p fname)
       (let ((scope (make-scope scope mexp)))
         (setf (gethash fname scopes) scope)
         (setf (gethash mexp scopes) scope)
         (analyze-type-mexp* mexp* scope scopes))))))

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
         (error "Cannot find type")))
    ((call-exp :value (qly-symbol :value :|array|) :args (list elem-type))
     (make-array-type :elem-type (process-type elem-type scope)))
    ((call-exp :value (qly-symbol :value :|record|) :args field-types)
     (make-record-type :fields (process-field-types field-types scope)))
    ((call-exp :value (qly-symbol :value :|or|) :args variants)
     (make-or-type :variants (mapcar (lambda (variant) (process-type variant scope))
                                     variants)))
    (_ (error "Unknown pattern of type"))))

(defun lookup-type (symbol scope)
  (lookup symbol (scope-type-defs scope)))

(defun process-field-types (fields scope)
  (mapcar (lambda (field)
            (match colon
              ((colon-exp :value field-name :colon field-type)
               (make-record-field :name field-name :type (process-type field-type scope)))))
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
    ;; Maybe detect in parser?
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
             do (resolve-var-mexp scopes scope quote))))))

(defun resolve-var-builtin-fdef (call-exp scopes scope quote)
  (match call-exp
    ((call-exp :value (qly-symbol :value :|f|)
               :args (qly-array :value (list* (qly-symbol :value fname) _ mexp*)))
     (loop for mexp in mexp*
           do (resolve-var-mexp mexp scopes (gethash fname scopes) quote)))
    ((call-exp :value (qly-symbol :value :|v|)
               (qly-array :value
                          (list _ value)))
     (resolve-var-mexp mexp scopes scope quote))
    ;; TODO: more builtin special ops and fs

    ))


;;; Check error pass, check type error and other semantic erros

(defun check-error (qly-sem))
