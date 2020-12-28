(in-package :cl-user)
(defpackage :qly.sem
  (:use :cl :qly.parser :qly.util)
  (:import-from :trivia :match)
  (:import-from :alexandria :if-let :when-let :hash-table-keys :hash-table-alist :make-keyword)
  (:export
   :semantic-error
   :unquote-out-of-quote
   :splice-out-of-quote
   :splice-out-of-array
   :undefined-var
   :undefined-type

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
   :type-def-children
   :array-type
   :make-array-type
   :array-type-elem-type
   :make-struct-type
   :make-struct-field
   :struct-type
   :struct-type-fields
   :struct-field-name
   :struct-field-type
   :fun-type
   :make-fun-type
   :fun-type-params
   :fun-type-return
   :lookup

   :expand-type))
(in-package :qly.sem)

;;; Semantic errors

(define-condition semantic-error (error) ())
(define-condition unquote-out-of-quote (semantic-error) ())
(define-condition splice-out-of-quote (semantic-error) ())
(define-condition splice-out-of-array (semantic-error) ())
(define-condition undefined-var (semantic-error)
  ((var :initarg var :reader var)))
(define-condition type-incompatible (semantic-error)
  ((expression :initarg expression :reader expression)
   (expected-type :initarg expected-type :reader expected-type)))
(define-condition undefined-type (semantic-error)
  ((type-name :initarg type-name :reader type-name)))

;;; Chain of environment

(defstruct (env-chain
            (:constructor make-env-chain (&optional %parent)))
  (%env (make-hash-table :test #'equalp))
  %parent)

(defun lookup/direct (name env)
  (gethash name (env-chain-%env env)))

(defun lookup (name env)
  (or (lookup/direct name env)
      (when-let ((parent (env-chain-%parent env)))
        (lookup name parent))))

(defun (setf lookup) (new-value name env)
  (setf (gethash name (env-chain-%env env)) new-value))

(defun lookup-var (qly-symbol scope)
  (or (lookup-var/direct qly-symbol)
      (when-let (parent (scope-parent scope))
        (lookup-var qly-symbol parent))))

(defun lookup-var/direct (qly-symbol scope)
  (if-let (def (lookup/direct (qly-symbol-value qly-symbol) (scope-var-defs scope)))
    (when (or (null (var-def-mexp def)) ; pos null is builtin
              (> (mexp-start qly-symbol)
                 (mexp-end (var-def-mexp def))))
      def)))

(defun (setf lookup-var) (var-def name scope)
  (setf (lookup name (scope-var-defs scope)) var-def))

(defun lookup-type/direct (qly-symbol scope)
  (if-let (def (lookup/direct (qly-symbol-value qly-symbol) (scope-type-defs scope)))
    (when (or (null (type-def-mexp def)) ; pos null is builtin
              (> (mexp-start qly-symbol)
                 (mexp-end (type-def-mexp def))))
      def)))

(defun lookup-type (qly-symbol scope)
  (or (lookup-type/direct qly-symbol scope)
      (when-let (parent (scope-parent scope))
        (lookup-type qly-symbol parent))))

(defun (setf lookup-type) (type-def name scope)
  (setf (lookup name (scope-type-defs scope)) type-def))

;;; Qly semantic unit

(defstruct (qly-sem
            (:constructor make-qly-sem (qly-ast)))
  qly-ast
  (scopes (make-hash-table :test 'equalp))
  (symbol-scopes (make-hash-table :test 'equalp)))

(defstruct (scope
            (:constructor %make-scope))
  mexp parent var-defs type-defs)

(defun scope-has-content-p (scope)
  (or (plusp (hash-table-count (env-chain-%env (scope-var-defs scope))))
      (plusp (hash-table-count (env-chain-%env (scope-type-defs scope))))))

(defstruct var-def name mexp type occurs type-expanded scope)
(defstruct type-def
  name mexp def expanded parents children scope)

;;; Basic type building blocks

(defstruct fun-type params return)
(defstruct range-type start end)
(defstruct array-type elem-type)
(defstruct struct-type fields)
(defstruct struct-field name type)
(defstruct op-type params return)
(defstruct exact-type value)

(defun set-super-type (child parent)
  (pushnew parent (type-def-parents child))
  (pushnew child (type-def-children parent)))

(defvar *builtin-scope* (%make-scope))

(defun create-builtin-types (type-defs types)
  (loop for type in types do
    (let ((type-name (make-keyword (string-downcase (string type)))))
      (setf (lookup type-name type-defs) (make-type-def :name type-name :scope *builtin-scope*)))))

(defun set-builtin-supertypes (type-defs child-parent-pairs)
  (loop for child-parent-pair in child-parent-pairs do
    (set-super-type (lookup (make-keyword (string-downcase (string (car child-parent-pair)))) type-defs)
                    (lookup (make-keyword (string-downcase (string (cadr child-parent-pair)))) type-defs))))

(defun create-builtin-vars (var-defs vars)
  (loop for var in vars do
    (setf (lookup (make-keyword (string-downcase (string (car var)))) var-defs) (make-var-def :type (cdr var) :scope *builtin-scope*))))

(defun create-builtin-scope ()
  (let ((var-defs (make-env-chain))
        (type-defs (make-env-chain)))
    (create-builtin-types
     type-defs
     (list :nil :any :symbol :int :uint :int8 :int16 :int32 :int64 :int128 :fixint :bigint :biguint
                :uint8 :uint16 :uint32 :uint64 :uint128 :fixuint :fixnum :real :float32 :float64
                :decimal :number :bool :mexp :char))
    (setf
     (lookup :|ascii-char| type-defs) (make-type-def :name :|ascii-char| :def (make-range-type :start 0 :end 127) :scope *builtin-scope*)
     (lookup :|extend-char| type-defs) (make-type-def :name :|char| :def (make-range-type :start 128 :end 1114111) :scope *builtin-scope*)
     (lookup :|string| type-defs) (make-type-def :name :|string| :def (make-array-type :elem-type :|char|) :scope *builtin-scope*))

    (set-builtin-supertypes
     type-defs
     '((:fixint :int) (:bigint :int) (:fixuint :uint) (:biguint :uint) (:int8 :fixint) (:int16 :fixint) (:int32 :fixint) (:int64 :fixint)
       (:int128 :fixint) (:uint16 :fixuint) (:uint32 :fixuint) (:uint64 :fixuint) (:uint128 :fixuint) (:fixint :fixnum) (:fixuint :fixnum)
       (:ascii-char :char) (:extend-char :char) (:float32 :real) (:float64 :real) (:decimal :real) (:int :number) (:uint :number)
       (:fixnum :number) (:real :number)))

    (create-builtin-vars
     var-defs
     `((:true . ,(lookup :|bool| type-defs))
       (:false . ,(lookup :|bool| type-defs))
       (:v . ,(make-var-def :type (make-op-type :return :|symbol|)))
       (:f . ,(make-var-def :type (make-op-type :return :|symbol|)))
       (:t . ,(make-var-def :type (make-op-type :return :|symbol|)))
       (:block . ,(make-op-type))
       (:if . ,(make-op-type))
       (:while . ,(make-op-type))
       (:continue . ,(make-op-type))
       (:break . ,(make-op-type))
       (:return . ,(make-op-type))
       (:set . ,(make-op-type))
       (:+ . ,(make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|number|))
       (:- . ,(make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|number|))
       (:* . ,(make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|number|))
       (:/ . ,(make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|number|))
       (:** . ,(make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|number|))
       (:is . ,(make-fun-type :params (list (make-array-type :elem-type :|any|)) :return :|bool|))
       (:= . ,(make-fun-type :params (list (make-array-type :elem-type :|any|)) :return :|bool|))
       (:!= . ,(make-fun-type :params (list (make-array-type :elem-type :|any|)) :return :|bool|))
       (:> . ,(make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|bool|))
       (:< . ,(make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|bool|))
       (:>= . ,(make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|bool|))
       (:<= . ,(make-fun-type :params (list (make-array-type :elem-type :|number|)) :return :|bool|))
       (:>> . ,(make-fun-type :params (list :|fixnum| :|fixnum| :return :|fixnum|)))
       (:<< . ,(make-fun-type :params (list :|fixnum| :|fixnum| :return :|fixnum|)))
       (:& . ,(make-fun-type :params (list :|fixnum| :|fixnum|) :return :|fixnum|))
       (:\| . ,(make-fun-type :params (list :|fixnum| :|fixnum|) :return :|fixnum|))
       (:! . ,(make-fun-type :params (list :|fixnum|) :return :|fixnum|))
       (:^ . ,(make-fun-type :params (list :|fixnum| :|fixnum|) :return :|fixnum|))
       (:and . ,(make-op-type :params (list (make-array-type :elem-type :|bool|)) :return :|bool|))
       (:or . ,(make-op-type :params (list (make-array-type :elem-type :|bool|)) :return :|bool|))
       (:not . ,(make-fun-type :params (list :|bool|) :return :|bool|))
       (:length . ,(make-fun-type :params (list (make-array-type :elem-type :|any|) :return :|uint|)))
       (:slice . ,(make-fun-type :params (list (make-array-type :elem-type :|any|) :return (make-array-type :elem-type :|any|))))
       (:append . ,(make-fun-type :params (list (make-array-type :elem-type :|any|) :|any|) :return (make-array-type :elem-type :|any|)))
       (:concat . ,(make-fun-type :params (list (make-array-type :elem-type :|any|) (make-array-type :elem-type :|any|)) :return (make-array-type :elem-type :|any|)))
       (:del . ,(make-fun-type :params (list (make-array-type :elem-type :|any|) :|uint|)))
       (:to . ,(make-fun-type :params (list :|any| :|mexp|) :return :|any|))
       (:shallow-copy . ,(make-fun-type :params (list :|any|) :return :|any|))
       (:copy . ,(make-fun-type :params (list :|any|) :return :|any|))
       (:r . ,(make-fun-type :params (list :|any|) :return :|any|))
       (:ffi . ,(make-fun-type))
       (:syscall . ,(make-fun-type))
     ;;; std extended
       (:for . ,(make-op-type))
       (:cond . ,(make-op-type))
       (:++ . ,(make-fun-type :params (list :|number|) :return :|number|))
       (:-- . ,(make-fun-type :params (list :|number|) :return :|number|))))

    (setf (scope-var-defs *builtin-scope*) var-defs
          (scope-type-defs *builtin-scope*) type-defs)))

(create-builtin-scope)

(defun make-scope (&optional (parent-scope *builtin-scope*) mexp)
  (%make-scope :mexp mexp
               :parent parent-scope
               :var-defs (make-env-chain (scope-var-defs parent-scope))
               :type-defs (make-env-chain (scope-type-defs parent-scope))))

;;; Main semantic analysis entry

(defun sem-ast (qly-ast)
  (let ((qly-sem (make-qly-sem qly-ast)))
    ;; Passes of semantic analysis
    (analyze-type qly-sem)
    (resolve-var qly-sem)))

;;; Analyze type pass, fill var-defs and type-defs in all scopes

(defun analyze-type (qly-sem)
  (let ((scope (make-scope)))
    (setf (gethash :root (qly-sem-scopes qly-sem)) scope)
    (analyze-type-mexp* (qly-ast-mexp* (qly-sem-qly-ast qly-sem))
                        scope (qly-sem-scopes qly-sem))
    qly-sem))

(defun analyze-type-mexp* (mexp* scope scopes)
  (loop for mexp in mexp* do (analyze-type-mexp-out mexp scope))
  (loop for mexp in mexp* do (analyze-type-mexp-in mexp scope scopes)))

;; In first pass, don't go into function definition but only process current level of v, t and f
(defun analyze-type-mexp-out (mexp scope)
  (match mexp
    (;; v[var : type value]
     ;; type must be defined before, value must compatible with type (check in later pass)
     (call-exp :value (qly-symbol :value :|v|) :args
               (qly-array :value
                          (list* (colon-exp :value (qly-symbol :value var)
                                            :colon type)
                                 _)))
     (let ((type (process-type type scope)))
       (setf (lookup-var var scope)
             (make-var-def
              :name var
              :mexp mexp
              :type type
              :type-expanded (expand-type type scope)
              :scope scope))))
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
        (let ((type (make-fun-type :params (process-param-types params scope)
                                   :return (process-type return-type scope))))
          (setf (lookup-var fname scope)
                (make-var-def
                 :name fname
                 :mexp mexp
                 :type type
                 :type-expanded (expand-type type scope)
                 :scope scope))))
       (;; [param1:type1 param2 param3:type3 ...]
        (qly-array :value params)
        (let ((type (make-fun-type :params (process-param-types params scope)
                                   :return :untyped)))
          (setf (lookup-var fname scope)
                (make-var-def
                 :name fname
                 :mexp mexp
                 :type type
                 :type-expanded (expand-type type scope)
                 :scope scope))))))
    (;; t[type]
     ;; t[type:supertype]
     ;; t[type typedef]
     ;; t[type:supertype typedef]
     (call-exp :value (qly-symbol :value :|t|) :args
               (qly-array :value args))
     (match args
       (;; t[type] or t[type typedef]
        (list* (qly-symbol :value type) maybe-typedef)
        (unless (or (null maybe-typedef) (list1p maybe-typedef))
          (error "expect t[type] or t[type typedef]"))
        (when (lookup-type/direct type scope)
          (error "type ~a is already defined in this scope" type))
        (match maybe-typedef
          (nil
           (let ((typedef (make-type-def :name type :mexp mexp :scope scope)))
             (set-super-type typedef *any-type*)
             (setf (lookup-type type scope)
                   typedef)))
          ((list typedef)
           (let ((typedef (make-type-def
                           :name type
                           :mexp mexp
                           :def (process-type typedef scope)
                           :scope scope)))
             (set-super-type typedef *any-type*)
             (setf (type-def-expanded typedef) (expand-type (type-def-def typedef) scope))
             (setf (lookup-type type scope)
                   typedef)))))
       (;; t[type:supertype]
        (list (colon-exp :value (qly-symbol :value type)
                         :colon (qly-symbol :value supertype)))
        (unless (lookup type (scope-type-defs scope))
          (setf (lookup type (scope-type-defs scope))
                (make-type-def :name type :mexp mexp :scope scope)))
        (let ((typedef (lookup-type type scope))
              (supertype (lookup-type supertype scope)))
          (set-super-type typedef *any-type*)
          (set-super-type typedef supertype)))
       (;; t[type:supertype typedef]
        (list (colon-exp :value (qly-symbol :value type)
                         :colon (qly-symbol :value supertype))
              typedef)
        (when (lookup-type/direct type scope)
          (error "type ~a is already defined in this scope" type))
        (let ((typedef (make-type-def :name type :mexp mexp :def (process-type typedef scope) :scope scope))
              (supertype (lookup-type supertype scope)))
          (set-super-type typedef *any-type*)
          (set-super-type typedef supertype)
          (setf (type-def-expanded typedef) (expand-type (type-def-def typedef) scope))
          (setf (lookup-type type scope) typedef)))
       (_ (error "Expect t[type], t[type:supertype], t[type typedef] or t[type:supertype typedef]")))))
  ;; TODO: should catch malformed f[] and v[] (maybe in a later pass)
  )

;; In second pass, go inside fun body, recursively process three passes
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
  "Create var def in function's scope for each param"
  (loop for param in params do
    (match param
      ((colon-exp :value (qly-symbol :value name)
                  :colon type)
       (let ((type (process-type type scope)))
         (setf (lookup name (scope-var-defs scope))
               (make-var-def
                :name name
                :mexp param
                :type type
                :type-expanded (expand-type type scope)
                :scope scope))))
      ;; Currently all function args need to be typed, no h-m type inference yet
      ;; but in analyze-type pass, we keep the room for future allowing untyped args
      ((qly-symbol :value name)
       (setf (lookup name (scope-var-defs scope))
             (make-var-def
              :name name
              :mexp param
              :type :untyped
              :scope scope)))
      (_ (error "function param must be either param or param:type")))))

(defun process-param-types (params scope)
  "Extract param type part of a function type from a function definition"
  (mapcar (lambda (param)
            (match param
              ((colon-exp :value param
                          :colon type)
               (process-type type scope))
              ((mexp :value param)
               :untyped)
              (_ (error "function param must be either param or param:type"))
              ))
          params))

(defun process-type (type scope)
  (match type
    ((qly-symbol :value symbol)
     (or (lookup-type symbol scope)
         (error 'undefined-type :type-name symbol)))
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
    ((call-exp :value (qly-symbol :value :|f|)
               :args (qly-array :value (list (colon-exp :value (qly-array :value types) :colon return-type))))
     (make-fun-type :params (mapcar (lambda (type)
                                      (process-type type scope))
                                    types)
                    :return (process-type return-type scope)))
    (_ (error "Unknown pattern of type"))))

(defun process-field-types (fields scope)
  (mapcar (lambda (field)
            (match field
              ((colon-exp :value (qly-symbol :value field-name) :colon field-type)
               (make-struct-field :name field-name :type (process-type field-type scope)))
              (_ (error "Field must be symbol:type"))))
          fields))

;;; Resolve var pass, resolve every var to its definition, check type errors

(defun resolve-var (qly-sem)
  (loop for mexp in (qly-ast-mexp* (qly-sem-qly-ast qly-sem))
        do (resolve-var-mexp mexp
                             (qly-sem-symbol-scopes qly-sem)
                             (qly-sem-scopes qly-sem)
                             (gethash :root (qly-sem-scopes qly-sem))
                             0)))

(defun resolve-var-mexp (mexp symbol-scopes scopes scope quote)
  (match mexp
    ((qly-symbol)
     (resolve-var-qly-symbol mexp symbol-scopes scopes scope quote))
    ((colon-exp)
     (error "colon exp can only appeared in var, type and function definition"))
    ;; ((dot-exp :value value :prop prop)
    ;;  (resolve-var-mexp value scopes scope quote)
    ;;  (resolve-var-mexp prop scopes scope quote))
    ;; ((quote-exp :value value)
    ;;  (resolve-var-mexp value scopes scope (1+ quote)))
    ;; ((unquote-exp :value value)
    ;;  (resolve-var-mexp value scopes scope (1- quote)))
    ;; ((splice-exp :value value)
    ;;  (resolve-var-mexp value scopes scope (1- quote)))
    ((qly-array :value mexp*)
     (make-array-type :elem-type
                      (common-type (loop for mexp in mexp*
                                         collect (resolve-var-mexp mexp symbol-scopes scopes scope quote)))))
    ((call-exp)
     (resolve-var-call-exp mexp symbol-scopes scopes scope quote))
    ((qly-int :type int-type)
     int-type)
    ((qly-uint :type uint-type)
     uint-type)
    ((qly-string)
     :|string|)
    ((qly-real :type real-type)
     real-type)))

(defun resolve-var-qly-symbol (qly-symbol symbol-scopes scopes scope quote)
  (cond
    ((plusp quote))
    ((minusp quote) (error "Comma not inside a quote"))
    (t
     (if-let (def (lookup-var qly-symbol scope))
       (progn (setf (gethash qly-symbol symbol-scopes) scope)
              (push qly-symbol (var-def-occurs def))
              (var-def-type-auto def))
       (error 'undefined-var :var qly-symbol)))))

(defun common-type (types)
  "Most common parent type of types")

(defun var-def-type-auto (def)
  (declare (var-def def))
  "For type that refer to other types, use expanded form
For type that is toplevel (most builtin and custom generic type), type def itself"
  (when (eql :untyped (var-def-type def))
    (error "unable to resolve untyped var"))
  (or (var-def-type-expanded def) def))

(defun resolve-var-call-exp (call-exp symbol-scopes scopes scope quote)
  (match (call-exp-value call-exp)
    ((call-exp)
     (error "Unimplemented consequtive call exp"))
    ((qly-symbol)
     (let ((type (resolve-var-qly-symbol (call-exp-value call-exp) symbol-scopes scopes scope quote)))
       (cond
         ((builtin-op-p type)
          ;; An operation
          (resolve-var-builtin-op call-exp symbol-scopes scopes scope quote))
         ((array-type-p type)
          ;; array access
          (assert (list1p (qly-array-value (call-exp-args call-exp))))
          (let ((mexp-type (resolve-var-mexp (car (qly-array-value (call-exp-args call-exp))) symbol-scopes scopes scope quote)))
            (assert (type-compatible mexp-type (lookup-type :|uint| :root)))
            (array-type-elem-type type)))
         (t
          ;; A function call
          (assert (fun-type-p type))
          (fun-arg-type-ok (loop for mexp in (qly-array-value (call-exp-args call-exp))
                                 collect (resolve-var-mexp mexp symbol-scopes scopes scope quote))
                           type)
          (let ((ret (fun-type-return type)))
            (if (eql ret :untyped)
                (error "Unable to resolve untyped")
                ret))))))
    ((dot-exp)
     (error "Unimplemented dot exp call"))))

(defun fun-arg-type-ok (actual expected))

(defun type-compatible (actual expected)
  (is-subtype actual expected))

(defun is-subtype (actual-type expected-type)
  (or
   (equalp actual-type expected-type)
   (some (lambda (expected)
           (is-subtype actual-type expected))
         (type-def-children expected-type))))

(defun expand-type (type scope)
  (match type
    (:untyped :untyped)
    ((fun-type :params params :return return)
     (make-fun-type :params (mapcar (lambda (type) (expand-type type scope))
                                    params)
                    :return (expand-type return scope)))
    ((array-type :elem-type elem-type)
     (make-array-type :elem-type (expand-type elem-type scope)))
    ((struct-type :fields fields)
     (make-struct-type :fields (mapcar (lambda (field)
                                         (make-struct-field :name (struct-field-name field)
                                                            :type (expand-type (struct-field-type field) scope)))
                                       fields)))
    (_
     (let ((expanded (type-def-expanded (lookup type (scope-type-defs scope)))))
       (or expanded
           type)))))

(defun builtin-op-p (type)
  (op-type-p type))

(defun resolve-var-builtin-op (call-exp scopes symbol-scopes scope quote)
  (match call-exp
    ((call-exp :value (qly-symbol :value :|f|)
               :args (qly-array :value (list* (qly-symbol :value fname) _ mexp*)))
     ;;; TODO bind return so that return clause works
     (loop with result = (lookup-type :|nil| scope)
           for mexp in mexp*
           do (setf result (resolve-var-mexp mexp scopes symbol-scopes (gethash call-exp scopes) quote))
           finally (return result))
;;; TODO: with a name, f[name [...] ...] returns symbol name. without a name, it returns function itself
     )
    ((call-exp :value (qly-symbol :value :|v|)
               :args (qly-array :value
                                (list (colon-exp :value (qly-symbol :value var-name))
                                      value)))
     (let* ((var-def
              ;; must use lookup because lookup-var check position and var def not available until v[..] finish
              (lookup var-name (scope-var-defs scope)))
            (var-type (var-def-type-auto var-def))
            (value-type (resolve-var-mexp value scopes symbol-scopes scope quote)))
       (cond
         ((type-compatible
           value-type
           var-type)
          (lookup-type :|symbol| scope))
         ((type-convertible
           value-type
           var-type
           scope)
          (lookup-type :|symbol| scope))
         (t
          (error 'type-incompatible :expression value :expected-type (var-def-type var-def))))))))
;; TODO: more builtin special ops and fs

(defun type-convertible (source-type target-type scope)
  (find-applicable-method :|as| (list source-type (make-exact-type :value target-type)) scope))

(defun find-applicable-method (method-name method-param-types scope)
  )
