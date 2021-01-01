(in-package :cl-user)
(defpackage :qly.test-sem
  (:use :cl :fiveam :qly.sem)
  (:import-from :qly.parser :parse-qly-text :qly-symbol-value :to-sexp))
(in-package :qly.test-sem)

(def-suite analyze-type-definition)
(in-suite analyze-type-definition)

(defmethod to-sexp ((type type-def))
  (type-def-name type))

(defmethod to-sexp ((type fun-type))
  (list :fun (mapcar 'to-sexp (fun-type-params type)) (to-sexp (fun-type-return type))))

(defmethod to-sexp ((type range-type))
  (list :range (range-type-start type) (range-type-end type)))

(defmethod to-sexp ((type array-type))
  (list :array (to-sexp (array-type-elem-type type))))

(defmethod to-sexp ((type struct-type))
  (cons :struct (mapcar (lambda (field)
                          (list (struct-field-name field)
                                (to-sexp (struct-field-type field))))
                        (struct-type-fields type))))

(defmethod to-sexp ((type (eql :untyped)))
  :untyped)

(defmethod op-type ((type op-type))
  (list :op (mapcar 'to-sexp (op-type-params type)) (to-sexp (op-type-return type))))

(defmethod op-type ((type exact-type))
  (list :exact (exact-type-value type)))

(defun var-def-env-is (expect env)
  (loop for (var . type) in expect
        do (is (equalp type
                       (to-sexp (var-def-type (gethash var (qly.sem::env-chain-%env env)))))))
  (is (= (length expect) (hash-table-count (qly.sem::env-chain-%env env)))))

(defun type-def-env-is (expect env)
  (loop for (type . def) in expect
        do (is (equalp def
                       (to-sexp (type-def-def (gethash type (qly.sem::env-chain-%env env)))))))
  (is (= (length expect) (hash-table-count (qly.sem::env-chain-%env env)))))

(defun type-def-env-subtype-has (expect env)
  (loop for (type . subtype) in expect
        do (progn
             (is (= (length subtype)
                    (length (type-def-children (gethash type (qly.sem::env-chain-%env env))))))
             (is (every (lambda (subtype subtype-def)
                          (eql (lookup subtype env) subtype-def))
                        subtype
                        (type-def-children (gethash type (qly.sem::env-chain-%env env))))))))

(test define-vars
  (let ((sem (make-qly-sem
              (parse-qly-text
               #"v[x:int 3]
v[y:string]
v[aaa:array[uint32]]
v[bbb:symbol 'x]"#))))
    (analyze-type sem)
    (var-def-env-is `((:|x| . :|int|)
                      (:|y| . :|string|)
                      (:|aaa| . (:array :|uint32|))
                      (:|bbb| . :|symbol|))
                    (scope-var-defs (gethash :root (qly-sem-scopes sem))))))

(test define-arrays
  (let ((sem (make-qly-sem
              (parse-qly-text
               #"v[x:array[array[string]]]
v[y:[string]]
v[z:[[string]]]
v[a:[array[string]]]
v[b:array[[string]]]
"#))))
    (analyze-type sem)
    (var-def-env-is `((:|x| . (:array (:array :|string|)))
                      (:|y| . (:array :|string|))
                      (:|z| . (:array (:array :|string|)))
                      (:|a| . (:array (:array :|string|)))
                      (:|b| . (:array (:array :|string|))))
                    (scope-var-defs (gethash :root (qly-sem-scopes sem))))))

(test define-structs
  (let ((sem (make-qly-sem
              (parse-qly-text
               #"v[x:struct[x:string]]
v[y:[y:string]]
v[z:[x:string y:[y:string] z:[string] a:[[x:string]]]]
"#))))
    (analyze-type sem)
    (var-def-env-is `((:|x| . (:struct (:|x| :|string|)))
                      (:|y| . (:struct (:|y| :|string|)))
                      (:|z| . (:struct
                               (:|x| :|string|)
                               (:|y| (:struct (:|y| :|string|)))
                               (:|z| (:array :|string|))
                               (:|a| (:array (:struct (:|x| :|string|)))))))
                    (scope-var-defs (gethash :root (qly-sem-scopes sem))))))


(test define-functions
  (let ((sem (make-qly-sem
              (parse-qly-text
               #"
f[x []]
f[foo [x]]
f[bar [x:int y:string]:uint]
f[high [x:f[[]:nil] y:f[[]:uint]]:f[[uint]:uint]]
"#))))
    (analyze-type sem)
    (print sem)
    (var-def-env-is `((:|x| . (:fun () :untyped))
                      (:|foo| . (:fun (:untyped) :untyped))
                      (:|bar| . (:fun (:|int| :|string|) :|uint|))
                      (:|high| . (:fun
                                  ((:fun () :|nil|)
                                   (:fun () :|uint|))
                                  (:fun (:|uint|) :|uint|))))
                    (scope-var-defs (gethash :root (qly-sem-scopes sem))))))

(test simple-type-def
  (let ((sem (make-qly-sem
              (parse-qly-text
               #"
t[a int]
t[b a]
"#))))
    (analyze-type sem)
    (type-def-env-is `((:|a| . :|int|)
                       (:|b| . :|a|))
                     (scope-type-defs (gethash :root (qly-sem-scopes sem))))))

(test complex-type-def
      (let ((sem (make-qly-sem
                  (parse-qly-text
                   #"
t[a f[[int]:int]]
t[b [int]]
t[c array[int]]
t[d [c:int]]
t[e struct[e:int]]
"#))))
        (analyze-type sem)
        (type-def-env-is `((:|a| . ,(make-fun-type :return :|int| :params (list :|int|)))
                           (:|b| . ,(make-array-type :elem-type :|int|))
                           (:|c| . ,(make-array-type :elem-type :|int|))
                           (:|d| . ,(make-struct-type :fields (list (make-struct-field :name :|c| :type :|int|))))
                           (:|e| . ,(make-struct-type :fields (list (make-struct-field :name :|e| :type :|int|))))                          )
                         (scope-type-defs (gethash :root (qly-sem-scopes sem))))))

(test recursive-type-def
  (let ((sem (make-qly-sem
              (parse-qly-text
               #"
t[tree]
t[data int]
t[node struct[node:data left:tree right:tree]]
t[leaf:tree data]
t[node:tree]

t[type1]
t[type2:type1 [type1]]
t[int:type1]
"#))))
    (analyze-type sem)
    (print sem)
    (type-def-env-is `((:|tree| . nil)
                       (:|node| . ,(make-struct-type :fields
                                                     (list
                                                      (make-struct-field :name :|node| :type :|data|)
                                                      (make-struct-field :name :|left| :type :|tree|)
                                                      (make-struct-field :name :|right| :type :|tree|))))
                       (:|leaf| . :|data|)
                       (:|data| . :|int|)
                       (:|type1| . nil)
                       (:|type2| . ,(make-array-type :elem-type :|type1|)))
                     (scope-type-defs (gethash :root (qly-sem-scopes sem))))
    (type-def-env-subtype-has `((:|tree| . (:|node| :|leaf|))
                                (:|type1| .(:|int| :|type2|)))
                              (scope-type-defs (gethash :root (qly-sem-scopes sem))))))

(test var-def-with-defined-type
  (let ((sem (make-qly-sem (parse-qly-text #"
t[x int]
v[y:x]
v[x:x 3]
"#))))
    (analyze-type sem)
    (type-def-env-is `((:|x| . :|int|))
                     (scope-type-defs (gethash :root (qly-sem-scopes sem))))
    (var-def-env-is `((:|y| . :|x|)
                      (:|x| . :|x|))
                    (scope-var-defs (gethash :root (qly-sem-scopes sem))))))

(test def-in-new-scope
  (let ((sem (make-qly-sem (parse-qly-text #"
t[type1 int]
v[var1:type1 3]
v[var2:type1 4]
f[foo [var1]
  t[type1 string]
  t[type2 bool]
  v[var2:type2 true]
  v[var3:type1]]
"#))))
    (analyze-type sem)
    (type-def-env-is `((:|type1| . :|int|))
                     (scope-type-defs (gethash :root (qly-sem-scopes sem))))
    (var-def-env-is `((:|var1| . :|type1|)
                      (:|var2| . :|type1|)
                      (:|foo| . ,(make-fun-type :return :untyped :params (list :untyped))))
                    (scope-var-defs (gethash :root (qly-sem-scopes sem))))
    (let ((foo-scope (gethash
                      (var-def-mexp (lookup :|foo|
                                            (scope-var-defs
                                             (gethash :root (qly-sem-scopes sem)))))
                      (qly-sem-scopes sem))))
      (type-def-env-is `((:|type1| . :|string|)
                         (:|type2| . :|bool|))
                       (scope-type-defs foo-scope))
      (var-def-env-is `((:|var2| . :|type2|)
                        (:|var3| . :|type1|)
                        (:|var1| . :untyped))
                      (scope-var-defs foo-scope)))))

(test inexist-type-def
  (let ((sem (make-qly-sem (parse-qly-text #"
t[type1 what]"#))))
    (signals undefined-type (analyze-type sem))))

(test inexist-type-in-var
  (let ((sem (make-qly-sem (parse-qly-text #"
v[x:what]"#))))
    (signals undefined-type (analyze-type sem))))

(def-suite resolve-vars)
(in-suite resolve-vars)

(test resolve-toplevel-symbol
  (let ((sem (make-qly-sem (parse-qly-text #"
true
"#))))
    (analyze-type sem)
    (resolve-var sem)))

(test resolve-toplevel-undefined
  (let ((sem (make-qly-sem (parse-qly-text #"
a"#))))
    (analyze-type sem)
    (signals undefined-var (resolve-var sem))))

(test resolve-var-def
  (let ((sem (make-qly-sem (parse-qly-text #"
#v[x:bool true]
v[y:int 2]
#v[z:uint 0x1]
#v[a:string "aaa"]"#))))
    (analyze-type sem)
    (resolve-var sem)
    ))

(test resolve-int-cast
  (let ((sem (make-qly-sem (parse-qly-text #"
v[x:int32 2]
"#))))
    (analyze-type sem)
    (resolve-var sem)))
