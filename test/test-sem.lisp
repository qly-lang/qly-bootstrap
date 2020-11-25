(in-package :cl-user)
(defpackage :qly.test-sem
  (:use :cl :fiveam :qly.sem)
  (:import-from :qly.parser :parse-qly-text :qly-symbol-value))
(in-package :qly.test-sem)

(def-suite analyze-type-definition)
(in-suite analyze-type-definition)

(defun var-def-env-is (expect env)
  (loop for (var . type) in expect
        do (is (equalp type
                       (var-def-type (gethash var (qly.sem::env-chain-%env env))))))
  (is (= (length expect) (hash-table-count (qly.sem::env-chain-%env env)))))

(defun type-def-env-is (expect env)
  (loop for (type . def) in expect
        do (is (equalp def
                       (type-def-def (gethash type (qly.sem::env-chain-%env env))))))
  (is (= (length expect) (hash-table-count (qly.sem::env-chain-%env env)))))

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
                      (:|aaa| . ,(make-array-type :elem-type :|uint32|))
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
    (var-def-env-is `((:|x| . ,(make-array-type :elem-type (make-array-type :elem-type :|string|)))
                      (:|y| . ,(make-array-type :elem-type :|string|))
                      (:|z| . ,(make-array-type :elem-type (make-array-type :elem-type :|string|)))
                      (:|a| . ,(make-array-type :elem-type (make-array-type :elem-type :|string|)))
                      (:|b| . ,(make-array-type :elem-type (make-array-type :elem-type :|string|))))
                    (scope-var-defs (gethash :root (qly-sem-scopes sem))))))

(test define-structs
      (let ((sem (make-qly-sem
                  (parse-qly-text
                   #"v[x:struct[x:string]]
v[y:[y:string]]
v[z:[x:string y:[y:string] z:[string] a:[[x:string]]]]
"#))))
        (analyze-type sem)
        (var-def-env-is `((:|x| . ,(make-struct-type :fields (list (make-struct-field :name :|x| :type :|string|))))
                          (:|y| . ,(make-struct-type :fields (list (make-struct-field :name :|y| :type :|string|))))
                          (:|z| . ,(make-struct-type
                                    :fields
                                    (list
                                     (make-struct-field :name :|x| :type :|string|)
                                     (make-struct-field
                                      :name :|y|
                                      :type (make-struct-type
                                             :fields (list
                                                      (make-struct-field :name :|y| :type :|string|))))
                                     (make-struct-field
                                      :name :|z|
                                      :type (make-array-type :elem-type :|string|))
                                     (make-struct-field
                                      :name :|a|
                                      :type (make-array-type :elem-type
                                                             (make-struct-type
                                                              :fields (list (make-struct-field
                                                                             :name :|x|
                                                                             :type :|string|)))))))))
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
        (var-def-env-is `((:|x| . ,(make-fun-type :return :untyped :params nil))
                          (:|foo| . ,(make-fun-type :return :untyped :params (list :untyped)))
                          (:|bar| . ,(make-fun-type :return :|uint| :params (list :|int| :|string|)))
                          (:|high| . ,(make-fun-type :return (make-fun-type :return :|uint| :params (list :|uint|))
                                                     :params (list (make-fun-type :return :|nil| :params ())
                                                                   (make-fun-type :return :|uint| :params ())))))
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
t[node struct[node:data left:tree right:tree]]
t[leaf:tree data]
t[data int]
t[node:tree]

t[type1]
t[type2:type1 [type1]]
t[int:type1]
"#))))
    (analyze-type sem)
    (type-def-env-is `((:|tree| . ,(make-or-type :variants
                                                 (list
                                                  (make-struct-type
                                                   :fields
                                                   (list
                                                    (make-struct-field :name :|node| :type :|data|)
                                                    (make-struct-field :name :|left| :type :|tree|)
                                                    (make-struct-field :name :|right| :type :|tree|)))
                                                  :|leaf|)))
                       (:|leaf| . :|data|)
                       (:|data| . :|int|)
                       (:|type1| . ,(make-or-type :variants
                                                  (list
                                                   :|type2|
                                                   :|int|)))
                       (:|type2| . ,(make-array-type :elem-type :|type1|)))
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

(def-suite resolve-vars)
(in-suite resolve-vars)

(test resolve-toplevel-symbol
  (let ((sem (make-qly-sem (parse-qly-text #"
true
"#))))
    (analyze-type sem)
    (resolve-var sem)))
