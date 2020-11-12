(in-package :cl-user)
(defpackage :qly.test-sem
  (:use :cl :fiveam :qly.sem)
  (:import-from :qly.parser :parse-qly-text :qly-symbol-value))
(in-package :qly.test-sem)

(def-suite analyze-type-definition)
(in-suite analyze-type-definition)

(defun var-def-env-is (expect env)
  (is (= (length expect) (hash-table-count (qly.sem::env-chain-%env env))))
  (loop for (var . type) in expect
        do (is (type-equal type
                           (var-def-type (gethash var (qly.sem::env-chain-%env env)))))))

(defun type-equal (t1 t2)
  (when (eql (type-of t1) (type-of t2))
    (typecase t1
      (array-type (type-equal (array-type-elem-type t1) (array-type-elem-type t2)))
      (record-type (every (lambda (field1 field2)
                            (and
                             (eql (record-field-name field1)
                                  (record-field-name field2))
                             (type-equal (record-field-type field1)
                                         (record-field-type field2))))
                          (record-type-fields t1)
                          (record-type-fields t2)))
      (t (equal t1 t2)))))

(test define-vars
  (let ((sem (make-qly-sem
              (parse-qly-text
               #"v[x:int 3]
v[y:string]
v[aaa:array[u32]]
v[bbb:symbol 'x]"#))))
    (analyze-type sem)
    (var-def-env-is `((:|x| . :|integer|)
                      (:|y| . :|string|)
                      (:|aaa| . ,(make-array-type :elem-type :|u32|))
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

(test define-records
  (let ((sem (make-qly-sem
              (parse-qly-text
               #"v[x:record[x:string]]
v[y:[y:string]]
v[z:[x:string y:[y:string] z:[string] a:[[x:string]]]]
"#))))
    (analyze-type sem)
    (var-def-env-is `((:|x| . ,(make-record-type :fields (list (make-record-field :name :|x| :type :|string|))))
                      (:|y| . ,(make-record-type :fields (list (make-record-field :name :|y| :type :|string|))))
                      (:|z| . ,(make-record-type
                                :fields
                                (list
                                 (make-record-field :name :|x| :type :|string|)
                                 (make-record-field
                                  :name :|y|
                                  :type (make-record-type
                                         :fields (list
                                                  (make-record-field :name :|y| :type :|string|))))
                                 (make-record-field
                                  :name :|z|
                                  :type (make-array-type :elem-type :|string|))
                                 (make-record-field
                                  :name :|a|
                                  :type (make-array-type :elem-type
                                                         (make-record-type
                                                          :fields (list (make-record-field
                                                                         :name :|x|
                                                                         :type :|string|)))))))))
                    (scope-var-defs (gethash :root (qly-sem-scopes sem))))))
