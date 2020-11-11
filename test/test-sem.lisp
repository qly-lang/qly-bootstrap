(in-package :cl-user)
(defpackage :qly.test-sem
  (:use :cl :fiveam :qly.sem)
  (:import-from :qly.parser :parse-qly-text))
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
"#))))
    (analyze-type sem)
    (var-def-env-is `((:|x| . ,(make-array-type :elem-type (make-array-type :elem-type :|string|)))
                      (:|y| . ,(make-array-type :elem-type :|string|)))
                    (scope-var-defs (gethash :root (qly-sem-scopes sem))))))
