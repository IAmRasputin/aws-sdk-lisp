(defpackage #:aws-sdk/generator/shape
  (:use #:cl
        #:trivial-types
        #:aws-sdk/utils)
  (:import-from #:alexandria
                #:ensure-car
                #:alist-hash-table
                #:flatten
                #:when-let)
  (:import-from #:serapeum
                #:@
                #:href-default
                #:maphash-return)
  (:import-from #:kebab
                #:to-pascal-case)
  (:import-from #:xml-emitter
                #:with-xml-output
                #:with-tag
                #:simple-tag)
  (:import-from #:com.inuoe.jzon
                #:stringify)
  (:export #:compile-shape
           #:make-request-with-input))
(in-package #:aws-sdk/generator/shape)

(defun composite-type-p (type-name)
  (<= (char-code #\A) (char-code (aref type-name 0)) (char-code #\Z)))

(defun lisp-native-type (type-name)
  (cond
    ((string= type-name "boolean") 'cl:boolean)
    ((string= type-name "string") 'cl:string)
    ((string= type-name "character") 'cl:character)
    ((string= type-name "blob") '(cl:simple-array (unsigned-byte 8) (*)))
    ((or (string= type-name "integer")
         (string= type-name "long")) 'cl:integer)
    ((string= type-name "float") 'cl:single-float)
    ((string= type-name "double") 'cl:double-float)
    ((string= type-name "timestamp") 'cl:string)
    (t (intern type-name))))

(defun lispify* (value &optional (package *package*))
  (check-type value string)
  (if (composite-type-p value)
      (lispify value package)
      (let ((*package* package))
        (lisp-native-type value))))

(defgeneric input-params (input)
  (:method (input) input))
(defgeneric input-headers (input))
(defgeneric input-payload (input))

(defun make-request-with-input (request-class input method protocol path-conversion action version)
  (make-instance request-class
                 :method method
                 :path (etypecase path-conversion
                         (string path-conversion)
                         (function (funcall path-conversion input))
                         (null "/"))
                 :params (append `(("Action" . ,action) ("Version" . ,version))
                                 (input-params input))
                 :protocol (intern (format nil "~:@(~A~)" protocol) (find-package :keyword))
                 :operation action
                 :headers (input-headers input)
                 :payload (input-payload input)))

(defun filter-member (key value members)
  (loop for member-name being each hash-key of members
        using (hash-value member-options)
        if (equal (gethash key member-options) value)
        collect (cons member-name member-options)))

(defun compile-structure-shape (name &key required members payload protocol)
  (let ((shape-name (lispify* name)))
    `(progn
       (defclass ,shape-name ()
         (,@(maphash-return (lambda (k v)
                              `(,(lispify k)
                                :initarg ,(lispify* k (find-package :keyword))
                                :type (or ,(lispify* (gethash "shape" v))
                                          ,@(when (gethash "streaming" v)
                                              '(stream pathname string))
                                          null)
                                :accessor ,(intern (format nil "%~:@(~a-~a~)" shape-name (lispify k)))
                                :initform ,(when (find k required :test #'string=)
                                             `(error ,(format nil ":~a is required" (lispify* k))))))
                            members)))

       (export (list ',shape-name
                     ',(intern (format nil "~:@(~A-~A~)" '#:make shape-name))))

       (defun ,(intern (format nil "~:@(~A-~A~)" '#:make shape-name)) (&rest args &key ,@(maphash-return (lambda (k v)
                                                                                                           (declare (ignore v))
                                                                                                           (lispify k))
                                                                                                         members))
         (apply #'make-instance ',shape-name args))

       (defmethod input-headers ((input ,shape-name))
         (append
          ,@(mapcar
             (lambda (key-value)
               (destructuring-bind (key . value) key-value
                 `(when-let (value (slot-value input ',(lispify key)))
                    (cons ,(gethash "locationName" value) value))))
             (filter-member "location" "header" members))
          ,@(mapcar
             (lambda (key-value)
               (destructuring-bind (key . value) key-value
                 `(when (slot-value input ',(lispify key))
                    (loop for key being each hash-key of (slot-value input ',(lispify key))
                            using (hash-value value)
                          collect (cons (format nil "~A~A" ,(gethash "locationName" value) key)
                                        value)))))
             (filter-member "location" "headers" members))))
       (defmethod input-params ((input ,shape-name))
         (append
          ,@(loop for key being each hash-key of members
                    using (hash-value value)
                  if (not (or (gethash "location" value)
                              (gethash "streaming" value)))
                    collect `(when-let (value (slot-value input ',(lispify key)))
                               (list (cons ,key (input-params value)))))))
       (defmethod input-payload ((input ,shape-name))
         ,(if payload
              (case protocol
                ((:json :rest-json) `(stringify (slot-value input ',(lispify payload))))
                (:rest-xml `(with-output-to-string (s)
                              (format t "~s~%" (com.inuoe.jzon:stringify input :pretty t))
                              (with-xml-output (s :encoding "UTF-8")
                                (with-tag (,payload nil ,(href-default nil members payload "xmlNamespace" "uri"))
                                  ())))))
              'nil)))))

(defun compile-exception-shape (name &key members exception)
  (let ((condition-name (lispify* name)))
    `(progn
       (define-condition ,condition-name (,exception)
         ,(loop for member-name being each hash-key of members
                  using (hash-value member-options)
                for slot-name = (lispify member-name)
                collect `(,(lispify member-name) :initarg ,(lispify* member-name (find-package :keyword))
                                                 :initform nil
                                                 :reader ,(lispify (format nil "~A-~A" condition-name slot-name)))))
       (export (list ',condition-name
                     ,@(loop for member-name being each hash-key of members
                             collect `',(lispify (format nil "~A-~A" condition-name (lispify member-name)))))))))

(defun compile-list-shape (name member)
  `(progn
     (deftype ,(lispify* name) () '(proper-list ,(lispify* member)))
     (defun ,(intern (format nil "~A-~A" '#:make (lispify* name))) (&rest members)
       (check-type members (proper-list ,(lispify* member)))
       members)))

(defun compile-map-shape (name)
  `(progn
     (deftype ,(lispify* name) () 'hash-table)
     (defun ,(intern (format nil "~A-~A" '#:make (lispify* name))) (key-values)
       (etypecase key-values
         (hash-table key-values)
         (list (alist-hash-table key-values))))))

(defun compile-otherwise (name type)
  (when (or (composite-type-p name)
            (not (eq (symbol-package (ensure-car (lisp-native-type name))) (find-package :cl))))
    `(deftype ,(if (composite-type-p name)
                   (lispify name)
                   (lisp-native-type name)) ()
       ',(if (composite-type-p type)
             (lispify type)
             (lisp-native-type type)))))

(defun compile-shape (name options exception-name service-protocol)
  (let ((type (gethash "type" options)))
    (cond
      ((string= type "map")
       (compile-map-shape name))
      ((string= type "list")
       (let ((member-type (@ options "member" "shape")))
         (assert member-type)
         (compile-list-shape name member-type)))
      ((string= type "structure")
       (if (gethash "exception" options)
           (compile-exception-shape name
                                    :members (gethash "members" options)
                                    :exception exception-name)
           (compile-structure-shape name
                                    :required (gethash "required" options)
                                    :members (gethash "members" options)
                                    :payload (gethash "payload" options)
                                    :protocol service-protocol)))
      (t
       (compile-otherwise name type)))))
