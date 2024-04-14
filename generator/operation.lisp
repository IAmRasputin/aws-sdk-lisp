(defpackage #:aws-sdk/generator/operation
  (:use #:cl
        #:aws-sdk/utils)
  (:import-from #:aws-sdk/generator/shape
                #:make-request-with-input)
  (:import-from #:aws-sdk/api
                #:aws-request)
  (:import-from #:aws-sdk/request
                #:request)
  (:import-from #:aws-sdk/error
                #:aws-error)
  (:import-from #:quri)
  (:import-from #:com.inuoe.jzon
                #:parse
                #:stringify)
  (:import-from #:cl-ppcre
                #:regex-replace-all
                #:do-matches-as-strings)
  (:import-from #:assoc-utils
                #:aget)
  (:import-from #:alexandria
                #:when-let
                #:ensure-car
                #:hash-table-alist)
  (:import-from #:babel)
  (:import-from #:xmls)
  (:export #:compile-operation))
(in-package #:aws-sdk/generator/operation)

(defun rec-hash-alist (json)
  (typecase json
    (hash-table (let ((alisted (hash-table-alist json)))
                  (dolist (entry alisted)
                    (setf (aget alisted (car entry))
                          (rec-hash-alist (aget alisted (car entry)))))
                  alisted))
    (string json) ;; Otherwise strings get picked up by the vector clause below
    (vector (map 'vector #'rec-hash-alist json))
    (t json)))

(defun %xmls-to-alist (xmls)
  (unless (consp xmls)
    (return-from %xmls-to-alist xmls))

  (destructuring-bind (name-and-ns attrs &rest contents) xmls
    (declare (ignore attrs))
    (cons (ensure-car name-and-ns)
          (mapcar #'%xmls-to-alist contents))))

(defun xmls-to-alist (xmls)
  (list (%xmls-to-alist xmls)))

(defun parse-response (response body-type wrapper-name error-map)
  (destructuring-bind (body status headers &rest ignore-args)
      response
    (declare (ignore ignore-args))
    (if (<= 400 status 599)
        (let ((body (ensure-string (or body ""))))
          (when (= 0 (length body))
            (error "Unexpected error raised with status=~A" status))
          (let* ((output (aget (xmls-to-alist (xmls:parse-to-list body)) "ErrorResponse"))
                 (error-alist (aget output "Error"))
                 (error-class (or (aget error-map (first (aget error-alist "Code")))
                                  'aws-error)))
            (break)
            (error error-class
                   :code (first (aget error-alist "Code"))
                   :message (first (aget error-alist "Message"))
                   :status status
                   :body body)))
        (if (equal body-type "blob")
            (ensure-string body)
            (let* ((body (ensure-string (or body "")))
                   (type-header (gethash "content-type" headers))
                   (is-json (or (equal type-header "application/x-amz-json-1.1")
                                (equal type-header "application/x-amz-json-1.0")
                                (equal type-header "application/json"))))
              (when (/= 0 (length body))
                (let* ((output (if is-json
                                   (rec-hash-alist (parse body))
                                   (cdr (first (xmls-to-alist (xmls:parse-to-list body)))))))
                  (if wrapper-name
                      (values (aget output wrapper-name)
                              (aget output "ResponseMetadata"))
                      output))))))))

(defun compile-path-pattern (path-pattern)
  (when path-pattern
    (let ((slots
            (let (slots)
              (ppcre:do-matches-as-strings (match "(?<={)[^}\\+]+\\+?(?=})" path-pattern (nreverse slots))
                (let* ((plus-ends (char= #\+ (aref match (1- (length match)))))
                       (slot-symbol (lispify (if plus-ends
                                                 (subseq match 0 (1- (length match)))
                                                 match))))
                  (push
                    (if plus-ends
                        `(slot-value input ',slot-symbol)
                        `(quri:url-encode (slot-value input ',slot-symbol)))
                    slots))))))
      (if slots
          `(lambda (input)
             (format nil ,(ppcre:regex-replace-all "{[^}]+}" path-pattern "~A")
                     ,@slots))
          path-pattern))))

(defun compile-operation (service name version options protocol params body-type error-map)
  (let* ((output (gethash "output" options))
         (method (gethash "method" (gethash "http" options)))
         (request-uri (gethash "requestUri" (gethash "http" options))))
    (if params
        (let ((input-shape-name (lispify (gethash "shape" (gethash "input" options)))))
          `(progn
             (defun ,(lispify name) (&rest args &key ,@params)
               (declare (ignorable ,@params))
               (let ((input (apply ',(intern (format nil "~:@(~A-~A~)" :make input-shape-name)) args)))
                 (parse-response
                  (aws-request
                    (make-request-with-input
                      ',(intern (format nil "~:@(~A-REQUEST~)" service))
                      input ,method ,protocol ,(compile-path-pattern request-uri) ,name ,version)
                    ,@(when (equal body-type "blob")
                        '(:want-stream t)))
                  ,body-type
                  ,(and output
                        (gethash "resultWrapper" output))
                  ,error-map)))
             (export ',(lispify name))))
        `(progn
           (defun ,(lispify name) ()
             (parse-response
               (aws-request
                 (make-instance ',(intern (format nil "~:@(~A-REQUEST~)" service))
                                :method ,method
                                :path ,request-uri
                                :protocol ,protocol
                                :operation ,name
                                :params `(("Action" . ,,name) ("Version" . ,,version)))
                 ,@(when (equal body-type "blob")
                     '(:want-stream t)))
              ,body-type
              ,(and output
                    (gethash "resultWrapper" output))
              ,error-map))
           (export ',(lispify name))))))
