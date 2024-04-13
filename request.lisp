(defpackage #:aws-sdk/request
  (:use #:cl)
  (:import-from #:aws-sdk/session
                #:*session*)
  (:import-from #:quri
                #:url-encode-params
                #:make-uri
                #:render-uri)
  (:import-from #:assoc-utils
                #:alistp)
  (:import-from #:com.inuoe.jzon
                #:parse
                #:stringify)
  (:import-from #:serapeum
                #:dict)
  (:import-from #:xml-emitter
                #:with-xml-output)
  (:export #:request
           #:request-service
           #:request-method
           #:request-path
           #:request-params
           #:request-headers
           #:request-payload
           #:request-session
           #:request-host
           #:request-endpoint
           #:*global-service-endpoints*))
(in-package #:aws-sdk/request)

(defparameter *global-service-endpoints* '("iam"
                                           "globalaccelerator"
                                           "cloudfront"
                                           "networkmanager"
                                           "organizations"
                                           "route53"
                                           "shield"
                                           "waf")
  "These are global services which don't support regions.  See: https://docs.aws.amazon.com/general/latest/gr/rande.html")

(defclass request ()
  ((service :initarg :service
            :initform (error ":service is required")
            :reader request-service)
   (method :initarg :method
           :initform (error ":method is required")
           :reader request-method)
   (path :initarg :path
         :initform "/"
         :reader request-path)
   (params :initarg :params
           :initform nil
           :reader request-params)
   (operation :initarg :operation
              :initform nil
              :reader request-operation)
   (protocol :initarg :protocol
             :initform (error ":protocol is required")
             :reader request-protocol)
   (headers :initarg :headers
            :initform nil
            :reader request-headers)
   (payload :initarg :payload
            :initform nil
            :reader request-payload)
   (session :initarg :session
            :initform (or *session* (error "aws-sdk:*session* has not been set; are you logged in?"))
            :reader request-session)))

(defmethod initialize-instance :after ((req request) &rest args &key path params &allow-other-keys)
  (declare (ignore args))
  (let ((uri (quri:uri path)))
    (setf (slot-value req 'path) (quri:uri-path uri))
    (case (request-protocol req)
      ((:ec2 :query) (setf (slot-value req 'params)
                           (append
                            (quri:uri-query-params uri)
                            (loop for (k . v) in params
                                  append (to-query-params k v)))))
      ((:rest-json :json) (setf (slot-value req 'payload)
                                (stringify
                                 (apply #'dict
                                        (loop for (k . v) in params
                                              append (list k v)))))))))


(defun to-query-params (key value)
  (typecase value
    (null)
    (cons
     (if (alistp value)
         (mapcar (lambda (kv)
                   (cons
                    (format nil "~A.~A" key (car kv))
                    (cdr kv)))
                 (loop for (k . v) in value
                       append (to-query-params k v)))
         (loop for i from 1
               for v in value
               collect (cons (format nil "~A.member.~A" key i) v))))
    (boolean
     (list (cons key
                 (if value
                     "true"
                     "false"))))
    (otherwise (list (cons key value)))))

(defgeneric request-host (request region)
  (:method ((req request) region)
    (if (member (request-service req) *global-service-endpoints* :test #'equalp)
        (format nil "~(~A~).amazonaws.com" (request-service req))
        (format nil "~(~A~).~(~A~).amazonaws.com" (request-service req) region))))

(defgeneric request-endpoint (request region)
  (:method ((req request) region)
    (with-slots (path params) req
      (quri:render-uri
        (quri:make-uri :scheme "https"
                       :host (request-host req region)
                       :path path
                       :query (and params
                                   (quri:url-encode-params params)))))))
