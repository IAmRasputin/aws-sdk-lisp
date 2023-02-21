;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/workmailmessageflow/api
  (:use)
  (:nicknames #:aws/workmailmessageflow)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/workmailmessageflow/api)
(common-lisp:progn
 (common-lisp:defclass workmailmessageflow-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "workmailmessageflow"))
 (common-lisp:export 'workmailmessageflow-request))
(common-lisp:progn
 (common-lisp:define-condition workmailmessageflow-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'workmailmessageflow-error))
(common-lisp:defvar *error-map*
  '(("InvalidContentLocation" . invalid-content-location)
    ("MessageFrozen" . message-frozen) ("MessageRejected" . message-rejected)
    ("ResourceNotFoundException" . resource-not-found-exception)))
(common-lisp:progn
 (common-lisp:defstruct
     (get-raw-message-content-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-raw-message-content-request-"))
   (message-id (common-lisp:error ":messageid is required") :type
    (common-lisp:or |messageIdType| common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-raw-message-content-request
                    'make-get-raw-message-content-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-raw-message-content-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-raw-message-content-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-raw-message-content-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (get-raw-message-content-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-raw-message-content-response-"))
   (message-content (common-lisp:error ":messagecontent is required") :type
    (common-lisp:or |messageContentBlob| common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-raw-message-content-response
                    'make-get-raw-message-content-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-raw-message-content-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-raw-message-content-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'message-content))
      (common-lisp:list
       (common-lisp:cons "messageContent"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-raw-message-content-response))
   (common-lisp:slot-value aws-sdk/generator/shape::input 'message-content)))
(common-lisp:progn
 (common-lisp:define-condition invalid-content-location
     (workmailmessageflow-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-content-location-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-content-location
                    'invalid-content-location-message)))
(common-lisp:progn
 (common-lisp:define-condition message-frozen
     (workmailmessageflow-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       message-frozen-message)))
 (common-lisp:export
  (common-lisp:list 'message-frozen 'message-frozen-message)))
(common-lisp:progn
 (common-lisp:define-condition message-rejected
     (workmailmessageflow-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       message-rejected-message)))
 (common-lisp:export
  (common-lisp:list 'message-rejected 'message-rejected-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (put-raw-message-content-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-put-raw-message-content-request-"))
   (message-id (common-lisp:error ":messageid is required") :type
    (common-lisp:or |messageIdType| common-lisp:null))
   (content (common-lisp:error ":content is required") :type
    (common-lisp:or raw-message-content common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'put-raw-message-content-request
                    'make-put-raw-message-content-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          put-raw-message-content-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          put-raw-message-content-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content))
      (common-lisp:list
       (common-lisp:cons "content"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          put-raw-message-content-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (put-raw-message-content-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-put-raw-message-content-response-")))
 (common-lisp:export
  (common-lisp:list 'put-raw-message-content-response
                    'make-put-raw-message-content-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          put-raw-message-content-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          put-raw-message-content-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          put-raw-message-content-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (raw-message-content (:copier common-lisp:nil)
      (:conc-name "struct-shape-raw-message-content-"))
   (s3reference (common-lisp:error ":s3reference is required") :type
    (common-lisp:or s3reference common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'raw-message-content 'make-raw-message-content))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input raw-message-content))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input raw-message-content))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 's3reference))
      (common-lisp:list
       (common-lisp:cons "s3Reference"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input raw-message-content))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:define-condition resource-not-found-exception
     (workmailmessageflow-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found-exception
                    'resource-not-found-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (s3reference (:copier common-lisp:nil)
      (:conc-name "struct-shape-s3reference-"))
   (bucket (common-lisp:error ":bucket is required") :type
    (common-lisp:or |s3BucketIdType| common-lisp:null))
   (key (common-lisp:error ":key is required") :type
    (common-lisp:or |s3KeyIdType| common-lisp:null))
   (object-version common-lisp:nil :type
    (common-lisp:or |s3VersionType| common-lisp:null)))
 (common-lisp:export (common-lisp:list 's3reference 'make-s3reference))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input s3reference))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input s3reference))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'bucket))
      (common-lisp:list
       (common-lisp:cons "bucket"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'key))
      (common-lisp:list
       (common-lisp:cons "key"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'object-version))
      (common-lisp:list
       (common-lisp:cons "objectVersion"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input s3reference))
   common-lisp:nil))
(common-lisp:deftype |errorMessage| () 'common-lisp:string)
(common-lisp:deftype |messageContentBlob| ()
  '(common-lisp:simple-array (common-lisp:unsigned-byte 8) (common-lisp:*)))
(common-lisp:deftype |messageIdType| () 'common-lisp:string)
(common-lisp:deftype |s3BucketIdType| () 'common-lisp:string)
(common-lisp:deftype |s3KeyIdType| () 'common-lisp:string)
(common-lisp:deftype |s3VersionType| () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun get-raw-message-content
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key message-id)
   (common-lisp:declare (common-lisp:ignorable message-id))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-get-raw-message-content-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'workmailmessageflow-request aws-sdk/generator/operation::input "GET"
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/messages/~A"
                              (quri.encode:url-encode
                               (common-lisp:slot-value
                                aws-sdk/generator/operation::input
                                'message-id))))
        "GetRawMessageContent" "2019-05-01")
       :want-stream common-lisp:t)
      "blob" common-lisp:nil *error-map*)))
 (common-lisp:export 'get-raw-message-content))
(common-lisp:progn
 (common-lisp:defun put-raw-message-content
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key message-id content)
   (common-lisp:declare (common-lisp:ignorable message-id content))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-put-raw-message-content-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'workmailmessageflow-request aws-sdk/generator/operation::input "POST"
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/messages/~A"
                              (quri.encode:url-encode
                               (common-lisp:slot-value
                                aws-sdk/generator/operation::input
                                'message-id))))
        "PutRawMessageContent" "2019-05-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'put-raw-message-content))
