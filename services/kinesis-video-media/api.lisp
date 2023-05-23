;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/kinesis-video-media/api
  (:use)
  (:nicknames #:aws/kinesis-video-media)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/kinesis-video-media/api)
(common-lisp:progn
 (common-lisp:defclass kinesis-video-media-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "kinesis-video-media"
                        :protocol :rest-json))
 (common-lisp:export 'kinesis-video-media-request))
(common-lisp:progn
 (common-lisp:define-condition kinesis-video-media-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'kinesis-video-media-error))
(common-lisp:defvar *error-map*
  '(("ClientLimitExceededException" . client-limit-exceeded-exception)
    ("ConnectionLimitExceededException" . connection-limit-exceeded-exception)
    ("InvalidArgumentException" . invalid-argument-exception)
    ("InvalidEndpointException" . invalid-endpoint-exception)
    ("NotAuthorizedException" . not-authorized-exception)
    ("ResourceNotFoundException" . resource-not-found-exception)))
(common-lisp:progn
 (common-lisp:define-condition client-limit-exceeded-exception
     (kinesis-video-media-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       client-limit-exceeded-exception-message)))
 (common-lisp:export
  (common-lisp:list 'client-limit-exceeded-exception
                    'client-limit-exceeded-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition connection-limit-exceeded-exception
     (kinesis-video-media-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       connection-limit-exceeded-exception-message)))
 (common-lisp:export
  (common-lisp:list 'connection-limit-exceeded-exception
                    'connection-limit-exceeded-exception-message)))
(common-lisp:deftype content-type () 'common-lisp:string)
(common-lisp:deftype continuation-token () 'common-lisp:string)
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:deftype fragment-number-string () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (get-media-input (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-media-input-"))
   (stream-name common-lisp:nil :type
    (common-lisp:or stream-name common-lisp:null))
   (stream-arn common-lisp:nil :type
    (common-lisp:or resource-arn common-lisp:null))
   (start-selector (common-lisp:error ":start-selector is required") :type
    (common-lisp:or start-selector common-lisp:null)))
 (common-lisp:export (common-lisp:list 'get-media-input 'make-get-media-input))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input get-media-input))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input get-media-input))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'stream-name))
      (common-lisp:list
       (common-lisp:cons "StreamName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'stream-arn))
      (common-lisp:list
       (common-lisp:cons "StreamARN"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'start-selector))
      (common-lisp:list
       (common-lisp:cons "StartSelector"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input get-media-input))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (get-media-output (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-media-output-"))
   (content-type common-lisp:nil :type
    (common-lisp:or content-type common-lisp:null))
   (payload common-lisp:nil :type (common-lisp:or payload common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-media-output 'make-get-media-output))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input get-media-output))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-type))
      (common-lisp:cons "Content-Type" aws-sdk/generator/shape::value))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input get-media-output))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'payload))
      (common-lisp:list
       (common-lisp:cons "Payload"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input get-media-output))
   (common-lisp:slot-value aws-sdk/generator/shape::input 'payload)))
(common-lisp:progn
 (common-lisp:define-condition invalid-argument-exception
     (kinesis-video-media-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-argument-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-argument-exception
                    'invalid-argument-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition invalid-endpoint-exception
     (kinesis-video-media-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-endpoint-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-endpoint-exception
                    'invalid-endpoint-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition not-authorized-exception
     (kinesis-video-media-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       not-authorized-exception-message)))
 (common-lisp:export
  (common-lisp:list 'not-authorized-exception
                    'not-authorized-exception-message)))
(common-lisp:deftype payload ()
  '(common-lisp:simple-array (common-lisp:unsigned-byte 8) (common-lisp:*)))
(common-lisp:deftype resource-arn () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition resource-not-found-exception
     (kinesis-video-media-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found-exception
                    'resource-not-found-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (start-selector (:copier common-lisp:nil)
      (:conc-name "struct-shape-start-selector-"))
   (start-selector-type (common-lisp:error ":start-selector-type is required")
    :type (common-lisp:or start-selector-type common-lisp:null))
   (after-fragment-number common-lisp:nil :type
    (common-lisp:or fragment-number-string common-lisp:null))
   (start-timestamp common-lisp:nil :type
    (common-lisp:or timestamp common-lisp:null))
   (continuation-token common-lisp:nil :type
    (common-lisp:or continuation-token common-lisp:null)))
 (common-lisp:export (common-lisp:list 'start-selector 'make-start-selector))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input start-selector))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input start-selector))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'start-selector-type))
      (common-lisp:list
       (common-lisp:cons "StartSelectorType"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'after-fragment-number))
      (common-lisp:list
       (common-lisp:cons "AfterFragmentNumber"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'start-timestamp))
      (common-lisp:list
       (common-lisp:cons "StartTimestamp"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'continuation-token))
      (common-lisp:list
       (common-lisp:cons "ContinuationToken"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input start-selector))
   common-lisp:nil))
(common-lisp:deftype start-selector-type () 'common-lisp:string)
(common-lisp:deftype stream-name () 'common-lisp:string)
(common-lisp:deftype timestamp () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun get-media
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key stream-name stream-arn start-selector)
   (common-lisp:declare
    (common-lisp:ignorable stream-name stream-arn start-selector))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-get-media-input
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'kinesis-video-media-request aws-sdk/generator/operation::input "POST"
        :rest-json "/getMedia" "GetMedia" "2017-09-30")
       :want-stream common-lisp:t)
      "blob" common-lisp:nil *error-map*)))
 (common-lisp:export 'get-media))
