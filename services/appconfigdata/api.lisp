;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/appconfigdata/api
  (:use)
  (:nicknames #:aws/appconfigdata)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/appconfigdata/api)
(common-lisp:progn
 (common-lisp:defclass appconfigdata-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "appconfigdata" :protocol
                        :rest-json))
 (common-lisp:export 'appconfigdata-request))
(common-lisp:progn
 (common-lisp:define-condition appconfigdata-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'appconfigdata-error))
(common-lisp:defvar *error-map*
  '(("BadRequestException" . bad-request-exception)
    ("InternalServerException" . internal-server-exception)
    ("ResourceNotFoundException" . resource-not-found-exception)
    ("ThrottlingException" . throttling-exception)))
(common-lisp:progn
 (common-lisp:defstruct
     (bad-request-details (:copier common-lisp:nil)
      (:conc-name "struct-shape-bad-request-details-"))
   (invalid-parameters common-lisp:nil :type
    (common-lisp:or invalid-parameter-map common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'bad-request-details 'make-bad-request-details))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input bad-request-details))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input bad-request-details))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'invalid-parameters))
      (common-lisp:list
       (common-lisp:cons "InvalidParameters"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input bad-request-details))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:define-condition bad-request-exception
     (appconfigdata-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       bad-request-exception-message)
      (reason :initarg :reason :initform common-lisp:nil :reader
       bad-request-exception-reason)
      (details :initarg :details :initform common-lisp:nil :reader
       bad-request-exception-details)))
 (common-lisp:export
  (common-lisp:list 'bad-request-exception 'bad-request-exception-message
                    'bad-request-exception-reason
                    'bad-request-exception-details)))
(common-lisp:deftype bad-request-reason () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (get-latest-configuration-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-latest-configuration-request-"))
   (configuration-token (common-lisp:error ":configuration-token is required")
    :type (common-lisp:or token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-latest-configuration-request
                    'make-get-latest-configuration-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-latest-configuration-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-latest-configuration-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-latest-configuration-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (get-latest-configuration-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-latest-configuration-response-"))
   (next-poll-configuration-token common-lisp:nil :type
    (common-lisp:or token common-lisp:null))
   (next-poll-interval-in-seconds common-lisp:nil :type
    (common-lisp:or integer common-lisp:null))
   (content-type common-lisp:nil :type
    (common-lisp:or string common-lisp:null))
   (configuration common-lisp:nil :type
    (common-lisp:or sensitive-blob common-lisp:null))
   (version-label common-lisp:nil :type
    (common-lisp:or string common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-latest-configuration-response
                    'make-get-latest-configuration-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-latest-configuration-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'next-poll-configuration-token))
      (common-lisp:cons "Next-Poll-Configuration-Token"
                        aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'next-poll-interval-in-seconds))
      (common-lisp:cons "Next-Poll-Interval-In-Seconds"
                        aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-type))
      (common-lisp:cons "Content-Type" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'version-label))
      (common-lisp:cons "Version-Label" aws-sdk/generator/shape::value))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-latest-configuration-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'configuration))
      (common-lisp:list
       (common-lisp:cons "Configuration"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-latest-configuration-response))
   (common-lisp:slot-value aws-sdk/generator/shape::input 'configuration)))
(common-lisp:deftype identifier () 'common-lisp:string)
(common-lisp:deftype integer () 'common-lisp:integer)
(common-lisp:progn
 (common-lisp:define-condition internal-server-exception
     (appconfigdata-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-server-exception-message)))
 (common-lisp:export
  (common-lisp:list 'internal-server-exception
                    'internal-server-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (invalid-parameter-detail (:copier common-lisp:nil)
      (:conc-name "struct-shape-invalid-parameter-detail-"))
   (problem common-lisp:nil :type
    (common-lisp:or invalid-parameter-problem common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'invalid-parameter-detail 'make-invalid-parameter-detail))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          invalid-parameter-detail))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          invalid-parameter-detail))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'problem))
      (common-lisp:list
       (common-lisp:cons "Problem"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          invalid-parameter-detail))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype invalid-parameter-map () 'common-lisp:hash-table)
 (common-lisp:defun |make-invalid-parameter-map|
                    (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:deftype invalid-parameter-problem () 'common-lisp:string)
(common-lisp:deftype optional-poll-seconds () 'common-lisp:integer)
(common-lisp:progn
 (common-lisp:define-condition resource-not-found-exception
     (appconfigdata-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-not-found-exception-message)
      (resource-type :initarg :resource-type :initform common-lisp:nil :reader
       resource-not-found-exception-resource-type)
      (referenced-by :initarg :referenced-by :initform common-lisp:nil :reader
       resource-not-found-exception-referenced-by)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found-exception
                    'resource-not-found-exception-message
                    'resource-not-found-exception-resource-type
                    'resource-not-found-exception-referenced-by)))
(common-lisp:deftype resource-type () 'common-lisp:string)
(common-lisp:deftype sensitive-blob ()
  '(common-lisp:simple-array (common-lisp:unsigned-byte 8) (common-lisp:*)))
(common-lisp:progn
 (common-lisp:defstruct
     (start-configuration-session-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-start-configuration-session-request-"))
   (application-identifier
    (common-lisp:error ":application-identifier is required") :type
    (common-lisp:or identifier common-lisp:null))
   (environment-identifier
    (common-lisp:error ":environment-identifier is required") :type
    (common-lisp:or identifier common-lisp:null))
   (configuration-profile-identifier
    (common-lisp:error ":configuration-profile-identifier is required") :type
    (common-lisp:or identifier common-lisp:null))
   (required-minimum-poll-interval-in-seconds common-lisp:nil :type
    (common-lisp:or optional-poll-seconds common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'start-configuration-session-request
                    'make-start-configuration-session-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          start-configuration-session-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          start-configuration-session-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'application-identifier))
      (common-lisp:list
       (common-lisp:cons "ApplicationIdentifier"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'environment-identifier))
      (common-lisp:list
       (common-lisp:cons "EnvironmentIdentifier"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'configuration-profile-identifier))
      (common-lisp:list
       (common-lisp:cons "ConfigurationProfileIdentifier"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'required-minimum-poll-interval-in-seconds))
      (common-lisp:list
       (common-lisp:cons "RequiredMinimumPollIntervalInSeconds"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          start-configuration-session-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (start-configuration-session-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-start-configuration-session-response-"))
   (initial-configuration-token common-lisp:nil :type
    (common-lisp:or token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'start-configuration-session-response
                    'make-start-configuration-session-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          start-configuration-session-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          start-configuration-session-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'initial-configuration-token))
      (common-lisp:list
       (common-lisp:cons "InitialConfigurationToken"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          start-configuration-session-response))
   common-lisp:nil))
(common-lisp:deftype string () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype string-map () 'common-lisp:hash-table)
 (common-lisp:defun |make-string-map| (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:progn
 (common-lisp:define-condition throttling-exception
     (appconfigdata-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       throttling-exception-message)))
 (common-lisp:export
  (common-lisp:list 'throttling-exception 'throttling-exception-message)))
(common-lisp:deftype token () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun get-latest-configuration
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key configuration-token)
   (common-lisp:declare (common-lisp:ignorable configuration-token))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-get-latest-configuration-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 'appconfigdata-request
                                                        aws-sdk/generator/operation::input
                                                        "GET" :rest-json
                                                        "/configuration"
                                                        "GetLatestConfiguration"
                                                        "2021-11-11")
       :want-stream common-lisp:t)
      "blob" common-lisp:nil *error-map*)))
 (common-lisp:export 'get-latest-configuration))
(common-lisp:progn
 (common-lisp:defun start-configuration-session
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key application-identifier
                     environment-identifier configuration-profile-identifier
                     required-minimum-poll-interval-in-seconds)
   (common-lisp:declare
    (common-lisp:ignorable application-identifier environment-identifier
     configuration-profile-identifier
     required-minimum-poll-interval-in-seconds))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply
                       'make-start-configuration-session-request
                       aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 'appconfigdata-request
                                                        aws-sdk/generator/operation::input
                                                        "POST" :rest-json
                                                        "/configurationsessions"
                                                        "StartConfigurationSession"
                                                        "2021-11-11"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'start-configuration-session))
