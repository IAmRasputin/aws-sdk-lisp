;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/kinesis-video-signaling/api
  (:use)
  (:nicknames #:aws/kinesis-video-signaling)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/json-request)
  (:import-from #:aws-sdk/rest-json-request)
  (:import-from #:aws-sdk/rest-xml-request)
  (:import-from #:aws-sdk/query-request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/kinesis-video-signaling/api)
(common-lisp:progn
 (common-lisp:define-condition kinesis-video-signaling-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'kinesis-video-signaling-error))
(common-lisp:progn
 (common-lisp:defclass kinesis-video-signaling-request
                       (aws-sdk/generator/service::rest-json-request)
                       common-lisp:nil
                       (:default-initargs :service "kinesis-video-signaling"
                        :api-version "2019-12-04" :host-prefix "kinesisvideo"
                        :signing-name common-lisp:nil :global-host
                        common-lisp:nil))
 (common-lisp:export 'kinesis-video-signaling-request))
(common-lisp:defvar *error-map*
  '(("ClientLimitExceededException" . client-limit-exceeded-exception)
    ("InvalidArgumentException" . invalid-argument-exception)
    ("InvalidClientException" . invalid-client-exception)
    ("NotAuthorizedException" . not-authorized-exception)
    ("ResourceNotFoundException" . resource-not-found-exception)
    ("SessionExpiredException" . session-expired-exception)))
(common-lisp:deftype answer () 'common-lisp:string)
(common-lisp:deftype client-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition client-limit-exceeded-exception
     (kinesis-video-signaling-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       client-limit-exceeded-exception-message)))
 (common-lisp:export
  (common-lisp:list 'client-limit-exceeded-exception
                    'client-limit-exceeded-exception-message)))
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (get-ice-server-config-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-ice-server-config-request-"))
   (channel-arn (common-lisp:error ":channel-arn is required") :type
    (common-lisp:or resource-arn common-lisp:null))
   (client-id common-lisp:nil :type
    (common-lisp:or client-id common-lisp:null))
   (service common-lisp:nil :type (common-lisp:or service common-lisp:null))
   (username common-lisp:nil :type (common-lisp:or username common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-ice-server-config-request
                    'make-get-ice-server-config-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-ice-server-config-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-ice-server-config-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'channel-arn))
      (common-lisp:list
       (common-lisp:cons "ChannelARN"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'client-id))
      (common-lisp:list
       (common-lisp:cons "ClientId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'service))
      (common-lisp:list
       (common-lisp:cons "Service"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'username))
      (common-lisp:list
       (common-lisp:cons "Username"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-ice-server-config-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (get-ice-server-config-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-ice-server-config-response-"))
   (ice-server-list common-lisp:nil :type
    (common-lisp:or ice-server-list common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-ice-server-config-response
                    'make-get-ice-server-config-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-ice-server-config-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-ice-server-config-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'ice-server-list))
      (common-lisp:list
       (common-lisp:cons "IceServerList"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-ice-server-config-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (ice-server (:copier common-lisp:nil)
      (:conc-name "struct-shape-ice-server-"))
   (uris common-lisp:nil :type (common-lisp:or uris common-lisp:null))
   (username common-lisp:nil :type (common-lisp:or username common-lisp:null))
   (password common-lisp:nil :type (common-lisp:or password common-lisp:null))
   (ttl common-lisp:nil :type (common-lisp:or ttl common-lisp:null)))
 (common-lisp:export (common-lisp:list 'ice-server 'make-ice-server))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input ice-server))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input ice-server))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'uris))
      (common-lisp:list
       (common-lisp:cons "Uris"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'username))
      (common-lisp:list
       (common-lisp:cons "Username"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'password))
      (common-lisp:list
       (common-lisp:cons "Password"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'ttl))
      (common-lisp:list
       (common-lisp:cons "Ttl"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input ice-server))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype ice-server-list ()
   '(trivial-types:proper-list ice-server))
 (common-lisp:defun make-ice-server-list
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list ice-server))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:define-condition invalid-argument-exception
     (kinesis-video-signaling-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-argument-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-argument-exception
                    'invalid-argument-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition invalid-client-exception
     (kinesis-video-signaling-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-client-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-client-exception
                    'invalid-client-exception-message)))
(common-lisp:deftype message-payload () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition not-authorized-exception
     (kinesis-video-signaling-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       not-authorized-exception-message)))
 (common-lisp:export
  (common-lisp:list 'not-authorized-exception
                    'not-authorized-exception-message)))
(common-lisp:deftype password () 'common-lisp:string)
(common-lisp:deftype resource-arn () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition resource-not-found-exception
     (kinesis-video-signaling-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found-exception
                    'resource-not-found-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (send-alexa-offer-to-master-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-send-alexa-offer-to-master-request-"))
   (channel-arn (common-lisp:error ":channel-arn is required") :type
    (common-lisp:or resource-arn common-lisp:null))
   (sender-client-id (common-lisp:error ":sender-client-id is required") :type
    (common-lisp:or client-id common-lisp:null))
   (message-payload (common-lisp:error ":message-payload is required") :type
    (common-lisp:or message-payload common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'send-alexa-offer-to-master-request
                    'make-send-alexa-offer-to-master-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          send-alexa-offer-to-master-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          send-alexa-offer-to-master-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'channel-arn))
      (common-lisp:list
       (common-lisp:cons "ChannelARN"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'sender-client-id))
      (common-lisp:list
       (common-lisp:cons "SenderClientId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'message-payload))
      (common-lisp:list
       (common-lisp:cons "MessagePayload"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          send-alexa-offer-to-master-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (send-alexa-offer-to-master-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-send-alexa-offer-to-master-response-"))
   (answer common-lisp:nil :type (common-lisp:or answer common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'send-alexa-offer-to-master-response
                    'make-send-alexa-offer-to-master-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          send-alexa-offer-to-master-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          send-alexa-offer-to-master-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'answer))
      (common-lisp:list
       (common-lisp:cons "Answer"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          send-alexa-offer-to-master-response))
   common-lisp:nil))
(common-lisp:deftype service () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition session-expired-exception
     (kinesis-video-signaling-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       session-expired-exception-message)))
 (common-lisp:export
  (common-lisp:list 'session-expired-exception
                    'session-expired-exception-message)))
(common-lisp:deftype ttl () 'common-lisp:integer)
(common-lisp:deftype uri () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype uris () '(trivial-types:proper-list uri))
 (common-lisp:defun make-uris
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list uri))
   aws-sdk/generator/shape::members))
(common-lisp:deftype username () 'common-lisp:string)
(common-lisp:deftype |errorMessage| () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun get-ice-server-config
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key channel-arn client-id service username)
   (common-lisp:declare
    (common-lisp:ignorable channel-arn client-id service username))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-get-ice-server-config-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'kinesis-video-signaling-request aws-sdk/generator/operation::input
        "POST" "/v1/get-ice-server-config" "GetIceServerConfig"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'get-ice-server-config))
(common-lisp:progn
 (common-lisp:defun send-alexa-offer-to-master
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key channel-arn sender-client-id
                     message-payload)
   (common-lisp:declare
    (common-lisp:ignorable channel-arn sender-client-id message-payload))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply
                       'make-send-alexa-offer-to-master-request
                       aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'kinesis-video-signaling-request aws-sdk/generator/operation::input
        "POST" "/v1/send-alexa-offer-to-master" "SendAlexaOfferToMaster"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'send-alexa-offer-to-master))
