;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/s3outposts/api
  (:use)
  (:nicknames #:aws/s3outposts)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/s3outposts/api)
(common-lisp:progn
 (common-lisp:defclass s3outposts-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "s3outposts"))
 (common-lisp:export 's3outposts-request))
(common-lisp:progn
 (common-lisp:define-condition s3outposts-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 's3outposts-error))
(common-lisp:defvar *error-map*
  '(("AccessDeniedException" . access-denied-exception)
    ("ConflictException" . conflict-exception)
    ("InternalServerException" . internal-server-exception)
    ("OutpostOfflineException" . outpost-offline-exception)
    ("ResourceNotFoundException" . resource-not-found-exception)
    ("ThrottlingException" . throttling-exception)
    ("ValidationException" . validation-exception)))
(common-lisp:progn
 (common-lisp:define-condition access-denied-exception
     (s3outposts-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       access-denied-exception-message)))
 (common-lisp:export
  (common-lisp:list 'access-denied-exception 'access-denied-exception-message)))
(common-lisp:deftype aws-account-id () 'common-lisp:string)
(common-lisp:deftype capacity-in-bytes () 'common-lisp:integer)
(common-lisp:deftype cidr-block () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition conflict-exception
     (s3outposts-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       conflict-exception-message)))
 (common-lisp:export
  (common-lisp:list 'conflict-exception 'conflict-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (create-endpoint-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-create-endpoint-request-"))
   (outpost-id (common-lisp:error ":outpost-id is required") :type
    (common-lisp:or outpost-id common-lisp:null))
   (subnet-id (common-lisp:error ":subnet-id is required") :type
    (common-lisp:or subnet-id common-lisp:null))
   (security-group-id (common-lisp:error ":security-group-id is required")
    :type (common-lisp:or security-group-id common-lisp:null))
   (access-type common-lisp:nil :type
    (common-lisp:or endpoint-access-type common-lisp:null))
   (customer-owned-ipv4pool common-lisp:nil :type
    (common-lisp:or customer-owned-ipv4pool common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'create-endpoint-request 'make-create-endpoint-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          create-endpoint-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          create-endpoint-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'outpost-id))
      (common-lisp:list
       (common-lisp:cons "OutpostId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'subnet-id))
      (common-lisp:list
       (common-lisp:cons "SubnetId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'security-group-id))
      (common-lisp:list
       (common-lisp:cons "SecurityGroupId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'access-type))
      (common-lisp:list
       (common-lisp:cons "AccessType"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'customer-owned-ipv4pool))
      (common-lisp:list
       (common-lisp:cons "CustomerOwnedIpv4Pool"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          create-endpoint-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (create-endpoint-result (:copier common-lisp:nil)
      (:conc-name "struct-shape-create-endpoint-result-"))
   (endpoint-arn common-lisp:nil :type
    (common-lisp:or endpoint-arn common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'create-endpoint-result 'make-create-endpoint-result))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          create-endpoint-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          create-endpoint-result))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'endpoint-arn))
      (common-lisp:list
       (common-lisp:cons "EndpointArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          create-endpoint-result))
   common-lisp:nil))
(common-lisp:deftype creation-time () 'common-lisp:string)
(common-lisp:deftype customer-owned-ipv4pool () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (delete-endpoint-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-delete-endpoint-request-"))
   (endpoint-id (common-lisp:error ":endpoint-id is required") :type
    (common-lisp:or endpoint-id common-lisp:null))
   (outpost-id (common-lisp:error ":outpost-id is required") :type
    (common-lisp:or outpost-id common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'delete-endpoint-request 'make-delete-endpoint-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          delete-endpoint-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          delete-endpoint-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          delete-endpoint-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (endpoint (:copier common-lisp:nil) (:conc-name "struct-shape-endpoint-"))
   (endpoint-arn common-lisp:nil :type
    (common-lisp:or endpoint-arn common-lisp:null))
   (outposts-id common-lisp:nil :type
    (common-lisp:or outpost-id common-lisp:null))
   (cidr-block common-lisp:nil :type
    (common-lisp:or cidr-block common-lisp:null))
   (status common-lisp:nil :type
    (common-lisp:or endpoint-status common-lisp:null))
   (creation-time common-lisp:nil :type
    (common-lisp:or creation-time common-lisp:null))
   (network-interfaces common-lisp:nil :type
    (common-lisp:or network-interfaces common-lisp:null))
   (vpc-id common-lisp:nil :type (common-lisp:or vpc-id common-lisp:null))
   (subnet-id common-lisp:nil :type
    (common-lisp:or subnet-id common-lisp:null))
   (security-group-id common-lisp:nil :type
    (common-lisp:or security-group-id common-lisp:null))
   (access-type common-lisp:nil :type
    (common-lisp:or endpoint-access-type common-lisp:null))
   (customer-owned-ipv4pool common-lisp:nil :type
    (common-lisp:or customer-owned-ipv4pool common-lisp:null))
   (failed-reason common-lisp:nil :type
    (common-lisp:or failed-reason common-lisp:null)))
 (common-lisp:export (common-lisp:list 'endpoint 'make-endpoint))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input endpoint))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input endpoint))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'endpoint-arn))
      (common-lisp:list
       (common-lisp:cons "EndpointArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'outposts-id))
      (common-lisp:list
       (common-lisp:cons "OutpostsId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'cidr-block))
      (common-lisp:list
       (common-lisp:cons "CidrBlock"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'status))
      (common-lisp:list
       (common-lisp:cons "Status"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'creation-time))
      (common-lisp:list
       (common-lisp:cons "CreationTime"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'network-interfaces))
      (common-lisp:list
       (common-lisp:cons "NetworkInterfaces"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'vpc-id))
      (common-lisp:list
       (common-lisp:cons "VpcId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'subnet-id))
      (common-lisp:list
       (common-lisp:cons "SubnetId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'security-group-id))
      (common-lisp:list
       (common-lisp:cons "SecurityGroupId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'access-type))
      (common-lisp:list
       (common-lisp:cons "AccessType"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'customer-owned-ipv4pool))
      (common-lisp:list
       (common-lisp:cons "CustomerOwnedIpv4Pool"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'failed-reason))
      (common-lisp:list
       (common-lisp:cons "FailedReason"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input endpoint))
   common-lisp:nil))
(common-lisp:deftype endpoint-access-type () 'common-lisp:string)
(common-lisp:deftype endpoint-arn () 'common-lisp:string)
(common-lisp:deftype endpoint-id () 'common-lisp:string)
(common-lisp:deftype endpoint-status () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype endpoints () '(trivial-types:proper-list endpoint))
 (common-lisp:defun |make-endpoints|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list endpoint))
   aws-sdk/generator/shape::members))
(common-lisp:deftype error-code () 'common-lisp:string)
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (failed-reason (:copier common-lisp:nil)
      (:conc-name "struct-shape-failed-reason-"))
   (error-code common-lisp:nil :type
    (common-lisp:or error-code common-lisp:null))
   (message common-lisp:nil :type (common-lisp:or message common-lisp:null)))
 (common-lisp:export (common-lisp:list 'failed-reason 'make-failed-reason))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input failed-reason))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input failed-reason))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'error-code))
      (common-lisp:list
       (common-lisp:cons "ErrorCode"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'message))
      (common-lisp:list
       (common-lisp:cons "Message"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input failed-reason))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:define-condition internal-server-exception
     (s3outposts-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-server-exception-message)))
 (common-lisp:export
  (common-lisp:list 'internal-server-exception
                    'internal-server-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (list-endpoints-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-endpoints-request-"))
   (next-token common-lisp:nil :type
    (common-lisp:or next-token common-lisp:null))
   (max-results common-lisp:nil :type
    (common-lisp:or max-results common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-endpoints-request 'make-list-endpoints-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-endpoints-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-endpoints-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          list-endpoints-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (list-endpoints-result (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-endpoints-result-"))
   (endpoints common-lisp:nil :type
    (common-lisp:or endpoints common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or next-token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-endpoints-result 'make-list-endpoints-result))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-endpoints-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-endpoints-result))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'endpoints))
      (common-lisp:list
       (common-lisp:cons "Endpoints"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'next-token))
      (common-lisp:list
       (common-lisp:cons "NextToken"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          list-endpoints-result))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (list-outposts-with-s3request (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-outposts-with-s3request-"))
   (next-token common-lisp:nil :type
    (common-lisp:or next-token common-lisp:null))
   (max-results common-lisp:nil :type
    (common-lisp:or max-results common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-outposts-with-s3request
                    'make-list-outposts-with-s3request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-outposts-with-s3request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-outposts-with-s3request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          list-outposts-with-s3request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (list-outposts-with-s3result (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-outposts-with-s3result-"))
   (outposts common-lisp:nil :type (common-lisp:or outposts common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or next-token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-outposts-with-s3result
                    'make-list-outposts-with-s3result))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-outposts-with-s3result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-outposts-with-s3result))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'outposts))
      (common-lisp:list
       (common-lisp:cons "Outposts"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'next-token))
      (common-lisp:list
       (common-lisp:cons "NextToken"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          list-outposts-with-s3result))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (list-shared-endpoints-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-shared-endpoints-request-"))
   (next-token common-lisp:nil :type
    (common-lisp:or next-token common-lisp:null))
   (max-results common-lisp:nil :type
    (common-lisp:or max-results common-lisp:null))
   (outpost-id (common-lisp:error ":outpost-id is required") :type
    (common-lisp:or outpost-id common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-shared-endpoints-request
                    'make-list-shared-endpoints-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-shared-endpoints-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-shared-endpoints-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          list-shared-endpoints-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (list-shared-endpoints-result (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-shared-endpoints-result-"))
   (endpoints common-lisp:nil :type
    (common-lisp:or endpoints common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or next-token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-shared-endpoints-result
                    'make-list-shared-endpoints-result))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-shared-endpoints-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-shared-endpoints-result))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'endpoints))
      (common-lisp:list
       (common-lisp:cons "Endpoints"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'next-token))
      (common-lisp:list
       (common-lisp:cons "NextToken"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          list-shared-endpoints-result))
   common-lisp:nil))
(common-lisp:deftype max-results () 'common-lisp:integer)
(common-lisp:deftype message () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (network-interface (:copier common-lisp:nil)
      (:conc-name "struct-shape-network-interface-"))
   (network-interface-id common-lisp:nil :type
    (common-lisp:or network-interface-id common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'network-interface 'make-network-interface))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input network-interface))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input network-interface))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'network-interface-id))
      (common-lisp:list
       (common-lisp:cons "NetworkInterfaceId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input network-interface))
   common-lisp:nil))
(common-lisp:deftype network-interface-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype network-interfaces ()
   '(trivial-types:proper-list network-interface))
 (common-lisp:defun |make-network-interfaces|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list network-interface))
   aws-sdk/generator/shape::members))
(common-lisp:deftype next-token () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (outpost (:copier common-lisp:nil) (:conc-name "struct-shape-outpost-"))
   (outpost-arn common-lisp:nil :type
    (common-lisp:or outpost-arn common-lisp:null))
   (outpost-id common-lisp:nil :type
    (common-lisp:or outpost-id common-lisp:null))
   (owner-id common-lisp:nil :type
    (common-lisp:or aws-account-id common-lisp:null))
   (capacity-in-bytes common-lisp:nil :type
    (common-lisp:or capacity-in-bytes common-lisp:null)))
 (common-lisp:export (common-lisp:list 'outpost 'make-outpost))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input outpost))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input outpost))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'outpost-arn))
      (common-lisp:list
       (common-lisp:cons "OutpostArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'outpost-id))
      (common-lisp:list
       (common-lisp:cons "OutpostId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'owner-id))
      (common-lisp:list
       (common-lisp:cons "OwnerId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'capacity-in-bytes))
      (common-lisp:list
       (common-lisp:cons "CapacityInBytes"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input outpost))
   common-lisp:nil))
(common-lisp:deftype outpost-arn () 'common-lisp:string)
(common-lisp:deftype outpost-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition outpost-offline-exception
     (s3outposts-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       outpost-offline-exception-message)))
 (common-lisp:export
  (common-lisp:list 'outpost-offline-exception
                    'outpost-offline-exception-message)))
(common-lisp:progn
 (common-lisp:deftype outposts () '(trivial-types:proper-list outpost))
 (common-lisp:defun |make-outposts|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list outpost))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:define-condition resource-not-found-exception
     (s3outposts-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found-exception
                    'resource-not-found-exception-message)))
(common-lisp:deftype security-group-id () 'common-lisp:string)
(common-lisp:deftype subnet-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition throttling-exception
     (s3outposts-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       throttling-exception-message)))
 (common-lisp:export
  (common-lisp:list 'throttling-exception 'throttling-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition validation-exception
     (s3outposts-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       validation-exception-message)))
 (common-lisp:export
  (common-lisp:list 'validation-exception 'validation-exception-message)))
(common-lisp:deftype vpc-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun create-endpoint
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key outpost-id subnet-id security-group-id
                     access-type customer-owned-ipv4pool)
   (common-lisp:declare
    (common-lisp:ignorable outpost-id subnet-id security-group-id access-type
     customer-owned-ipv4pool))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-create-endpoint-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 's3outposts-request
                                                        aws-sdk/generator/operation::input
                                                        "POST"
                                                        "/S3Outposts/CreateEndpoint"
                                                        "CreateEndpoint"
                                                        "2017-07-25"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'create-endpoint))
(common-lisp:progn
 (common-lisp:defun delete-endpoint
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key endpoint-id outpost-id)
   (common-lisp:declare (common-lisp:ignorable endpoint-id outpost-id))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-delete-endpoint-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 's3outposts-request
                                                        aws-sdk/generator/operation::input
                                                        "DELETE"
                                                        "/S3Outposts/DeleteEndpoint"
                                                        "DeleteEndpoint"
                                                        "2017-07-25"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'delete-endpoint))
(common-lisp:progn
 (common-lisp:defun list-endpoints
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key next-token max-results)
   (common-lisp:declare (common-lisp:ignorable next-token max-results))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-list-endpoints-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 's3outposts-request
                                                        aws-sdk/generator/operation::input
                                                        "GET"
                                                        "/S3Outposts/ListEndpoints"
                                                        "ListEndpoints"
                                                        "2017-07-25"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'list-endpoints))
(common-lisp:progn
 (common-lisp:defun list-outposts-with-s3
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key next-token max-results)
   (common-lisp:declare (common-lisp:ignorable next-token max-results))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-list-outposts-with-s3request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 's3outposts-request
                                                        aws-sdk/generator/operation::input
                                                        "GET"
                                                        "/S3Outposts/ListOutpostsWithS3"
                                                        "ListOutpostsWithS3"
                                                        "2017-07-25"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'list-outposts-with-s3))
(common-lisp:progn
 (common-lisp:defun list-shared-endpoints
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key next-token max-results outpost-id)
   (common-lisp:declare
    (common-lisp:ignorable next-token max-results outpost-id))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-list-shared-endpoints-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 's3outposts-request
                                                        aws-sdk/generator/operation::input
                                                        "GET"
                                                        "/S3Outposts/ListSharedEndpoints"
                                                        "ListSharedEndpoints"
                                                        "2017-07-25"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'list-shared-endpoints))
