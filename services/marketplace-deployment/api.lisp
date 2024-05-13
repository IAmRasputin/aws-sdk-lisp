;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/marketplace-deployment/api
  (:use)
  (:nicknames #:aws/marketplace-deployment)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/json-request)
  (:import-from #:aws-sdk/rest-json-request)
  (:import-from #:aws-sdk/rest-xml-request)
  (:import-from #:aws-sdk/query-request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/marketplace-deployment/api)
(common-lisp:progn
 (common-lisp:define-condition marketplace-deployment-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'marketplace-deployment-error))
(common-lisp:progn
 (common-lisp:defclass marketplace-deployment-request
                       (aws-sdk/generator/service::rest-json-request)
                       common-lisp:nil
                       (:default-initargs :service "marketplace-deployment"
                        :api-version "2023-01-25" :host-prefix
                        "deployment-marketplace" :signing-name
                        "aws-marketplace" :global-host common-lisp:nil))
 (common-lisp:export 'marketplace-deployment-request))
(common-lisp:defvar *error-map*
  '(("AccessDeniedException" . access-denied-exception)
    ("ConflictException" . conflict-exception)
    ("InternalServerException" . internal-server-exception)
    ("ResourceNotFoundException" . resource-not-found-exception)
    ("ServiceQuotaExceededException" . service-quota-exceeded-exception)
    ("ThrottlingException" . throttling-exception)
    ("ValidationException" . validation-exception)))
(common-lisp:progn
 (common-lisp:define-condition access-denied-exception
     (marketplace-deployment-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       access-denied-exception-message)))
 (common-lisp:export
  (common-lisp:list 'access-denied-exception 'access-denied-exception-message)))
(common-lisp:deftype catalog () 'common-lisp:string)
(common-lisp:deftype client-token () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition conflict-exception
     (marketplace-deployment-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       conflict-exception-message)
      (resource-id :initarg :resource-id :initform common-lisp:nil :reader
       conflict-exception-resource-id)))
 (common-lisp:export
  (common-lisp:list 'conflict-exception 'conflict-exception-message
                    'conflict-exception-resource-id)))
(common-lisp:progn
 (common-lisp:defstruct
     (deployment-parameter-input (:copier common-lisp:nil)
      (:conc-name "struct-shape-deployment-parameter-input-"))
   (name (common-lisp:error ":name is required") :type
    (common-lisp:or deployment-parameter-name common-lisp:null))
   (secret-string (common-lisp:error ":secretstring is required") :type
    (common-lisp:or secret-string common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'deployment-parameter-input
                    'make-deployment-parameter-input))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          deployment-parameter-input))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          deployment-parameter-input))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'name))
      (common-lisp:list
       (common-lisp:cons "name"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'secret-string))
      (common-lisp:list
       (common-lisp:cons "secretString"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          deployment-parameter-input))
   common-lisp:nil))
(common-lisp:deftype deployment-parameter-name () 'common-lisp:string)
(common-lisp:deftype deployment-parameter-resource-identifier ()
  'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition internal-server-exception
     (marketplace-deployment-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-server-exception-message)))
 (common-lisp:export
  (common-lisp:list 'internal-server-exception
                    'internal-server-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (list-tags-for-resource-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-tags-for-resource-request-"))
   (resource-arn (common-lisp:error ":resourcearn is required") :type
    (common-lisp:or string common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-tags-for-resource-request
                    'make-list-tags-for-resource-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-tags-for-resource-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-tags-for-resource-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          list-tags-for-resource-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (list-tags-for-resource-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-tags-for-resource-response-"))
   (tags common-lisp:nil :type (common-lisp:or tags common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-tags-for-resource-response
                    'make-list-tags-for-resource-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-tags-for-resource-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-tags-for-resource-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'tags))
      (common-lisp:list
       (common-lisp:cons "tags"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          list-tags-for-resource-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (put-deployment-parameter-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-put-deployment-parameter-request-"))
   (agreement-id (common-lisp:error ":agreementid is required") :type
    (common-lisp:or resource-id common-lisp:null))
   (catalog (common-lisp:error ":catalog is required") :type
    (common-lisp:or catalog common-lisp:null))
   (client-token common-lisp:nil :type
    (common-lisp:or client-token common-lisp:null))
   (deployment-parameter (common-lisp:error ":deploymentparameter is required")
    :type (common-lisp:or deployment-parameter-input common-lisp:null))
   (expiration-date common-lisp:nil :type
    (common-lisp:or synthetic-timestamp-date-time common-lisp:null))
   (product-id (common-lisp:error ":productid is required") :type
    (common-lisp:or resource-id common-lisp:null))
   (tags common-lisp:nil :type (common-lisp:or tags-map common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'put-deployment-parameter-request
                    'make-put-deployment-parameter-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          put-deployment-parameter-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          put-deployment-parameter-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'agreement-id))
      (common-lisp:list
       (common-lisp:cons "agreementId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'client-token))
      (common-lisp:list
       (common-lisp:cons "clientToken"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'deployment-parameter))
      (common-lisp:list
       (common-lisp:cons "deploymentParameter"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'expiration-date))
      (common-lisp:list
       (common-lisp:cons "expirationDate"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'tags))
      (common-lisp:list
       (common-lisp:cons "tags"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          put-deployment-parameter-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (put-deployment-parameter-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-put-deployment-parameter-response-"))
   (agreement-id (common-lisp:error ":agreementid is required") :type
    (common-lisp:or resource-id common-lisp:null))
   (deployment-parameter-id
    (common-lisp:error ":deploymentparameterid is required") :type
    (common-lisp:or deployment-parameter-resource-identifier common-lisp:null))
   (resource-arn (common-lisp:error ":resourcearn is required") :type
    (common-lisp:or resource-arn common-lisp:null))
   (tags common-lisp:nil :type (common-lisp:or tags-map common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'put-deployment-parameter-response
                    'make-put-deployment-parameter-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          put-deployment-parameter-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          put-deployment-parameter-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'agreement-id))
      (common-lisp:list
       (common-lisp:cons "agreementId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'deployment-parameter-id))
      (common-lisp:list
       (common-lisp:cons "deploymentParameterId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'resource-arn))
      (common-lisp:list
       (common-lisp:cons "resourceArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'tags))
      (common-lisp:list
       (common-lisp:cons "tags"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          put-deployment-parameter-response))
   common-lisp:nil))
(common-lisp:deftype resource-arn () 'common-lisp:string)
(common-lisp:deftype resource-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition resource-not-found-exception
     (marketplace-deployment-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found-exception
                    'resource-not-found-exception-message)))
(common-lisp:deftype secret-string () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition service-quota-exceeded-exception
     (marketplace-deployment-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       service-quota-exceeded-exception-message)))
 (common-lisp:export
  (common-lisp:list 'service-quota-exceeded-exception
                    'service-quota-exceeded-exception-message)))
(common-lisp:deftype string () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype string-list () '(trivial-types:proper-list string))
 (common-lisp:defun make-string-list
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list string))
   aws-sdk/generator/shape::members))
(common-lisp:deftype synthetic-timestamp-date-time () 'common-lisp:string)
(common-lisp:deftype tag-key () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (tag-resource-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-tag-resource-request-"))
   (resource-arn (common-lisp:error ":resourcearn is required") :type
    (common-lisp:or string common-lisp:null))
   (tags common-lisp:nil :type (common-lisp:or tags common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'tag-resource-request 'make-tag-resource-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input tag-resource-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input tag-resource-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'tags))
      (common-lisp:list
       (common-lisp:cons "tags"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input tag-resource-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (tag-resource-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-tag-resource-response-")))
 (common-lisp:export
  (common-lisp:list 'tag-resource-response 'make-tag-resource-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          tag-resource-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          tag-resource-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          tag-resource-response))
   common-lisp:nil))
(common-lisp:deftype tag-value () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype tags () 'common-lisp:hash-table)
 (common-lisp:defun make-tags (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:progn
 (common-lisp:deftype tags-map () 'common-lisp:hash-table)
 (common-lisp:defun make-tags-map (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:progn
 (common-lisp:define-condition throttling-exception
     (marketplace-deployment-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       throttling-exception-message)))
 (common-lisp:export
  (common-lisp:list 'throttling-exception 'throttling-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (untag-resource-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-untag-resource-request-"))
   (resource-arn (common-lisp:error ":resourcearn is required") :type
    (common-lisp:or string common-lisp:null))
   (tag-keys (common-lisp:error ":tagkeys is required") :type
    (common-lisp:or string-list common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'untag-resource-request 'make-untag-resource-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          untag-resource-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          untag-resource-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          untag-resource-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (untag-resource-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-untag-resource-response-")))
 (common-lisp:export
  (common-lisp:list 'untag-resource-response 'make-untag-resource-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          untag-resource-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          untag-resource-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          untag-resource-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:define-condition validation-exception
     (marketplace-deployment-error)
     ((field-name :initarg :field-name :initform common-lisp:nil :reader
       validation-exception-field-name)
      (message :initarg :message :initform common-lisp:nil :reader
       validation-exception-message)))
 (common-lisp:export
  (common-lisp:list 'validation-exception 'validation-exception-field-name
                    'validation-exception-message)))
(common-lisp:progn
 (common-lisp:defun list-tags-for-resource
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key resource-arn)
   (common-lisp:declare (common-lisp:ignorable resource-arn))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-list-tags-for-resource-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'marketplace-deployment-request aws-sdk/generator/operation::input
        "GET"
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/tags/~A"
                              (quri.encode:url-encode
                               (common-lisp:slot-value
                                aws-sdk/generator/operation::input
                                'resource-arn))))
        "ListTagsForResource"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'list-tags-for-resource))
(common-lisp:progn
 (common-lisp:defun put-deployment-parameter
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key agreement-id catalog client-token
                     deployment-parameter expiration-date product-id tags)
   (common-lisp:declare
    (common-lisp:ignorable agreement-id catalog client-token
     deployment-parameter expiration-date product-id tags))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-put-deployment-parameter-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'marketplace-deployment-request aws-sdk/generator/operation::input
        "POST"
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil
                              "/catalogs/~A/products/~A/deployment-parameters"
                              (quri.encode:url-encode
                               (common-lisp:slot-value
                                aws-sdk/generator/operation::input 'catalog))
                              (quri.encode:url-encode
                               (common-lisp:slot-value
                                aws-sdk/generator/operation::input
                                'product-id))))
        "PutDeploymentParameter"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'put-deployment-parameter))
(common-lisp:progn
 (common-lisp:defun tag-resource
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key resource-arn tags)
   (common-lisp:declare (common-lisp:ignorable resource-arn tags))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-tag-resource-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'marketplace-deployment-request aws-sdk/generator/operation::input
        "POST"
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/tags/~A"
                              (quri.encode:url-encode
                               (common-lisp:slot-value
                                aws-sdk/generator/operation::input
                                'resource-arn))))
        "TagResource"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'tag-resource))
(common-lisp:progn
 (common-lisp:defun untag-resource
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key resource-arn tag-keys)
   (common-lisp:declare (common-lisp:ignorable resource-arn tag-keys))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-untag-resource-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'marketplace-deployment-request aws-sdk/generator/operation::input
        "DELETE"
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/tags/~A"
                              (quri.encode:url-encode
                               (common-lisp:slot-value
                                aws-sdk/generator/operation::input
                                'resource-arn))))
        "UntagResource"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'untag-resource))
