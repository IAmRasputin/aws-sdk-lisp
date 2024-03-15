;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/entitlement.marketplace/api
  (:use)
  (:nicknames #:aws/entitlement.marketplace)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/json-request)
  (:import-from #:aws-sdk/rest-json-request)
  (:import-from #:aws-sdk/rest-xml-request)
  (:import-from #:aws-sdk/query-request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/entitlement.marketplace/api)
(common-lisp:progn
 (common-lisp:define-condition entitlement.marketplace-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'entitlement.marketplace-error))
(common-lisp:progn
 (common-lisp:defclass entitlement.marketplace-request
                       (aws-sdk/json-request:json-request) common-lisp:nil
                       (:default-initargs :service "entitlement.marketplace"
                        :api-version "2017-01-11" :host-prefix
                        "entitlement.marketplace" :signing-name
                        "aws-marketplace" :global-host common-lisp:nil
                        :target-prefix "AWSMPEntitlementService" :json-version
                        "1.1"))
 (common-lisp:export 'entitlement.marketplace-request))
(common-lisp:defvar *error-map*
  '(("InternalServiceErrorException" . internal-service-error-exception)
    ("InvalidParameterException" . invalid-parameter-exception)
    ("ThrottlingException" . throttling-exception)))
(common-lisp:deftype boolean () 'common-lisp:boolean)
(common-lisp:deftype double () 'common-lisp:double-float)
(common-lisp:progn
 (common-lisp:defstruct
     (entitlement (:copier common-lisp:nil)
      (:conc-name "struct-shape-entitlement-"))
   (product-code common-lisp:nil :type
    (common-lisp:or product-code common-lisp:null))
   (dimension common-lisp:nil :type
    (common-lisp:or non-empty-string common-lisp:null))
   (customer-identifier common-lisp:nil :type
    (common-lisp:or non-empty-string common-lisp:null))
   (value common-lisp:nil :type
    (common-lisp:or entitlement-value common-lisp:null))
   (expiration-date common-lisp:nil :type
    (common-lisp:or timestamp common-lisp:null)))
 (common-lisp:export (common-lisp:list 'entitlement 'make-entitlement))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input entitlement))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input entitlement))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'product-code))
      (common-lisp:list
       (common-lisp:cons "ProductCode"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'dimension))
      (common-lisp:list
       (common-lisp:cons "Dimension"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'customer-identifier))
      (common-lisp:list
       (common-lisp:cons "CustomerIdentifier"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'value))
      (common-lisp:list
       (common-lisp:cons "Value"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'expiration-date))
      (common-lisp:list
       (common-lisp:cons "ExpirationDate"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input entitlement))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype entitlement-list ()
   '(trivial-types:proper-list entitlement))
 (common-lisp:defun make-entitlement-list
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list entitlement))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defstruct
     (entitlement-value (:copier common-lisp:nil)
      (:conc-name "struct-shape-entitlement-value-"))
   (integer-value common-lisp:nil :type
    (common-lisp:or integer common-lisp:null))
   (double-value common-lisp:nil :type
    (common-lisp:or double common-lisp:null))
   (boolean-value common-lisp:nil :type
    (common-lisp:or boolean common-lisp:null))
   (string-value common-lisp:nil :type
    (common-lisp:or string common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'entitlement-value 'make-entitlement-value))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input entitlement-value))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input entitlement-value))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'integer-value))
      (common-lisp:list
       (common-lisp:cons "IntegerValue"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'double-value))
      (common-lisp:list
       (common-lisp:cons "DoubleValue"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'boolean-value))
      (common-lisp:list
       (common-lisp:cons "BooleanValue"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'string-value))
      (common-lisp:list
       (common-lisp:cons "StringValue"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input entitlement-value))
   common-lisp:nil))
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:deftype filter-value () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype filter-value-list ()
   '(trivial-types:proper-list filter-value))
 (common-lisp:defun make-filter-value-list
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list filter-value))
   aws-sdk/generator/shape::members))
(common-lisp:deftype get-entitlement-filter-name () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype get-entitlement-filters () 'common-lisp:hash-table)
 (common-lisp:defun make-get-entitlement-filters
                    (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:progn
 (common-lisp:defstruct
     (get-entitlements-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-entitlements-request-"))
   (product-code (common-lisp:error ":product-code is required") :type
    (common-lisp:or product-code common-lisp:null))
   (filter common-lisp:nil :type
    (common-lisp:or get-entitlement-filters common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or non-empty-string common-lisp:null))
   (max-results common-lisp:nil :type
    (common-lisp:or page-size-integer common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-entitlements-request 'make-get-entitlements-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-entitlements-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-entitlements-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'product-code))
      (common-lisp:list
       (common-lisp:cons "ProductCode"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filter))
      (common-lisp:list
       (common-lisp:cons "Filter"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'next-token))
      (common-lisp:list
       (common-lisp:cons "NextToken"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'max-results))
      (common-lisp:list
       (common-lisp:cons "MaxResults"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-entitlements-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (get-entitlements-result (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-entitlements-result-"))
   (entitlements common-lisp:nil :type
    (common-lisp:or entitlement-list common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or non-empty-string common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-entitlements-result 'make-get-entitlements-result))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-entitlements-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-entitlements-result))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'entitlements))
      (common-lisp:list
       (common-lisp:cons "Entitlements"
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
                          get-entitlements-result))
   common-lisp:nil))
(common-lisp:deftype integer () 'common-lisp:integer)
(common-lisp:progn
 (common-lisp:define-condition internal-service-error-exception
     (entitlement.marketplace-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-service-error-exception-message)))
 (common-lisp:export
  (common-lisp:list 'internal-service-error-exception
                    'internal-service-error-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition invalid-parameter-exception
     (entitlement.marketplace-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-parameter-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-parameter-exception
                    'invalid-parameter-exception-message)))
(common-lisp:deftype non-empty-string () 'common-lisp:string)
(common-lisp:deftype page-size-integer () 'common-lisp:integer)
(common-lisp:deftype product-code () 'common-lisp:string)
(common-lisp:deftype string () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition throttling-exception
     (entitlement.marketplace-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       throttling-exception-message)))
 (common-lisp:export
  (common-lisp:list 'throttling-exception 'throttling-exception-message)))
(common-lisp:deftype timestamp () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun get-entitlements
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key product-code filter next-token
                     max-results)
   (common-lisp:declare
    (common-lisp:ignorable product-code filter next-token max-results))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-get-entitlements-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'entitlement.marketplace-request aws-sdk/generator/operation::input
        "POST" "/" "GetEntitlements"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'get-entitlements))
