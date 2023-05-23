;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage
    #:aws-sdk/services/license-manager-linux-subscriptions/api
  (:use)
  (:nicknames #:aws/license-manager-linux-subscriptions)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package
 #:aws-sdk/services/license-manager-linux-subscriptions/api)
(common-lisp:progn
 (common-lisp:defclass license-manager-linux-subscriptions-request
                       (aws-sdk/request:request) common-lisp:nil
                       (:default-initargs :service
                        "license-manager-linux-subscriptions" :protocol
                        :rest-json))
 (common-lisp:export 'license-manager-linux-subscriptions-request))
(common-lisp:progn
 (common-lisp:define-condition license-manager-linux-subscriptions-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'license-manager-linux-subscriptions-error))
(common-lisp:defvar *error-map*
  '(("InternalServerException" . internal-server-exception)
    ("ThrottlingException" . throttling-exception)
    ("ValidationException" . validation-exception)))
(common-lisp:deftype boolean () 'common-lisp:boolean)
(common-lisp:deftype box-integer () 'common-lisp:integer)
(common-lisp:deftype box-long () 'common-lisp:integer)
(common-lisp:progn
 (common-lisp:defstruct
     (filter (:copier common-lisp:nil) (:conc-name "struct-shape-filter-"))
   (name common-lisp:nil :type (common-lisp:or string common-lisp:null))
   (operator common-lisp:nil :type (common-lisp:or operator common-lisp:null))
   (values common-lisp:nil :type
    (common-lisp:or string-list common-lisp:null)))
 (common-lisp:export (common-lisp:list 'filter 'make-filter))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input filter))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input filter))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'name))
      (common-lisp:list
       (common-lisp:cons "Name"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'operator))
      (common-lisp:list
       (common-lisp:cons "Operator"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'values))
      (common-lisp:list
       (common-lisp:cons "Values"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input filter))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype filter-list () '(trivial-types:proper-list filter))
 (common-lisp:defun |make-filter-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list filter))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defstruct
     (get-service-settings-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-service-settings-request-")))
 (common-lisp:export
  (common-lisp:list 'get-service-settings-request
                    'make-get-service-settings-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-service-settings-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-service-settings-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-service-settings-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (get-service-settings-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-service-settings-response-"))
   (home-regions common-lisp:nil :type
    (common-lisp:or string-list common-lisp:null))
   (linux-subscriptions-discovery common-lisp:nil :type
    (common-lisp:or linux-subscriptions-discovery common-lisp:null))
   (linux-subscriptions-discovery-settings common-lisp:nil :type
    (common-lisp:or linux-subscriptions-discovery-settings common-lisp:null))
   (status common-lisp:nil :type (common-lisp:or status common-lisp:null))
   (status-message common-lisp:nil :type
    (common-lisp:or string-map common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-service-settings-response
                    'make-get-service-settings-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-service-settings-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-service-settings-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'home-regions))
      (common-lisp:list
       (common-lisp:cons "HomeRegions"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'linux-subscriptions-discovery))
      (common-lisp:list
       (common-lisp:cons "LinuxSubscriptionsDiscovery"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'linux-subscriptions-discovery-settings))
      (common-lisp:list
       (common-lisp:cons "LinuxSubscriptionsDiscoverySettings"
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
                           aws-sdk/generator/shape::input 'status-message))
      (common-lisp:list
       (common-lisp:cons "StatusMessage"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-service-settings-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (instance (:copier common-lisp:nil) (:conc-name "struct-shape-instance-"))
   (account-id common-lisp:nil :type (common-lisp:or string common-lisp:null))
   (ami-id common-lisp:nil :type (common-lisp:or string common-lisp:null))
   (instance-id common-lisp:nil :type (common-lisp:or string common-lisp:null))
   (instance-type common-lisp:nil :type
    (common-lisp:or string common-lisp:null))
   (last-updated-time common-lisp:nil :type
    (common-lisp:or string common-lisp:null))
   (product-code common-lisp:nil :type
    (common-lisp:or product-code-list common-lisp:null))
   (region common-lisp:nil :type (common-lisp:or string common-lisp:null))
   (status common-lisp:nil :type (common-lisp:or string common-lisp:null))
   (subscription-name common-lisp:nil :type
    (common-lisp:or string common-lisp:null))
   (usage-operation common-lisp:nil :type
    (common-lisp:or string common-lisp:null)))
 (common-lisp:export (common-lisp:list 'instance 'make-instance))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input instance))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input instance))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'account-id))
      (common-lisp:list
       (common-lisp:cons "AccountID"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'ami-id))
      (common-lisp:list
       (common-lisp:cons "AmiId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'instance-id))
      (common-lisp:list
       (common-lisp:cons "InstanceID"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'instance-type))
      (common-lisp:list
       (common-lisp:cons "InstanceType"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'last-updated-time))
      (common-lisp:list
       (common-lisp:cons "LastUpdatedTime"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'product-code))
      (common-lisp:list
       (common-lisp:cons "ProductCode"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'region))
      (common-lisp:list
       (common-lisp:cons "Region"
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
                           aws-sdk/generator/shape::input 'subscription-name))
      (common-lisp:list
       (common-lisp:cons "SubscriptionName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'usage-operation))
      (common-lisp:list
       (common-lisp:cons "UsageOperation"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input instance))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype instance-list () '(trivial-types:proper-list instance))
 (common-lisp:defun |make-instance-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list instance))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:define-condition internal-server-exception
     (license-manager-linux-subscriptions-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-server-exception-message)))
 (common-lisp:export
  (common-lisp:list 'internal-server-exception
                    'internal-server-exception-message)))
(common-lisp:deftype linux-subscriptions-discovery () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (linux-subscriptions-discovery-settings (:copier common-lisp:nil)
      (:conc-name "struct-shape-linux-subscriptions-discovery-settings-"))
   (organization-integration
    (common-lisp:error ":organization-integration is required") :type
    (common-lisp:or organization-integration common-lisp:null))
   (source-regions (common-lisp:error ":source-regions is required") :type
    (common-lisp:or string-list common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'linux-subscriptions-discovery-settings
                    'make-linux-subscriptions-discovery-settings))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          linux-subscriptions-discovery-settings))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          linux-subscriptions-discovery-settings))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'organization-integration))
      (common-lisp:list
       (common-lisp:cons "OrganizationIntegration"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'source-regions))
      (common-lisp:list
       (common-lisp:cons "SourceRegions"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          linux-subscriptions-discovery-settings))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (list-linux-subscription-instances-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-linux-subscription-instances-request-"))
   (filters common-lisp:nil :type
    (common-lisp:or filter-list common-lisp:null))
   (max-results common-lisp:nil :type
    (common-lisp:or box-integer common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or list-linux-subscription-instances-request-next-token-string
                    common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-linux-subscription-instances-request
                    'make-list-linux-subscription-instances-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-linux-subscription-instances-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-linux-subscription-instances-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filters))
      (common-lisp:list
       (common-lisp:cons "Filters"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'max-results))
      (common-lisp:list
       (common-lisp:cons "MaxResults"
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
                          list-linux-subscription-instances-request))
   common-lisp:nil))
(common-lisp:deftype list-linux-subscription-instances-request-next-token-string
                     ()
  'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (list-linux-subscription-instances-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-linux-subscription-instances-response-"))
   (instances common-lisp:nil :type
    (common-lisp:or instance-list common-lisp:null))
   (next-token common-lisp:nil :type (common-lisp:or string common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-linux-subscription-instances-response
                    'make-list-linux-subscription-instances-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-linux-subscription-instances-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-linux-subscription-instances-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'instances))
      (common-lisp:list
       (common-lisp:cons "Instances"
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
                          list-linux-subscription-instances-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (list-linux-subscriptions-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-linux-subscriptions-request-"))
   (filters common-lisp:nil :type
    (common-lisp:or filter-list common-lisp:null))
   (max-results common-lisp:nil :type
    (common-lisp:or box-integer common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or list-linux-subscriptions-request-next-token-string
                    common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-linux-subscriptions-request
                    'make-list-linux-subscriptions-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-linux-subscriptions-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-linux-subscriptions-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filters))
      (common-lisp:list
       (common-lisp:cons "Filters"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'max-results))
      (common-lisp:list
       (common-lisp:cons "MaxResults"
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
                          list-linux-subscriptions-request))
   common-lisp:nil))
(common-lisp:deftype list-linux-subscriptions-request-next-token-string ()
  'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (list-linux-subscriptions-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-linux-subscriptions-response-"))
   (next-token common-lisp:nil :type (common-lisp:or string common-lisp:null))
   (subscriptions common-lisp:nil :type
    (common-lisp:or subscription-list common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-linux-subscriptions-response
                    'make-list-linux-subscriptions-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-linux-subscriptions-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-linux-subscriptions-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'next-token))
      (common-lisp:list
       (common-lisp:cons "NextToken"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'subscriptions))
      (common-lisp:list
       (common-lisp:cons "Subscriptions"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          list-linux-subscriptions-response))
   common-lisp:nil))
(common-lisp:deftype operator () 'common-lisp:string)
(common-lisp:deftype organization-integration () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype product-code-list () '(trivial-types:proper-list string))
 (common-lisp:defun |make-product-code-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list string))
   aws-sdk/generator/shape::members))
(common-lisp:deftype status () 'common-lisp:string)
(common-lisp:deftype string () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype string-list ()
   '(trivial-types:proper-list string-list-member-string))
 (common-lisp:defun |make-string-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list
                            string-list-member-string))
   aws-sdk/generator/shape::members))
(common-lisp:deftype string-list-member-string () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype string-map () 'common-lisp:hash-table)
 (common-lisp:defun |make-string-map| (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:progn
 (common-lisp:defstruct
     (subscription (:copier common-lisp:nil)
      (:conc-name "struct-shape-subscription-"))
   (instance-count common-lisp:nil :type
    (common-lisp:or box-long common-lisp:null))
   (name common-lisp:nil :type (common-lisp:or string common-lisp:null))
   (type common-lisp:nil :type (common-lisp:or string common-lisp:null)))
 (common-lisp:export (common-lisp:list 'subscription 'make-subscription))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input subscription))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input subscription))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'instance-count))
      (common-lisp:list
       (common-lisp:cons "InstanceCount"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'name))
      (common-lisp:list
       (common-lisp:cons "Name"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'type))
      (common-lisp:list
       (common-lisp:cons "Type"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input subscription))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype subscription-list ()
   '(trivial-types:proper-list subscription))
 (common-lisp:defun |make-subscription-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list subscription))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:define-condition throttling-exception
     (license-manager-linux-subscriptions-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       throttling-exception-message)))
 (common-lisp:export
  (common-lisp:list 'throttling-exception 'throttling-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (update-service-settings-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-update-service-settings-request-"))
   (allow-update common-lisp:nil :type
    (common-lisp:or boolean common-lisp:null))
   (linux-subscriptions-discovery
    (common-lisp:error ":linux-subscriptions-discovery is required") :type
    (common-lisp:or linux-subscriptions-discovery common-lisp:null))
   (linux-subscriptions-discovery-settings
    (common-lisp:error ":linux-subscriptions-discovery-settings is required")
    :type
    (common-lisp:or linux-subscriptions-discovery-settings common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'update-service-settings-request
                    'make-update-service-settings-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          update-service-settings-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          update-service-settings-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'allow-update))
      (common-lisp:list
       (common-lisp:cons "AllowUpdate"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'linux-subscriptions-discovery))
      (common-lisp:list
       (common-lisp:cons "LinuxSubscriptionsDiscovery"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'linux-subscriptions-discovery-settings))
      (common-lisp:list
       (common-lisp:cons "LinuxSubscriptionsDiscoverySettings"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          update-service-settings-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (update-service-settings-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-update-service-settings-response-"))
   (home-regions common-lisp:nil :type
    (common-lisp:or string-list common-lisp:null))
   (linux-subscriptions-discovery common-lisp:nil :type
    (common-lisp:or linux-subscriptions-discovery common-lisp:null))
   (linux-subscriptions-discovery-settings common-lisp:nil :type
    (common-lisp:or linux-subscriptions-discovery-settings common-lisp:null))
   (status common-lisp:nil :type (common-lisp:or status common-lisp:null))
   (status-message common-lisp:nil :type
    (common-lisp:or string-map common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'update-service-settings-response
                    'make-update-service-settings-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          update-service-settings-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          update-service-settings-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'home-regions))
      (common-lisp:list
       (common-lisp:cons "HomeRegions"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'linux-subscriptions-discovery))
      (common-lisp:list
       (common-lisp:cons "LinuxSubscriptionsDiscovery"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'linux-subscriptions-discovery-settings))
      (common-lisp:list
       (common-lisp:cons "LinuxSubscriptionsDiscoverySettings"
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
                           aws-sdk/generator/shape::input 'status-message))
      (common-lisp:list
       (common-lisp:cons "StatusMessage"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          update-service-settings-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:define-condition validation-exception
     (license-manager-linux-subscriptions-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       validation-exception-message)))
 (common-lisp:export
  (common-lisp:list 'validation-exception 'validation-exception-message)))
(common-lisp:progn
 (common-lisp:defun get-service-settings ()
   (aws-sdk/generator/operation::parse-response
    (aws-sdk/api:aws-request
     (common-lisp:make-instance 'license-manager-linux-subscriptions-request
                                :method "POST" :path
                                "/subscription/GetServiceSettings" :protocol
                                :rest-json :operation "GetServiceSettings"
                                :params
                                `(("Action" ,@"GetServiceSettings")
                                  ("Version" ,@"2018-05-10"))))
    common-lisp:nil common-lisp:nil *error-map*))
 (common-lisp:export 'get-service-settings))
(common-lisp:progn
 (common-lisp:defun list-linux-subscription-instances
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key filters max-results next-token)
   (common-lisp:declare (common-lisp:ignorable filters max-results next-token))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply
                       'make-list-linux-subscription-instances-request
                       aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'license-manager-linux-subscriptions-request
        aws-sdk/generator/operation::input "POST" :rest-json
        "/subscription/ListLinuxSubscriptionInstances"
        "ListLinuxSubscriptionInstances" "2018-05-10"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'list-linux-subscription-instances))
(common-lisp:progn
 (common-lisp:defun list-linux-subscriptions
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key filters max-results next-token)
   (common-lisp:declare (common-lisp:ignorable filters max-results next-token))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-list-linux-subscriptions-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'license-manager-linux-subscriptions-request
        aws-sdk/generator/operation::input "POST" :rest-json
        "/subscription/ListLinuxSubscriptions" "ListLinuxSubscriptions"
        "2018-05-10"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'list-linux-subscriptions))
(common-lisp:progn
 (common-lisp:defun update-service-settings
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key allow-update
                     linux-subscriptions-discovery
                     linux-subscriptions-discovery-settings)
   (common-lisp:declare
    (common-lisp:ignorable allow-update linux-subscriptions-discovery
     linux-subscriptions-discovery-settings))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-update-service-settings-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'license-manager-linux-subscriptions-request
        aws-sdk/generator/operation::input "POST" :rest-json
        "/subscription/UpdateServiceSettings" "UpdateServiceSettings"
        "2018-05-10"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'update-service-settings))
