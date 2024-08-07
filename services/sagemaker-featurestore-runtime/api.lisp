;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/sagemaker-featurestore-runtime/api
  (:use)
  (:nicknames #:aws/sagemaker-featurestore-runtime)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/sagemaker-featurestore-runtime/api)
(common-lisp:progn
 (common-lisp:defclass sagemaker-featurestore-runtime-request
                       (aws-sdk/request:request) common-lisp:nil
                       (:default-initargs :service
                        "sagemaker-featurestore-runtime" :protocol :rest-json))
 (common-lisp:export 'sagemaker-featurestore-runtime-request))
(common-lisp:progn
 (common-lisp:define-condition sagemaker-featurestore-runtime-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'sagemaker-featurestore-runtime-error))
(common-lisp:defvar *error-map*
  '(("AccessForbidden" . access-forbidden)
    ("InternalFailure" . internal-failure)
    ("ResourceNotFound" . resource-not-found)
    ("ServiceUnavailable" . service-unavailable)
    ("ValidationError" . validation-error)))
(common-lisp:progn
 (common-lisp:define-condition access-forbidden
     (sagemaker-featurestore-runtime-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       access-forbidden-message)))
 (common-lisp:export
  (common-lisp:list 'access-forbidden 'access-forbidden-message)))
(common-lisp:progn
 (common-lisp:defclass batch-get-record-error common-lisp:nil
                       ((error-message :initarg :error-message :type
                         (common-lisp:or message common-lisp:null) :accessor
                         %batch-get-record-error-error-message :initform
                         (common-lisp:error ":error-message is required"))
                        (error-code :initarg :error-code :type
                         (common-lisp:or value-as-string common-lisp:null)
                         :accessor %batch-get-record-error-error-code :initform
                         (common-lisp:error ":error-code is required"))
                        (record-identifier-value-as-string :initarg
                         :record-identifier-value-as-string :type
                         (common-lisp:or value-as-string common-lisp:null)
                         :accessor
                         %batch-get-record-error-record-identifier-value-as-string
                         :initform
                         (common-lisp:error
                          ":record-identifier-value-as-string is required"))
                        (feature-group-name :initarg :feature-group-name :type
                         (common-lisp:or value-as-string common-lisp:null)
                         :accessor %batch-get-record-error-feature-group-name
                         :initform
                         (common-lisp:error
                          ":feature-group-name is required"))))
 (common-lisp:export
  (common-lisp:list 'batch-get-record-error 'make-batch-get-record-error))
 (common-lisp:defun make-batch-get-record-error
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key error-message error-code
                     record-identifier-value-as-string feature-group-name)
   (common-lisp:apply #'common-lisp:make-instance 'batch-get-record-error
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-error))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-error))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'feature-group-name))
      (common-lisp:list
       (common-lisp:cons "FeatureGroupName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'record-identifier-value-as-string))
      (common-lisp:list
       (common-lisp:cons "RecordIdentifierValueAsString"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'error-code))
      (common-lisp:list
       (common-lisp:cons "ErrorCode"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'error-message))
      (common-lisp:list
       (common-lisp:cons "ErrorMessage"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-error))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype batch-get-record-errors ()
   '(trivial-types:proper-list batch-get-record-error))
 (common-lisp:defun |make-batch-get-record-errors|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list batch-get-record-error))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defclass batch-get-record-identifier common-lisp:nil
                       ((feature-names :initarg :feature-names :type
                         (common-lisp:or feature-names common-lisp:null)
                         :accessor %batch-get-record-identifier-feature-names
                         :initform common-lisp:nil)
                        (record-identifiers-value-as-string :initarg
                         :record-identifiers-value-as-string :type
                         (common-lisp:or record-identifiers common-lisp:null)
                         :accessor
                         %batch-get-record-identifier-record-identifiers-value-as-string
                         :initform
                         (common-lisp:error
                          ":record-identifiers-value-as-string is required"))
                        (feature-group-name :initarg :feature-group-name :type
                         (common-lisp:or feature-group-name common-lisp:null)
                         :accessor
                         %batch-get-record-identifier-feature-group-name
                         :initform
                         (common-lisp:error
                          ":feature-group-name is required"))))
 (common-lisp:export
  (common-lisp:list 'batch-get-record-identifier
                    'make-batch-get-record-identifier))
 (common-lisp:defun make-batch-get-record-identifier
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key feature-names
                     record-identifiers-value-as-string feature-group-name)
   (common-lisp:apply #'common-lisp:make-instance 'batch-get-record-identifier
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-identifier))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-identifier))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'feature-group-name))
      (common-lisp:list
       (common-lisp:cons "FeatureGroupName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'record-identifiers-value-as-string))
      (common-lisp:list
       (common-lisp:cons "RecordIdentifiersValueAsString"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'feature-names))
      (common-lisp:list
       (common-lisp:cons "FeatureNames"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-identifier))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype batch-get-record-identifiers ()
   '(trivial-types:proper-list batch-get-record-identifier))
 (common-lisp:defun |make-batch-get-record-identifiers|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list
                            batch-get-record-identifier))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defclass batch-get-record-request common-lisp:nil
                       ((identifiers :initarg :identifiers :type
                         (common-lisp:or batch-get-record-identifiers
                                         common-lisp:null)
                         :accessor %batch-get-record-request-identifiers
                         :initform
                         (common-lisp:error ":identifiers is required"))))
 (common-lisp:export
  (common-lisp:list 'batch-get-record-request 'make-batch-get-record-request))
 (common-lisp:defun make-batch-get-record-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key identifiers)
   (common-lisp:apply #'common-lisp:make-instance 'batch-get-record-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'identifiers))
      (common-lisp:list
       (common-lisp:cons "Identifiers"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass batch-get-record-response common-lisp:nil
                       ((unprocessed-identifiers :initarg
                         :unprocessed-identifiers :type
                         (common-lisp:or unprocessed-identifiers
                                         common-lisp:null)
                         :accessor
                         %batch-get-record-response-unprocessed-identifiers
                         :initform
                         (common-lisp:error
                          ":unprocessed-identifiers is required"))
                        (errors :initarg :errors :type
                         (common-lisp:or batch-get-record-errors
                                         common-lisp:null)
                         :accessor %batch-get-record-response-errors :initform
                         (common-lisp:error ":errors is required"))
                        (records :initarg :records :type
                         (common-lisp:or batch-get-record-result-details
                                         common-lisp:null)
                         :accessor %batch-get-record-response-records :initform
                         (common-lisp:error ":records is required"))))
 (common-lisp:export
  (common-lisp:list 'batch-get-record-response
                    'make-batch-get-record-response))
 (common-lisp:defun make-batch-get-record-response
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key unprocessed-identifiers errors records)
   (common-lisp:apply #'common-lisp:make-instance 'batch-get-record-response
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'records))
      (common-lisp:list
       (common-lisp:cons "Records"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'errors))
      (common-lisp:list
       (common-lisp:cons "Errors"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'unprocessed-identifiers))
      (common-lisp:list
       (common-lisp:cons "UnprocessedIdentifiers"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass batch-get-record-result-detail common-lisp:nil
                       ((record :initarg :record :type
                         (common-lisp:or record common-lisp:null) :accessor
                         %batch-get-record-result-detail-record :initform
                         (common-lisp:error ":record is required"))
                        (record-identifier-value-as-string :initarg
                         :record-identifier-value-as-string :type
                         (common-lisp:or value-as-string common-lisp:null)
                         :accessor
                         %batch-get-record-result-detail-record-identifier-value-as-string
                         :initform
                         (common-lisp:error
                          ":record-identifier-value-as-string is required"))
                        (feature-group-name :initarg :feature-group-name :type
                         (common-lisp:or value-as-string common-lisp:null)
                         :accessor
                         %batch-get-record-result-detail-feature-group-name
                         :initform
                         (common-lisp:error
                          ":feature-group-name is required"))))
 (common-lisp:export
  (common-lisp:list 'batch-get-record-result-detail
                    'make-batch-get-record-result-detail))
 (common-lisp:defun make-batch-get-record-result-detail
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key record record-identifier-value-as-string
                     feature-group-name)
   (common-lisp:apply #'common-lisp:make-instance
                      'batch-get-record-result-detail
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-result-detail))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-result-detail))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'feature-group-name))
      (common-lisp:list
       (common-lisp:cons "FeatureGroupName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'record-identifier-value-as-string))
      (common-lisp:list
       (common-lisp:cons "RecordIdentifierValueAsString"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'record))
      (common-lisp:list
       (common-lisp:cons "Record"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          batch-get-record-result-detail))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype batch-get-record-result-details ()
   '(trivial-types:proper-list batch-get-record-result-detail))
 (common-lisp:defun |make-batch-get-record-result-details|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list
                            batch-get-record-result-detail))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defclass delete-record-request common-lisp:nil
                       ((deletion-mode :initarg :deletion-mode :type
                         (common-lisp:or deletion-mode common-lisp:null)
                         :accessor %delete-record-request-deletion-mode
                         :initform common-lisp:nil)
                        (target-stores :initarg :target-stores :type
                         (common-lisp:or target-stores common-lisp:null)
                         :accessor %delete-record-request-target-stores
                         :initform common-lisp:nil)
                        (event-time :initarg :event-time :type
                         (common-lisp:or value-as-string common-lisp:null)
                         :accessor %delete-record-request-event-time :initform
                         (common-lisp:error ":event-time is required"))
                        (record-identifier-value-as-string :initarg
                         :record-identifier-value-as-string :type
                         (common-lisp:or value-as-string common-lisp:null)
                         :accessor
                         %delete-record-request-record-identifier-value-as-string
                         :initform
                         (common-lisp:error
                          ":record-identifier-value-as-string is required"))
                        (feature-group-name :initarg :feature-group-name :type
                         (common-lisp:or feature-group-name common-lisp:null)
                         :accessor %delete-record-request-feature-group-name
                         :initform
                         (common-lisp:error
                          ":feature-group-name is required"))))
 (common-lisp:export
  (common-lisp:list 'delete-record-request 'make-delete-record-request))
 (common-lisp:defun make-delete-record-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key deletion-mode target-stores event-time
                     record-identifier-value-as-string feature-group-name)
   (common-lisp:apply #'common-lisp:make-instance 'delete-record-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          delete-record-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          delete-record-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          delete-record-request))
   common-lisp:nil))
(common-lisp:deftype deletion-mode () 'common-lisp:string)
(common-lisp:deftype feature-group-name () 'common-lisp:string)
(common-lisp:deftype feature-name () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype feature-names ()
   '(trivial-types:proper-list feature-name))
 (common-lisp:defun |make-feature-names|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list feature-name))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defclass feature-value common-lisp:nil
                       ((value-as-string :initarg :value-as-string :type
                         (common-lisp:or value-as-string common-lisp:null)
                         :accessor %feature-value-value-as-string :initform
                         (common-lisp:error ":value-as-string is required"))
                        (feature-name :initarg :feature-name :type
                         (common-lisp:or feature-name common-lisp:null)
                         :accessor %feature-value-feature-name :initform
                         (common-lisp:error ":feature-name is required"))))
 (common-lisp:export (common-lisp:list 'feature-value 'make-feature-value))
 (common-lisp:defun make-feature-value
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key value-as-string feature-name)
   (common-lisp:apply #'common-lisp:make-instance 'feature-value
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input feature-value))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input feature-value))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'feature-name))
      (common-lisp:list
       (common-lisp:cons "FeatureName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'value-as-string))
      (common-lisp:list
       (common-lisp:cons "ValueAsString"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input feature-value))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass get-record-request common-lisp:nil
                       ((feature-names :initarg :feature-names :type
                         (common-lisp:or feature-names common-lisp:null)
                         :accessor %get-record-request-feature-names :initform
                         common-lisp:nil)
                        (record-identifier-value-as-string :initarg
                         :record-identifier-value-as-string :type
                         (common-lisp:or value-as-string common-lisp:null)
                         :accessor
                         %get-record-request-record-identifier-value-as-string
                         :initform
                         (common-lisp:error
                          ":record-identifier-value-as-string is required"))
                        (feature-group-name :initarg :feature-group-name :type
                         (common-lisp:or feature-group-name common-lisp:null)
                         :accessor %get-record-request-feature-group-name
                         :initform
                         (common-lisp:error
                          ":feature-group-name is required"))))
 (common-lisp:export
  (common-lisp:list 'get-record-request 'make-get-record-request))
 (common-lisp:defun make-get-record-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key feature-names
                     record-identifier-value-as-string feature-group-name)
   (common-lisp:apply #'common-lisp:make-instance 'get-record-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input get-record-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input get-record-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input get-record-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass get-record-response common-lisp:nil
                       ((record :initarg :record :type
                         (common-lisp:or record common-lisp:null) :accessor
                         %get-record-response-record :initform
                         common-lisp:nil)))
 (common-lisp:export
  (common-lisp:list 'get-record-response 'make-get-record-response))
 (common-lisp:defun make-get-record-response
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key record)
   (common-lisp:apply #'common-lisp:make-instance 'get-record-response
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input get-record-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input get-record-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'record))
      (common-lisp:list
       (common-lisp:cons "Record"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input get-record-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:define-condition internal-failure
     (sagemaker-featurestore-runtime-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-failure-message)))
 (common-lisp:export
  (common-lisp:list 'internal-failure 'internal-failure-message)))
(common-lisp:deftype message () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass put-record-request common-lisp:nil
                       ((target-stores :initarg :target-stores :type
                         (common-lisp:or target-stores common-lisp:null)
                         :accessor %put-record-request-target-stores :initform
                         common-lisp:nil)
                        (record :initarg :record :type
                         (common-lisp:or record common-lisp:null) :accessor
                         %put-record-request-record :initform
                         (common-lisp:error ":record is required"))
                        (feature-group-name :initarg :feature-group-name :type
                         (common-lisp:or feature-group-name common-lisp:null)
                         :accessor %put-record-request-feature-group-name
                         :initform
                         (common-lisp:error
                          ":feature-group-name is required"))))
 (common-lisp:export
  (common-lisp:list 'put-record-request 'make-put-record-request))
 (common-lisp:defun make-put-record-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key target-stores record feature-group-name)
   (common-lisp:apply #'common-lisp:make-instance 'put-record-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input put-record-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input put-record-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'record))
      (common-lisp:list
       (common-lisp:cons "Record"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'target-stores))
      (common-lisp:list
       (common-lisp:cons "TargetStores"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input put-record-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype record () '(trivial-types:proper-list feature-value))
 (common-lisp:defun |make-record|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list feature-value))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:deftype record-identifiers ()
   '(trivial-types:proper-list value-as-string))
 (common-lisp:defun |make-record-identifiers|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list value-as-string))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:define-condition resource-not-found
     (sagemaker-featurestore-runtime-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-not-found-message)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found 'resource-not-found-message)))
(common-lisp:progn
 (common-lisp:define-condition service-unavailable
     (sagemaker-featurestore-runtime-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       service-unavailable-message)))
 (common-lisp:export
  (common-lisp:list 'service-unavailable 'service-unavailable-message)))
(common-lisp:deftype target-store () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype target-stores ()
   '(trivial-types:proper-list target-store))
 (common-lisp:defun |make-target-stores|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list target-store))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:deftype unprocessed-identifiers ()
   '(trivial-types:proper-list batch-get-record-identifier))
 (common-lisp:defun |make-unprocessed-identifiers|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list
                            batch-get-record-identifier))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:define-condition validation-error
     (sagemaker-featurestore-runtime-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       validation-error-message)))
 (common-lisp:export
  (common-lisp:list 'validation-error 'validation-error-message)))
(common-lisp:deftype value-as-string () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun batch-get-record
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key identifiers)
   (common-lisp:declare (common-lisp:ignorable identifiers))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-batch-get-record-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'sagemaker-featurestore-runtime-request
        aws-sdk/generator/operation::input "POST" :rest-json "/BatchGetRecord"
        "BatchGetRecord" "2020-07-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'batch-get-record))
(common-lisp:progn
 (common-lisp:defun delete-record
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key feature-group-name
                     record-identifier-value-as-string event-time target-stores
                     deletion-mode)
   (common-lisp:declare
    (common-lisp:ignorable feature-group-name record-identifier-value-as-string
     event-time target-stores deletion-mode))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-delete-record-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'sagemaker-featurestore-runtime-request
        aws-sdk/generator/operation::input "DELETE" :rest-json
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/FeatureGroup/~A"
                              (quri.encode:url-encode
                               (common-lisp:slot-value
                                aws-sdk/generator/operation::input
                                'feature-group-name))))
        "DeleteRecord" "2020-07-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'delete-record))
(common-lisp:progn
 (common-lisp:defun get-record
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key feature-group-name
                     record-identifier-value-as-string feature-names)
   (common-lisp:declare
    (common-lisp:ignorable feature-group-name record-identifier-value-as-string
     feature-names))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-get-record-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'sagemaker-featurestore-runtime-request
        aws-sdk/generator/operation::input "GET" :rest-json
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/FeatureGroup/~A"
                              (quri.encode:url-encode
                               (common-lisp:slot-value
                                aws-sdk/generator/operation::input
                                'feature-group-name))))
        "GetRecord" "2020-07-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'get-record))
(common-lisp:progn
 (common-lisp:defun put-record
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key feature-group-name record target-stores)
   (common-lisp:declare
    (common-lisp:ignorable feature-group-name record target-stores))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-put-record-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'sagemaker-featurestore-runtime-request
        aws-sdk/generator/operation::input "PUT" :rest-json
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/FeatureGroup/~A"
                              (quri.encode:url-encode
                               (common-lisp:slot-value
                                aws-sdk/generator/operation::input
                                'feature-group-name))))
        "PutRecord" "2020-07-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'put-record))
