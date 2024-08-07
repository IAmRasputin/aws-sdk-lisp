;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/marketplacecommerceanalytics/api
  (:use)
  (:nicknames #:aws/marketplacecommerceanalytics)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/marketplacecommerceanalytics/api)
(common-lisp:progn
 (common-lisp:defclass marketplacecommerceanalytics-request
                       (aws-sdk/request:request) common-lisp:nil
                       (:default-initargs :service
                        "marketplacecommerceanalytics" :protocol :json))
 (common-lisp:export 'marketplacecommerceanalytics-request))
(common-lisp:progn
 (common-lisp:define-condition marketplacecommerceanalytics-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'marketplacecommerceanalytics-error))
(common-lisp:defvar *error-map*
  '(("MarketplaceCommerceAnalyticsException"
     . marketplace-commerce-analytics-exception)))
(common-lisp:progn
 (common-lisp:deftype customer-defined-values () 'common-lisp:hash-table)
 (common-lisp:defun |make-customer-defined-values|
                    (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:deftype data-set-publication-date () 'common-lisp:string)
(common-lisp:deftype data-set-request-id () 'common-lisp:string)
(common-lisp:deftype data-set-type () 'common-lisp:string)
(common-lisp:deftype destination-s3bucket-name () 'common-lisp:string)
(common-lisp:deftype destination-s3prefix () 'common-lisp:string)
(common-lisp:deftype exception-message () 'common-lisp:string)
(common-lisp:deftype from-date () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass generate-data-set-request common-lisp:nil
                       ((customer-defined-values :initarg
                         :|customerDefinedValues| :type
                         (common-lisp:or customer-defined-values
                                         common-lisp:null)
                         :accessor
                         %generate-data-set-request-customer-defined-values
                         :initform common-lisp:nil)
                        (sns-topic-arn :initarg :|snsTopicArn| :type
                         (common-lisp:or sns-topic-arn common-lisp:null)
                         :accessor %generate-data-set-request-sns-topic-arn
                         :initform
                         (common-lisp:error ":snstopicarn is required"))
                        (destination-s3prefix :initarg :|destinationS3Prefix|
                         :type
                         (common-lisp:or destination-s3prefix common-lisp:null)
                         :accessor
                         %generate-data-set-request-destination-s3prefix
                         :initform common-lisp:nil)
                        (destination-s3bucket-name :initarg
                         :|destinationS3BucketName| :type
                         (common-lisp:or destination-s3bucket-name
                                         common-lisp:null)
                         :accessor
                         %generate-data-set-request-destination-s3bucket-name
                         :initform
                         (common-lisp:error
                          ":destinations3bucketname is required"))
                        (role-name-arn :initarg :|roleNameArn| :type
                         (common-lisp:or role-name-arn common-lisp:null)
                         :accessor %generate-data-set-request-role-name-arn
                         :initform
                         (common-lisp:error ":rolenamearn is required"))
                        (data-set-publication-date :initarg
                         :|dataSetPublicationDate| :type
                         (common-lisp:or data-set-publication-date
                                         common-lisp:null)
                         :accessor
                         %generate-data-set-request-data-set-publication-date
                         :initform
                         (common-lisp:error
                          ":datasetpublicationdate is required"))
                        (data-set-type :initarg :|dataSetType| :type
                         (common-lisp:or data-set-type common-lisp:null)
                         :accessor %generate-data-set-request-data-set-type
                         :initform
                         (common-lisp:error ":datasettype is required"))))
 (common-lisp:export
  (common-lisp:list 'generate-data-set-request
                    'make-generate-data-set-request))
 (common-lisp:defun make-generate-data-set-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key customer-defined-values sns-topic-arn
                     destination-s3prefix destination-s3bucket-name
                     role-name-arn data-set-publication-date data-set-type)
   (common-lisp:apply #'common-lisp:make-instance 'generate-data-set-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          generate-data-set-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          generate-data-set-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'data-set-type))
      (common-lisp:list
       (common-lisp:cons "dataSetType"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'data-set-publication-date))
      (common-lisp:list
       (common-lisp:cons "dataSetPublicationDate"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'role-name-arn))
      (common-lisp:list
       (common-lisp:cons "roleNameArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'destination-s3bucket-name))
      (common-lisp:list
       (common-lisp:cons "destinationS3BucketName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'destination-s3prefix))
      (common-lisp:list
       (common-lisp:cons "destinationS3Prefix"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'sns-topic-arn))
      (common-lisp:list
       (common-lisp:cons "snsTopicArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'customer-defined-values))
      (common-lisp:list
       (common-lisp:cons "customerDefinedValues"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          generate-data-set-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass generate-data-set-result common-lisp:nil
                       ((data-set-request-id :initarg :|dataSetRequestId| :type
                         (common-lisp:or data-set-request-id common-lisp:null)
                         :accessor
                         %generate-data-set-result-data-set-request-id
                         :initform common-lisp:nil)))
 (common-lisp:export
  (common-lisp:list 'generate-data-set-result 'make-generate-data-set-result))
 (common-lisp:defun make-generate-data-set-result
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key data-set-request-id)
   (common-lisp:apply #'common-lisp:make-instance 'generate-data-set-result
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          generate-data-set-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          generate-data-set-result))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'data-set-request-id))
      (common-lisp:list
       (common-lisp:cons "dataSetRequestId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          generate-data-set-result))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:define-condition marketplace-commerce-analytics-exception
     (marketplacecommerceanalytics-error)
     ((message :initarg :|message| :initform common-lisp:nil :reader
       marketplace-commerce-analytics-exception-message)))
 (common-lisp:export
  (common-lisp:list 'marketplace-commerce-analytics-exception
                    'marketplace-commerce-analytics-exception-message)))
(common-lisp:deftype optional-key () 'common-lisp:string)
(common-lisp:deftype optional-value () 'common-lisp:string)
(common-lisp:deftype role-name-arn () 'common-lisp:string)
(common-lisp:deftype sns-topic-arn () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass start-support-data-export-request common-lisp:nil
                       ((customer-defined-values :initarg
                         :|customerDefinedValues| :type
                         (common-lisp:or customer-defined-values
                                         common-lisp:null)
                         :accessor
                         %start-support-data-export-request-customer-defined-values
                         :initform common-lisp:nil)
                        (sns-topic-arn :initarg :|snsTopicArn| :type
                         (common-lisp:or sns-topic-arn common-lisp:null)
                         :accessor
                         %start-support-data-export-request-sns-topic-arn
                         :initform
                         (common-lisp:error ":snstopicarn is required"))
                        (destination-s3prefix :initarg :|destinationS3Prefix|
                         :type
                         (common-lisp:or destination-s3prefix common-lisp:null)
                         :accessor
                         %start-support-data-export-request-destination-s3prefix
                         :initform common-lisp:nil)
                        (destination-s3bucket-name :initarg
                         :|destinationS3BucketName| :type
                         (common-lisp:or destination-s3bucket-name
                                         common-lisp:null)
                         :accessor
                         %start-support-data-export-request-destination-s3bucket-name
                         :initform
                         (common-lisp:error
                          ":destinations3bucketname is required"))
                        (role-name-arn :initarg :|roleNameArn| :type
                         (common-lisp:or role-name-arn common-lisp:null)
                         :accessor
                         %start-support-data-export-request-role-name-arn
                         :initform
                         (common-lisp:error ":rolenamearn is required"))
                        (from-date :initarg :|fromDate| :type
                         (common-lisp:or from-date common-lisp:null) :accessor
                         %start-support-data-export-request-from-date :initform
                         (common-lisp:error ":fromdate is required"))
                        (data-set-type :initarg :|dataSetType| :type
                         (common-lisp:or support-data-set-type
                                         common-lisp:null)
                         :accessor
                         %start-support-data-export-request-data-set-type
                         :initform
                         (common-lisp:error ":datasettype is required"))))
 (common-lisp:export
  (common-lisp:list 'start-support-data-export-request
                    'make-start-support-data-export-request))
 (common-lisp:defun make-start-support-data-export-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key customer-defined-values sns-topic-arn
                     destination-s3prefix destination-s3bucket-name
                     role-name-arn from-date data-set-type)
   (common-lisp:apply #'common-lisp:make-instance
                      'start-support-data-export-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          start-support-data-export-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          start-support-data-export-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'data-set-type))
      (common-lisp:list
       (common-lisp:cons "dataSetType"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'from-date))
      (common-lisp:list
       (common-lisp:cons "fromDate"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'role-name-arn))
      (common-lisp:list
       (common-lisp:cons "roleNameArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'destination-s3bucket-name))
      (common-lisp:list
       (common-lisp:cons "destinationS3BucketName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'destination-s3prefix))
      (common-lisp:list
       (common-lisp:cons "destinationS3Prefix"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'sns-topic-arn))
      (common-lisp:list
       (common-lisp:cons "snsTopicArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'customer-defined-values))
      (common-lisp:list
       (common-lisp:cons "customerDefinedValues"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          start-support-data-export-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass start-support-data-export-result common-lisp:nil
                       ((data-set-request-id :initarg :|dataSetRequestId| :type
                         (common-lisp:or data-set-request-id common-lisp:null)
                         :accessor
                         %start-support-data-export-result-data-set-request-id
                         :initform common-lisp:nil)))
 (common-lisp:export
  (common-lisp:list 'start-support-data-export-result
                    'make-start-support-data-export-result))
 (common-lisp:defun make-start-support-data-export-result
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key data-set-request-id)
   (common-lisp:apply #'common-lisp:make-instance
                      'start-support-data-export-result
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          start-support-data-export-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          start-support-data-export-result))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'data-set-request-id))
      (common-lisp:list
       (common-lisp:cons "dataSetRequestId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          start-support-data-export-result))
   common-lisp:nil))
(common-lisp:deftype support-data-set-type () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun generate-data-set
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key data-set-type data-set-publication-date
                     role-name-arn destination-s3bucket-name
                     destination-s3prefix sns-topic-arn
                     customer-defined-values)
   (common-lisp:declare
    (common-lisp:ignorable data-set-type data-set-publication-date
     role-name-arn destination-s3bucket-name destination-s3prefix sns-topic-arn
     customer-defined-values))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-generate-data-set-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'marketplacecommerceanalytics-request
        aws-sdk/generator/operation::input "POST" :json "/" "GenerateDataSet"
        "2015-07-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'generate-data-set))
(common-lisp:progn
 (common-lisp:defun start-support-data-export
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key data-set-type from-date role-name-arn
                     destination-s3bucket-name destination-s3prefix
                     sns-topic-arn customer-defined-values)
   (common-lisp:declare
    (common-lisp:ignorable data-set-type from-date role-name-arn
     destination-s3bucket-name destination-s3prefix sns-topic-arn
     customer-defined-values))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply
                       'make-start-support-data-export-request
                       aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'marketplacecommerceanalytics-request
        aws-sdk/generator/operation::input "POST" :json "/"
        "StartSupportDataExport" "2015-07-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'start-support-data-export))
