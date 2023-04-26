;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/sagemaker-metrics/api
  (:use)
  (:nicknames #:aws/sagemaker-metrics)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/sagemaker-metrics/api)
(common-lisp:progn
 (common-lisp:defclass sagemaker-metrics-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "sagemaker-metrics"))
 (common-lisp:export 'sagemaker-metrics-request))
(common-lisp:progn
 (common-lisp:define-condition sagemaker-metrics-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'sagemaker-metrics-error))
(common-lisp:defvar *error-map* 'common-lisp:nil)
(common-lisp:progn
 (common-lisp:defstruct
     (batch-put-metrics-error (:copier common-lisp:nil)
      (:conc-name "struct-shape-batch-put-metrics-error-"))
   (code common-lisp:nil :type
    (common-lisp:or put-metrics-error-code common-lisp:null))
   (metric-index common-lisp:nil :type
    (common-lisp:or integer common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'batch-put-metrics-error 'make-batch-put-metrics-error))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          batch-put-metrics-error))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          batch-put-metrics-error))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'code))
      (common-lisp:list
       (common-lisp:cons "Code"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'metric-index))
      (common-lisp:list
       (common-lisp:cons "MetricIndex"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          batch-put-metrics-error))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype batch-put-metrics-error-list ()
   '(trivial-types:proper-list batch-put-metrics-error))
 (common-lisp:defun |make-batch-put-metrics-error-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list batch-put-metrics-error))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defstruct
     (batch-put-metrics-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-batch-put-metrics-request-"))
   (trial-component-name
    (common-lisp:error ":trial-component-name is required") :type
    (common-lisp:or experiment-entity-name common-lisp:null))
   (metric-data (common-lisp:error ":metric-data is required") :type
    (common-lisp:or raw-metric-data-list common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'batch-put-metrics-request
                    'make-batch-put-metrics-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          batch-put-metrics-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          batch-put-metrics-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'trial-component-name))
      (common-lisp:list
       (common-lisp:cons "TrialComponentName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'metric-data))
      (common-lisp:list
       (common-lisp:cons "MetricData"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          batch-put-metrics-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (batch-put-metrics-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-batch-put-metrics-response-"))
   (errors common-lisp:nil :type
    (common-lisp:or batch-put-metrics-error-list common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'batch-put-metrics-response
                    'make-batch-put-metrics-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          batch-put-metrics-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          batch-put-metrics-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'errors))
      (common-lisp:list
       (common-lisp:cons "Errors"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          batch-put-metrics-response))
   common-lisp:nil))
(common-lisp:deftype double () 'common-lisp:double-float)
(common-lisp:deftype experiment-entity-name () 'common-lisp:string)
(common-lisp:deftype integer () 'common-lisp:integer)
(common-lisp:deftype metric-name () 'common-lisp:string)
(common-lisp:deftype put-metrics-error-code () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (raw-metric-data (:copier common-lisp:nil)
      (:conc-name "struct-shape-raw-metric-data-"))
   (metric-name (common-lisp:error ":metric-name is required") :type
    (common-lisp:or metric-name common-lisp:null))
   (timestamp (common-lisp:error ":timestamp is required") :type
    (common-lisp:or timestamp common-lisp:null))
   (step common-lisp:nil :type (common-lisp:or step common-lisp:null))
   (value (common-lisp:error ":value is required") :type
    (common-lisp:or double common-lisp:null)))
 (common-lisp:export (common-lisp:list 'raw-metric-data 'make-raw-metric-data))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input raw-metric-data))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input raw-metric-data))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'metric-name))
      (common-lisp:list
       (common-lisp:cons "MetricName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'timestamp))
      (common-lisp:list
       (common-lisp:cons "Timestamp"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'step))
      (common-lisp:list
       (common-lisp:cons "Step"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'value))
      (common-lisp:list
       (common-lisp:cons "Value"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input raw-metric-data))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype raw-metric-data-list ()
   '(trivial-types:proper-list raw-metric-data))
 (common-lisp:defun |make-raw-metric-data-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list raw-metric-data))
   aws-sdk/generator/shape::members))
(common-lisp:deftype step () 'common-lisp:integer)
(common-lisp:deftype timestamp () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun batch-put-metrics
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key trial-component-name metric-data)
   (common-lisp:declare
    (common-lisp:ignorable trial-component-name metric-data))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-batch-put-metrics-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'sagemaker-metrics-request aws-sdk/generator/operation::input "PUT"
        "/BatchPutMetrics" "BatchPutMetrics" "2022-09-30"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'batch-put-metrics))
