;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/forecastquery/api
  (:use)
  (:nicknames #:aws/forecastquery)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/forecastquery/api)
(common-lisp:progn
 (common-lisp:defclass forecastquery-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "forecastquery" :protocol
                        :json))
 (common-lisp:export 'forecastquery-request))
(common-lisp:progn
 (common-lisp:define-condition forecastquery-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'forecastquery-error))
(common-lisp:defvar *error-map*
  '(("InvalidInputException" . invalid-input-exception)
    ("InvalidNextTokenException" . invalid-next-token-exception)
    ("LimitExceededException" . limit-exceeded-exception)
    ("ResourceInUseException" . resource-in-use-exception)
    ("ResourceNotFoundException" . resource-not-found-exception)))
(common-lisp:deftype arn () 'common-lisp:string)
(common-lisp:deftype attribute-name () 'common-lisp:string)
(common-lisp:deftype attribute-value () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (data-point (:copier common-lisp:nil)
      (:conc-name "struct-shape-data-point-"))
   (timestamp common-lisp:nil :type
    (common-lisp:or timestamp common-lisp:null))
   (value common-lisp:nil :type (common-lisp:or double common-lisp:null)))
 (common-lisp:export (common-lisp:list 'data-point 'make-data-point))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input data-point))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input data-point))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'timestamp))
      (common-lisp:list
       (common-lisp:cons "Timestamp"
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
                        ((aws-sdk/generator/shape::input data-point))
   common-lisp:nil))
(common-lisp:deftype date-time () 'common-lisp:string)
(common-lisp:deftype double () 'common-lisp:double-float)
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype filters () 'common-lisp:hash-table)
 (common-lisp:defun |make-filters| (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:progn
 (common-lisp:defstruct
     (forecast (:copier common-lisp:nil) (:conc-name "struct-shape-forecast-"))
   (predictions common-lisp:nil :type
    (common-lisp:or predictions common-lisp:null)))
 (common-lisp:export (common-lisp:list 'forecast 'make-forecast))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input forecast))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input forecast))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'predictions))
      (common-lisp:list
       (common-lisp:cons "Predictions"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input forecast))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:define-condition invalid-input-exception
     (forecastquery-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-input-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-input-exception 'invalid-input-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition invalid-next-token-exception
     (forecastquery-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-next-token-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-next-token-exception
                    'invalid-next-token-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition limit-exceeded-exception
     (forecastquery-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       limit-exceeded-exception-message)))
 (common-lisp:export
  (common-lisp:list 'limit-exceeded-exception
                    'limit-exceeded-exception-message)))
(common-lisp:deftype long-arn () 'common-lisp:string)
(common-lisp:deftype next-token () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype predictions () 'common-lisp:hash-table)
 (common-lisp:defun |make-predictions| (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:progn
 (common-lisp:defstruct
     (query-forecast-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-query-forecast-request-"))
   (forecast-arn (common-lisp:error ":forecast-arn is required") :type
    (common-lisp:or arn common-lisp:null))
   (start-date common-lisp:nil :type
    (common-lisp:or date-time common-lisp:null))
   (end-date common-lisp:nil :type (common-lisp:or date-time common-lisp:null))
   (filters (common-lisp:error ":filters is required") :type
    (common-lisp:or filters common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or next-token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'query-forecast-request 'make-query-forecast-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          query-forecast-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          query-forecast-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'forecast-arn))
      (common-lisp:list
       (common-lisp:cons "ForecastArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'start-date))
      (common-lisp:list
       (common-lisp:cons "StartDate"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'end-date))
      (common-lisp:list
       (common-lisp:cons "EndDate"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filters))
      (common-lisp:list
       (common-lisp:cons "Filters"
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
                          query-forecast-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (query-forecast-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-query-forecast-response-"))
   (forecast common-lisp:nil :type (common-lisp:or forecast common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'query-forecast-response 'make-query-forecast-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          query-forecast-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          query-forecast-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'forecast))
      (common-lisp:list
       (common-lisp:cons "Forecast"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          query-forecast-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (query-what-if-forecast-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-query-what-if-forecast-request-"))
   (what-if-forecast-arn
    (common-lisp:error ":what-if-forecast-arn is required") :type
    (common-lisp:or long-arn common-lisp:null))
   (start-date common-lisp:nil :type
    (common-lisp:or date-time common-lisp:null))
   (end-date common-lisp:nil :type (common-lisp:or date-time common-lisp:null))
   (filters (common-lisp:error ":filters is required") :type
    (common-lisp:or filters common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or next-token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'query-what-if-forecast-request
                    'make-query-what-if-forecast-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          query-what-if-forecast-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          query-what-if-forecast-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'what-if-forecast-arn))
      (common-lisp:list
       (common-lisp:cons "WhatIfForecastArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'start-date))
      (common-lisp:list
       (common-lisp:cons "StartDate"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'end-date))
      (common-lisp:list
       (common-lisp:cons "EndDate"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filters))
      (common-lisp:list
       (common-lisp:cons "Filters"
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
                          query-what-if-forecast-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (query-what-if-forecast-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-query-what-if-forecast-response-"))
   (forecast common-lisp:nil :type (common-lisp:or forecast common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'query-what-if-forecast-response
                    'make-query-what-if-forecast-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          query-what-if-forecast-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          query-what-if-forecast-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'forecast))
      (common-lisp:list
       (common-lisp:cons "Forecast"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          query-what-if-forecast-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:define-condition resource-in-use-exception
     (forecastquery-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-in-use-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-in-use-exception
                    'resource-in-use-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition resource-not-found-exception
     (forecastquery-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found-exception
                    'resource-not-found-exception-message)))
(common-lisp:deftype statistic () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype time-series () '(trivial-types:proper-list data-point))
 (common-lisp:defun |make-time-series|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list data-point))
   aws-sdk/generator/shape::members))
(common-lisp:deftype timestamp () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun query-forecast
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key forecast-arn start-date end-date filters
                     next-token)
   (common-lisp:declare
    (common-lisp:ignorable forecast-arn start-date end-date filters
     next-token))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-query-forecast-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 'forecastquery-request
                                                        aws-sdk/generator/operation::input
                                                        "POST" :json "/"
                                                        "QueryForecast"
                                                        "2018-06-26"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'query-forecast))
(common-lisp:progn
 (common-lisp:defun query-what-if-forecast
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key what-if-forecast-arn start-date end-date
                     filters next-token)
   (common-lisp:declare
    (common-lisp:ignorable what-if-forecast-arn start-date end-date filters
     next-token))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-query-what-if-forecast-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 'forecastquery-request
                                                        aws-sdk/generator/operation::input
                                                        "POST" :json "/"
                                                        "QueryWhatIfForecast"
                                                        "2018-06-26"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'query-what-if-forecast))
