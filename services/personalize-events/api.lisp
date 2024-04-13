;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/personalize-events/api
  (:use)
  (:nicknames #:aws/personalize-events)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/personalize-events/api)
(common-lisp:progn
 (common-lisp:defclass personalize-events-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "personalize-events"
                        :protocol :rest-json))
 (common-lisp:export 'personalize-events-request))
(common-lisp:progn
 (common-lisp:define-condition personalize-events-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'personalize-events-error))
(common-lisp:defvar *error-map*
  '(("InvalidInputException" . invalid-input-exception)
    ("ResourceInUseException" . resource-in-use-exception)
    ("ResourceNotFoundException" . resource-not-found-exception)))
(common-lisp:deftype arn () 'common-lisp:string)
(common-lisp:deftype date () 'common-lisp:string)
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass event common-lisp:nil
                       ((metric-attribution :initarg :|metricAttribution| :type
                         (common-lisp:or metric-attribution common-lisp:null)
                         :accessor %event-metric-attribution :initform
                         common-lisp:nil)
                        (impression :initarg :|impression| :type
                         (common-lisp:or impression common-lisp:null) :accessor
                         %event-impression :initform common-lisp:nil)
                        (recommendation-id :initarg :|recommendationId| :type
                         (common-lisp:or recommendation-id common-lisp:null)
                         :accessor %event-recommendation-id :initform
                         common-lisp:nil)
                        (sent-at :initarg :|sentAt| :type
                         (common-lisp:or date common-lisp:null) :accessor
                         %event-sent-at :initform
                         (common-lisp:error ":sentat is required"))
                        (properties :initarg :|properties| :type
                         (common-lisp:or event-properties-json
                                         common-lisp:null)
                         :accessor %event-properties :initform common-lisp:nil)
                        (item-id :initarg :|itemId| :type
                         (common-lisp:or item-id common-lisp:null) :accessor
                         %event-item-id :initform common-lisp:nil)
                        (event-value :initarg :|eventValue| :type
                         (common-lisp:or float-type common-lisp:null) :accessor
                         %event-event-value :initform common-lisp:nil)
                        (event-type :initarg :|eventType| :type
                         (common-lisp:or string-type common-lisp:null)
                         :accessor %event-event-type :initform
                         (common-lisp:error ":eventtype is required"))
                        (event-id :initarg :|eventId| :type
                         (common-lisp:or string-type common-lisp:null)
                         :accessor %event-event-id :initform common-lisp:nil)))
 (common-lisp:export (common-lisp:list 'event 'make-event))
 (common-lisp:defun make-event
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key metric-attribution impression
                     recommendation-id sent-at properties item-id event-value
                     event-type event-id)
   (common-lisp:apply #'common-lisp:make-instance 'event
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input event))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input event))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'event-id))
      (common-lisp:list
       (common-lisp:cons "eventId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'event-type))
      (common-lisp:list
       (common-lisp:cons "eventType"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'event-value))
      (common-lisp:list
       (common-lisp:cons "eventValue"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'item-id))
      (common-lisp:list
       (common-lisp:cons "itemId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'properties))
      (common-lisp:list
       (common-lisp:cons "properties"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'sent-at))
      (common-lisp:list
       (common-lisp:cons "sentAt"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'recommendation-id))
      (common-lisp:list
       (common-lisp:cons "recommendationId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'impression))
      (common-lisp:list
       (common-lisp:cons "impression"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'metric-attribution))
      (common-lisp:list
       (common-lisp:cons "metricAttribution"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input event))
   common-lisp:nil))
(common-lisp:deftype event-attribution-source () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype event-list () '(trivial-types:proper-list event))
 (common-lisp:defun |make-event-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list event))
   aws-sdk/generator/shape::members))
(common-lisp:deftype event-properties-json () 'common-lisp:string)
(common-lisp:deftype float-type () 'common-lisp:single-float)
(common-lisp:progn
 (common-lisp:deftype impression () '(trivial-types:proper-list item-id))
 (common-lisp:defun |make-impression|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list item-id))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:define-condition invalid-input-exception
     (personalize-events-error)
     ((message :initarg :|message| :initform common-lisp:nil :reader
       invalid-input-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-input-exception 'invalid-input-exception-message)))
(common-lisp:progn
 (common-lisp:defclass item common-lisp:nil
                       ((properties :initarg :|properties| :type
                         (common-lisp:or item-properties common-lisp:null)
                         :accessor %item-properties :initform common-lisp:nil)
                        (item-id :initarg :|itemId| :type
                         (common-lisp:or string-type common-lisp:null)
                         :accessor %item-item-id :initform
                         (common-lisp:error ":itemid is required"))))
 (common-lisp:export (common-lisp:list 'item 'make-item))
 (common-lisp:defun make-item
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key properties item-id)
   (common-lisp:apply #'common-lisp:make-instance 'item
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input item))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input item))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'item-id))
      (common-lisp:list
       (common-lisp:cons "itemId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'properties))
      (common-lisp:list
       (common-lisp:cons "properties"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input item))
   common-lisp:nil))
(common-lisp:deftype item-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype item-list () '(trivial-types:proper-list item))
 (common-lisp:defun |make-item-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list item))
   aws-sdk/generator/shape::members))
(common-lisp:deftype item-properties () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass metric-attribution common-lisp:nil
                       ((event-attribution-source :initarg
                         :|eventAttributionSource| :type
                         (common-lisp:or event-attribution-source
                                         common-lisp:null)
                         :accessor %metric-attribution-event-attribution-source
                         :initform
                         (common-lisp:error
                          ":eventattributionsource is required"))))
 (common-lisp:export
  (common-lisp:list 'metric-attribution 'make-metric-attribution))
 (common-lisp:defun make-metric-attribution
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key event-attribution-source)
   (common-lisp:apply #'common-lisp:make-instance 'metric-attribution
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input metric-attribution))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input metric-attribution))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'event-attribution-source))
      (common-lisp:list
       (common-lisp:cons "eventAttributionSource"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input metric-attribution))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass put-events-request common-lisp:nil
                       ((event-list :initarg :|eventList| :type
                         (common-lisp:or event-list common-lisp:null) :accessor
                         %put-events-request-event-list :initform
                         (common-lisp:error ":eventlist is required"))
                        (session-id :initarg :|sessionId| :type
                         (common-lisp:or string-type common-lisp:null)
                         :accessor %put-events-request-session-id :initform
                         (common-lisp:error ":sessionid is required"))
                        (user-id :initarg :|userId| :type
                         (common-lisp:or user-id common-lisp:null) :accessor
                         %put-events-request-user-id :initform common-lisp:nil)
                        (tracking-id :initarg :|trackingId| :type
                         (common-lisp:or string-type common-lisp:null)
                         :accessor %put-events-request-tracking-id :initform
                         (common-lisp:error ":trackingid is required"))))
 (common-lisp:export
  (common-lisp:list 'put-events-request 'make-put-events-request))
 (common-lisp:defun make-put-events-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key event-list session-id user-id
                     tracking-id)
   (common-lisp:apply #'common-lisp:make-instance 'put-events-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input put-events-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input put-events-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'tracking-id))
      (common-lisp:list
       (common-lisp:cons "trackingId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'user-id))
      (common-lisp:list
       (common-lisp:cons "userId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'session-id))
      (common-lisp:list
       (common-lisp:cons "sessionId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'event-list))
      (common-lisp:list
       (common-lisp:cons "eventList"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input put-events-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass put-items-request common-lisp:nil
                       ((items :initarg :|items| :type
                         (common-lisp:or item-list common-lisp:null) :accessor
                         %put-items-request-items :initform
                         (common-lisp:error ":items is required"))
                        (dataset-arn :initarg :|datasetArn| :type
                         (common-lisp:or arn common-lisp:null) :accessor
                         %put-items-request-dataset-arn :initform
                         (common-lisp:error ":datasetarn is required"))))
 (common-lisp:export
  (common-lisp:list 'put-items-request 'make-put-items-request))
 (common-lisp:defun make-put-items-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key items dataset-arn)
   (common-lisp:apply #'common-lisp:make-instance 'put-items-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input put-items-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input put-items-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'dataset-arn))
      (common-lisp:list
       (common-lisp:cons "datasetArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'items))
      (common-lisp:list
       (common-lisp:cons "items"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input put-items-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass put-users-request common-lisp:nil
                       ((users :initarg :|users| :type
                         (common-lisp:or user-list common-lisp:null) :accessor
                         %put-users-request-users :initform
                         (common-lisp:error ":users is required"))
                        (dataset-arn :initarg :|datasetArn| :type
                         (common-lisp:or arn common-lisp:null) :accessor
                         %put-users-request-dataset-arn :initform
                         (common-lisp:error ":datasetarn is required"))))
 (common-lisp:export
  (common-lisp:list 'put-users-request 'make-put-users-request))
 (common-lisp:defun make-put-users-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key users dataset-arn)
   (common-lisp:apply #'common-lisp:make-instance 'put-users-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input put-users-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input put-users-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'dataset-arn))
      (common-lisp:list
       (common-lisp:cons "datasetArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'users))
      (common-lisp:list
       (common-lisp:cons "users"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input put-users-request))
   common-lisp:nil))
(common-lisp:deftype recommendation-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition resource-in-use-exception
     (personalize-events-error)
     ((message :initarg :|message| :initform common-lisp:nil :reader
       resource-in-use-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-in-use-exception
                    'resource-in-use-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition resource-not-found-exception
     (personalize-events-error)
     ((message :initarg :|message| :initform common-lisp:nil :reader
       resource-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found-exception
                    'resource-not-found-exception-message)))
(common-lisp:deftype string-type () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass user common-lisp:nil
                       ((properties :initarg :|properties| :type
                         (common-lisp:or user-properties common-lisp:null)
                         :accessor %user-properties :initform common-lisp:nil)
                        (user-id :initarg :|userId| :type
                         (common-lisp:or string-type common-lisp:null)
                         :accessor %user-user-id :initform
                         (common-lisp:error ":userid is required"))))
 (common-lisp:export (common-lisp:list 'user 'make-user))
 (common-lisp:defun make-user
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key properties user-id)
   (common-lisp:apply #'common-lisp:make-instance 'user
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input user))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input user))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'user-id))
      (common-lisp:list
       (common-lisp:cons "userId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'properties))
      (common-lisp:list
       (common-lisp:cons "properties"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input user))
   common-lisp:nil))
(common-lisp:deftype user-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype user-list () '(trivial-types:proper-list user))
 (common-lisp:defun |make-user-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list user))
   aws-sdk/generator/shape::members))
(common-lisp:deftype user-properties () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun put-events
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key tracking-id user-id session-id
                     event-list)
   (common-lisp:declare
    (common-lisp:ignorable tracking-id user-id session-id event-list))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-put-events-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'personalize-events-request aws-sdk/generator/operation::input "POST"
        :rest-json "/events" "PutEvents" "2018-03-22"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'put-events))
(common-lisp:progn
 (common-lisp:defun put-items
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key dataset-arn items)
   (common-lisp:declare (common-lisp:ignorable dataset-arn items))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-put-items-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'personalize-events-request aws-sdk/generator/operation::input "POST"
        :rest-json "/items" "PutItems" "2018-03-22"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'put-items))
(common-lisp:progn
 (common-lisp:defun put-users
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key dataset-arn users)
   (common-lisp:declare (common-lisp:ignorable dataset-arn users))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-put-users-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'personalize-events-request aws-sdk/generator/operation::input "POST"
        :rest-json "/users" "PutUsers" "2018-03-22"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'put-users))
