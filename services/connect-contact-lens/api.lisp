;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/connect-contact-lens/api
  (:use)
  (:nicknames #:aws/connect-contact-lens)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/connect-contact-lens/api)
(common-lisp:progn
 (common-lisp:defclass connect-contact-lens-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "connect-contact-lens"))
 (common-lisp:export 'connect-contact-lens-request))
(common-lisp:progn
 (common-lisp:define-condition connect-contact-lens-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'connect-contact-lens-error))
(common-lisp:defvar *error-map*
  '(("AccessDeniedException" . access-denied-exception)
    ("InternalServiceException" . internal-service-exception)
    ("InvalidRequestException" . invalid-request-exception)
    ("ResourceNotFoundException" . resource-not-found-exception)
    ("ThrottlingException" . throttling-exception)))
(common-lisp:progn
 (common-lisp:define-condition access-denied-exception
     (connect-contact-lens-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       access-denied-exception-message)))
 (common-lisp:export
  (common-lisp:list 'access-denied-exception 'access-denied-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (categories (:copier common-lisp:nil)
      (:conc-name "struct-shape-categories-"))
   (matched-categories (common-lisp:error ":matched-categories is required")
    :type (common-lisp:or matched-categories common-lisp:null))
   (matched-details (common-lisp:error ":matched-details is required") :type
    (common-lisp:or matched-details common-lisp:null)))
 (common-lisp:export (common-lisp:list 'categories 'make-categories))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input categories))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input categories))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'matched-categories))
      (common-lisp:list
       (common-lisp:cons "MatchedCategories"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'matched-details))
      (common-lisp:list
       (common-lisp:cons "MatchedDetails"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input categories))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (category-details (:copier common-lisp:nil)
      (:conc-name "struct-shape-category-details-"))
   (points-of-interest (common-lisp:error ":points-of-interest is required")
    :type (common-lisp:or points-of-interest common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'category-details 'make-category-details))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input category-details))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input category-details))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'points-of-interest))
      (common-lisp:list
       (common-lisp:cons "PointsOfInterest"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input category-details))
   common-lisp:nil))
(common-lisp:deftype category-name () 'common-lisp:string)
(common-lisp:deftype character-offset () 'common-lisp:integer)
(common-lisp:progn
 (common-lisp:defstruct
     (character-offsets (:copier common-lisp:nil)
      (:conc-name "struct-shape-character-offsets-"))
   (begin-offset-char (common-lisp:error ":begin-offset-char is required")
    :type (common-lisp:or character-offset common-lisp:null))
   (end-offset-char (common-lisp:error ":end-offset-char is required") :type
    (common-lisp:or character-offset common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'character-offsets 'make-character-offsets))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input character-offsets))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input character-offsets))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'begin-offset-char))
      (common-lisp:list
       (common-lisp:cons "BeginOffsetChar"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'end-offset-char))
      (common-lisp:list
       (common-lisp:cons "EndOffsetChar"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input character-offsets))
   common-lisp:nil))
(common-lisp:deftype contact-id () 'common-lisp:string)
(common-lisp:deftype instance-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition internal-service-exception
     (connect-contact-lens-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-service-exception-message)))
 (common-lisp:export
  (common-lisp:list 'internal-service-exception
                    'internal-service-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition invalid-request-exception
     (connect-contact-lens-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-request-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-request-exception
                    'invalid-request-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (issue-detected (:copier common-lisp:nil)
      (:conc-name "struct-shape-issue-detected-"))
   (character-offsets (common-lisp:error ":character-offsets is required")
    :type (common-lisp:or character-offsets common-lisp:null)))
 (common-lisp:export (common-lisp:list 'issue-detected 'make-issue-detected))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input issue-detected))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input issue-detected))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'character-offsets))
      (common-lisp:list
       (common-lisp:cons "CharacterOffsets"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input issue-detected))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype issues-detected ()
   '(trivial-types:proper-list issue-detected))
 (common-lisp:defun |make-issues-detected|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list issue-detected))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defstruct
     (list-realtime-contact-analysis-segments-request (:copier common-lisp:nil)
      (:conc-name
       "struct-shape-list-realtime-contact-analysis-segments-request-"))
   (instance-id (common-lisp:error ":instance-id is required") :type
    (common-lisp:or instance-id common-lisp:null))
   (contact-id (common-lisp:error ":contact-id is required") :type
    (common-lisp:or contact-id common-lisp:null))
   (max-results common-lisp:nil :type
    (common-lisp:or max-results common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or next-token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-realtime-contact-analysis-segments-request
                    'make-list-realtime-contact-analysis-segments-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-realtime-contact-analysis-segments-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-realtime-contact-analysis-segments-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'instance-id))
      (common-lisp:list
       (common-lisp:cons "InstanceId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'contact-id))
      (common-lisp:list
       (common-lisp:cons "ContactId"
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
                          list-realtime-contact-analysis-segments-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (list-realtime-contact-analysis-segments-response
      (:copier common-lisp:nil)
      (:conc-name
       "struct-shape-list-realtime-contact-analysis-segments-response-"))
   (segments (common-lisp:error ":segments is required") :type
    (common-lisp:or realtime-contact-analysis-segments common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or next-token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-realtime-contact-analysis-segments-response
                    'make-list-realtime-contact-analysis-segments-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          list-realtime-contact-analysis-segments-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          list-realtime-contact-analysis-segments-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'segments))
      (common-lisp:list
       (common-lisp:cons "Segments"
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
                          list-realtime-contact-analysis-segments-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype matched-categories ()
   '(trivial-types:proper-list category-name))
 (common-lisp:defun |make-matched-categories|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list category-name))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:deftype matched-details () 'common-lisp:hash-table)
 (common-lisp:defun |make-matched-details|
                    (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:deftype max-results () 'common-lisp:integer)
(common-lisp:deftype message () 'common-lisp:string)
(common-lisp:deftype next-token () 'common-lisp:string)
(common-lisp:deftype offset-millis () 'common-lisp:integer)
(common-lisp:deftype participant-id () 'common-lisp:string)
(common-lisp:deftype participant-role () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (point-of-interest (:copier common-lisp:nil)
      (:conc-name "struct-shape-point-of-interest-"))
   (begin-offset-millis (common-lisp:error ":begin-offset-millis is required")
    :type (common-lisp:or offset-millis common-lisp:null))
   (end-offset-millis (common-lisp:error ":end-offset-millis is required")
    :type (common-lisp:or offset-millis common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'point-of-interest 'make-point-of-interest))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input point-of-interest))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input point-of-interest))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'begin-offset-millis))
      (common-lisp:list
       (common-lisp:cons "BeginOffsetMillis"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'end-offset-millis))
      (common-lisp:list
       (common-lisp:cons "EndOffsetMillis"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input point-of-interest))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype points-of-interest ()
   '(trivial-types:proper-list point-of-interest))
 (common-lisp:defun |make-points-of-interest|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list point-of-interest))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defstruct
     (realtime-contact-analysis-segment (:copier common-lisp:nil)
      (:conc-name "struct-shape-realtime-contact-analysis-segment-"))
   (transcript common-lisp:nil :type
    (common-lisp:or transcript common-lisp:null))
   (categories common-lisp:nil :type
    (common-lisp:or categories common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'realtime-contact-analysis-segment
                    'make-realtime-contact-analysis-segment))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          realtime-contact-analysis-segment))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          realtime-contact-analysis-segment))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'transcript))
      (common-lisp:list
       (common-lisp:cons "Transcript"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'categories))
      (common-lisp:list
       (common-lisp:cons "Categories"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          realtime-contact-analysis-segment))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype realtime-contact-analysis-segments ()
   '(trivial-types:proper-list realtime-contact-analysis-segment))
 (common-lisp:defun |make-realtime-contact-analysis-segments|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list
                            realtime-contact-analysis-segment))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:define-condition resource-not-found-exception
     (connect-contact-lens-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found-exception
                    'resource-not-found-exception-message)))
(common-lisp:deftype sentiment-value () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition throttling-exception
     (connect-contact-lens-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       throttling-exception-message)))
 (common-lisp:export
  (common-lisp:list 'throttling-exception 'throttling-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (transcript (:copier common-lisp:nil)
      (:conc-name "struct-shape-transcript-"))
   (id (common-lisp:error ":id is required") :type
    (common-lisp:or transcript-id common-lisp:null))
   (participant-id (common-lisp:error ":participant-id is required") :type
    (common-lisp:or participant-id common-lisp:null))
   (participant-role (common-lisp:error ":participant-role is required") :type
    (common-lisp:or participant-role common-lisp:null))
   (content (common-lisp:error ":content is required") :type
    (common-lisp:or transcript-content common-lisp:null))
   (begin-offset-millis (common-lisp:error ":begin-offset-millis is required")
    :type (common-lisp:or offset-millis common-lisp:null))
   (end-offset-millis (common-lisp:error ":end-offset-millis is required")
    :type (common-lisp:or offset-millis common-lisp:null))
   (sentiment (common-lisp:error ":sentiment is required") :type
    (common-lisp:or sentiment-value common-lisp:null))
   (issues-detected common-lisp:nil :type
    (common-lisp:or issues-detected common-lisp:null)))
 (common-lisp:export (common-lisp:list 'transcript 'make-transcript))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input transcript))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input transcript))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'id))
      (common-lisp:list
       (common-lisp:cons "Id"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'participant-id))
      (common-lisp:list
       (common-lisp:cons "ParticipantId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'participant-role))
      (common-lisp:list
       (common-lisp:cons "ParticipantRole"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content))
      (common-lisp:list
       (common-lisp:cons "Content"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'begin-offset-millis))
      (common-lisp:list
       (common-lisp:cons "BeginOffsetMillis"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'end-offset-millis))
      (common-lisp:list
       (common-lisp:cons "EndOffsetMillis"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'sentiment))
      (common-lisp:list
       (common-lisp:cons "Sentiment"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'issues-detected))
      (common-lisp:list
       (common-lisp:cons "IssuesDetected"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input transcript))
   common-lisp:nil))
(common-lisp:deftype transcript-content () 'common-lisp:string)
(common-lisp:deftype transcript-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun list-realtime-contact-analysis-segments
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key instance-id contact-id max-results
                     next-token)
   (common-lisp:declare
    (common-lisp:ignorable instance-id contact-id max-results next-token))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply
                       'make-list-realtime-contact-analysis-segments-request
                       aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'connect-contact-lens-request aws-sdk/generator/operation::input "POST"
        "/realtime-contact-analysis/analysis-segments"
        "ListRealtimeContactAnalysisSegments" "2020-08-21"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'list-realtime-contact-analysis-segments))