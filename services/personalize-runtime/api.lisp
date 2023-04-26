;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/personalize-runtime/api
  (:use)
  (:nicknames #:aws/personalize-runtime)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/personalize-runtime/api)
(common-lisp:progn
 (common-lisp:defclass personalize-runtime-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "personalize-runtime"))
 (common-lisp:export 'personalize-runtime-request))
(common-lisp:progn
 (common-lisp:define-condition personalize-runtime-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'personalize-runtime-error))
(common-lisp:defvar *error-map*
  '(("InvalidInputException" . invalid-input-exception)
    ("ResourceNotFoundException" . resource-not-found-exception)))
(common-lisp:deftype arn () 'common-lisp:string)
(common-lisp:deftype attribute-name () 'common-lisp:string)
(common-lisp:deftype attribute-value () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype context () 'common-lisp:hash-table)
 (common-lisp:defun |make-context| (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:deftype filter-attribute-name () 'common-lisp:string)
(common-lisp:deftype filter-attribute-value () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype filter-values () 'common-lisp:hash-table)
 (common-lisp:defun |make-filter-values| (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:progn
 (common-lisp:defstruct
     (get-personalized-ranking-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-personalized-ranking-request-"))
   (campaign-arn (common-lisp:error ":campaignarn is required") :type
    (common-lisp:or arn common-lisp:null))
   (input-list (common-lisp:error ":inputlist is required") :type
    (common-lisp:or input-list common-lisp:null))
   (user-id (common-lisp:error ":userid is required") :type
    (common-lisp:or user-id common-lisp:null))
   (context common-lisp:nil :type (common-lisp:or context common-lisp:null))
   (filter-arn common-lisp:nil :type (common-lisp:or arn common-lisp:null))
   (filter-values common-lisp:nil :type
    (common-lisp:or filter-values common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-personalized-ranking-request
                    'make-get-personalized-ranking-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-personalized-ranking-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-personalized-ranking-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'campaign-arn))
      (common-lisp:list
       (common-lisp:cons "campaignArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'input-list))
      (common-lisp:list
       (common-lisp:cons "inputList"
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
                           aws-sdk/generator/shape::input 'context))
      (common-lisp:list
       (common-lisp:cons "context"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filter-arn))
      (common-lisp:list
       (common-lisp:cons "filterArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filter-values))
      (common-lisp:list
       (common-lisp:cons "filterValues"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-personalized-ranking-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (get-personalized-ranking-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-personalized-ranking-response-"))
   (personalized-ranking common-lisp:nil :type
    (common-lisp:or item-list common-lisp:null))
   (recommendation-id common-lisp:nil :type
    (common-lisp:or recommendation-id common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-personalized-ranking-response
                    'make-get-personalized-ranking-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-personalized-ranking-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-personalized-ranking-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'personalized-ranking))
      (common-lisp:list
       (common-lisp:cons "personalizedRanking"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'recommendation-id))
      (common-lisp:list
       (common-lisp:cons "recommendationId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-personalized-ranking-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (get-recommendations-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-recommendations-request-"))
   (campaign-arn common-lisp:nil :type (common-lisp:or arn common-lisp:null))
   (item-id common-lisp:nil :type (common-lisp:or item-id common-lisp:null))
   (user-id common-lisp:nil :type (common-lisp:or user-id common-lisp:null))
   (num-results common-lisp:nil :type
    (common-lisp:or num-results common-lisp:null))
   (context common-lisp:nil :type (common-lisp:or context common-lisp:null))
   (filter-arn common-lisp:nil :type (common-lisp:or arn common-lisp:null))
   (filter-values common-lisp:nil :type
    (common-lisp:or filter-values common-lisp:null))
   (recommender-arn common-lisp:nil :type
    (common-lisp:or arn common-lisp:null))
   (promotions common-lisp:nil :type
    (common-lisp:or promotion-list common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-recommendations-request
                    'make-get-recommendations-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-recommendations-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-recommendations-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'campaign-arn))
      (common-lisp:list
       (common-lisp:cons "campaignArn"
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
                           aws-sdk/generator/shape::input 'user-id))
      (common-lisp:list
       (common-lisp:cons "userId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'num-results))
      (common-lisp:list
       (common-lisp:cons "numResults"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'context))
      (common-lisp:list
       (common-lisp:cons "context"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filter-arn))
      (common-lisp:list
       (common-lisp:cons "filterArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filter-values))
      (common-lisp:list
       (common-lisp:cons "filterValues"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'recommender-arn))
      (common-lisp:list
       (common-lisp:cons "recommenderArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'promotions))
      (common-lisp:list
       (common-lisp:cons "promotions"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-recommendations-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (get-recommendations-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-recommendations-response-"))
   (item-list common-lisp:nil :type
    (common-lisp:or item-list common-lisp:null))
   (recommendation-id common-lisp:nil :type
    (common-lisp:or recommendation-id common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-recommendations-response
                    'make-get-recommendations-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-recommendations-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-recommendations-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'item-list))
      (common-lisp:list
       (common-lisp:cons "itemList"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'recommendation-id))
      (common-lisp:list
       (common-lisp:cons "recommendationId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-recommendations-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype input-list () '(trivial-types:proper-list item-id))
 (common-lisp:defun |make-input-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list item-id))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:define-condition invalid-input-exception
     (personalize-runtime-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-input-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-input-exception 'invalid-input-exception-message)))
(common-lisp:deftype item-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype item-list () '(trivial-types:proper-list predicted-item))
 (common-lisp:defun |make-item-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list predicted-item))
   aws-sdk/generator/shape::members))
(common-lisp:deftype name () 'common-lisp:string)
(common-lisp:deftype num-results () 'common-lisp:integer)
(common-lisp:deftype percent-promoted-items () 'common-lisp:integer)
(common-lisp:progn
 (common-lisp:defstruct
     (predicted-item (:copier common-lisp:nil)
      (:conc-name "struct-shape-predicted-item-"))
   (item-id common-lisp:nil :type (common-lisp:or item-id common-lisp:null))
   (score common-lisp:nil :type (common-lisp:or score common-lisp:null))
   (promotion-name common-lisp:nil :type
    (common-lisp:or name common-lisp:null)))
 (common-lisp:export (common-lisp:list 'predicted-item 'make-predicted-item))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input predicted-item))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input predicted-item))
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
                           aws-sdk/generator/shape::input 'score))
      (common-lisp:list
       (common-lisp:cons "score"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'promotion-name))
      (common-lisp:list
       (common-lisp:cons "promotionName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input predicted-item))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (promotion (:copier common-lisp:nil)
      (:conc-name "struct-shape-promotion-"))
   (name common-lisp:nil :type (common-lisp:or name common-lisp:null))
   (percent-promoted-items common-lisp:nil :type
    (common-lisp:or percent-promoted-items common-lisp:null))
   (filter-arn common-lisp:nil :type (common-lisp:or arn common-lisp:null))
   (filter-values common-lisp:nil :type
    (common-lisp:or filter-values common-lisp:null)))
 (common-lisp:export (common-lisp:list 'promotion 'make-promotion))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input promotion))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input promotion))
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
                           aws-sdk/generator/shape::input
                           'percent-promoted-items))
      (common-lisp:list
       (common-lisp:cons "percentPromotedItems"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filter-arn))
      (common-lisp:list
       (common-lisp:cons "filterArn"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'filter-values))
      (common-lisp:list
       (common-lisp:cons "filterValues"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input promotion))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype promotion-list () '(trivial-types:proper-list promotion))
 (common-lisp:defun |make-promotion-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list promotion))
   aws-sdk/generator/shape::members))
(common-lisp:deftype recommendation-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition resource-not-found-exception
     (personalize-runtime-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       resource-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'resource-not-found-exception
                    'resource-not-found-exception-message)))
(common-lisp:deftype score () 'common-lisp:double-float)
(common-lisp:deftype user-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun get-personalized-ranking
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key campaign-arn input-list user-id context
                     filter-arn filter-values)
   (common-lisp:declare
    (common-lisp:ignorable campaign-arn input-list user-id context filter-arn
     filter-values))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-get-personalized-ranking-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'personalize-runtime-request aws-sdk/generator/operation::input "POST"
        "/personalize-ranking" "GetPersonalizedRanking" "2018-05-22"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'get-personalized-ranking))
(common-lisp:progn
 (common-lisp:defun get-recommendations
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key campaign-arn item-id user-id num-results
                     context filter-arn filter-values recommender-arn
                     promotions)
   (common-lisp:declare
    (common-lisp:ignorable campaign-arn item-id user-id num-results context
     filter-arn filter-values recommender-arn promotions))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-get-recommendations-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'personalize-runtime-request aws-sdk/generator/operation::input "POST"
        "/recommendations" "GetRecommendations" "2018-05-22"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'get-recommendations))
