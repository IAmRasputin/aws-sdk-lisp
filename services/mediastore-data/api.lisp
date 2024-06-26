;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/mediastore-data/api
  (:use)
  (:nicknames #:aws/mediastore-data)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/mediastore-data/api)
(common-lisp:progn
 (common-lisp:defclass mediastore-data-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "mediastore-data" :protocol
                        :rest-json))
 (common-lisp:export 'mediastore-data-request))
(common-lisp:progn
 (common-lisp:define-condition mediastore-data-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'mediastore-data-error))
(common-lisp:defvar *error-map*
  '(("ContainerNotFoundException" . container-not-found-exception)
    ("InternalServerError" . internal-server-error)
    ("ObjectNotFoundException" . object-not-found-exception)
    ("RequestedRangeNotSatisfiableException"
     . requested-range-not-satisfiable-exception)))
(common-lisp:progn
 (common-lisp:define-condition container-not-found-exception
     (mediastore-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       container-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'container-not-found-exception
                    'container-not-found-exception-message)))
(common-lisp:deftype content-range-pattern () 'common-lisp:string)
(common-lisp:deftype content-type () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass delete-object-request common-lisp:nil
                       ((path :initarg :path :type
                         (common-lisp:or path-naming common-lisp:null)
                         :accessor %delete-object-request-path :initform
                         (common-lisp:error ":path is required"))))
 (common-lisp:export
  (common-lisp:list 'delete-object-request 'make-delete-object-request))
 (common-lisp:defun make-delete-object-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key path)
   (common-lisp:apply #'common-lisp:make-instance 'delete-object-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          delete-object-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          delete-object-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          delete-object-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass delete-object-response common-lisp:nil common-lisp:nil)
 (common-lisp:export
  (common-lisp:list 'delete-object-response 'make-delete-object-response))
 (common-lisp:defun make-delete-object-response
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key)
   (common-lisp:apply #'common-lisp:make-instance 'delete-object-response
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          delete-object-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          delete-object-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          delete-object-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass describe-object-request common-lisp:nil
                       ((path :initarg :path :type
                         (common-lisp:or path-naming common-lisp:null)
                         :accessor %describe-object-request-path :initform
                         (common-lisp:error ":path is required"))))
 (common-lisp:export
  (common-lisp:list 'describe-object-request 'make-describe-object-request))
 (common-lisp:defun make-describe-object-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key path)
   (common-lisp:apply #'common-lisp:make-instance 'describe-object-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          describe-object-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          describe-object-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          describe-object-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass describe-object-response common-lisp:nil
                       ((last-modified :initarg :last-modified :type
                         (common-lisp:or time-stamp common-lisp:null) :accessor
                         %describe-object-response-last-modified :initform
                         common-lisp:nil)
                        (cache-control :initarg :cache-control :type
                         (common-lisp:or string-primitive common-lisp:null)
                         :accessor %describe-object-response-cache-control
                         :initform common-lisp:nil)
                        (content-length :initarg :content-length :type
                         (common-lisp:or non-negative-long common-lisp:null)
                         :accessor %describe-object-response-content-length
                         :initform common-lisp:nil)
                        (content-type :initarg :content-type :type
                         (common-lisp:or content-type common-lisp:null)
                         :accessor %describe-object-response-content-type
                         :initform common-lisp:nil)
                        (etag :initarg :etag :type
                         (common-lisp:or etag common-lisp:null) :accessor
                         %describe-object-response-etag :initform
                         common-lisp:nil)))
 (common-lisp:export
  (common-lisp:list 'describe-object-response 'make-describe-object-response))
 (common-lisp:defun make-describe-object-response
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key last-modified cache-control
                     content-length content-type etag)
   (common-lisp:apply #'common-lisp:make-instance 'describe-object-response
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          describe-object-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'etag))
      (common-lisp:cons "ETag" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-type))
      (common-lisp:cons "Content-Type" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-length))
      (common-lisp:cons "Content-Length" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'cache-control))
      (common-lisp:cons "Cache-Control" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'last-modified))
      (common-lisp:cons "Last-Modified" aws-sdk/generator/shape::value))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          describe-object-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          describe-object-response))
   common-lisp:nil))
(common-lisp:deftype etag () 'common-lisp:string)
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass get-object-request common-lisp:nil
                       ((range :initarg :range :type
                         (common-lisp:or range-pattern common-lisp:null)
                         :accessor %get-object-request-range :initform
                         common-lisp:nil)
                        (path :initarg :path :type
                         (common-lisp:or path-naming common-lisp:null)
                         :accessor %get-object-request-path :initform
                         (common-lisp:error ":path is required"))))
 (common-lisp:export
  (common-lisp:list 'get-object-request 'make-get-object-request))
 (common-lisp:defun make-get-object-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key range path)
   (common-lisp:apply #'common-lisp:make-instance 'get-object-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input get-object-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'range))
      (common-lisp:cons "Range" aws-sdk/generator/shape::value))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input get-object-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input get-object-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass get-object-response common-lisp:nil
                       ((status-code :initarg :status-code :type
                         (common-lisp:or |statusCode| common-lisp:null)
                         :accessor %get-object-response-status-code :initform
                         (common-lisp:error ":status-code is required"))
                        (last-modified :initarg :last-modified :type
                         (common-lisp:or time-stamp common-lisp:null) :accessor
                         %get-object-response-last-modified :initform
                         common-lisp:nil)
                        (etag :initarg :etag :type
                         (common-lisp:or etag common-lisp:null) :accessor
                         %get-object-response-etag :initform common-lisp:nil)
                        (content-type :initarg :content-type :type
                         (common-lisp:or content-type common-lisp:null)
                         :accessor %get-object-response-content-type :initform
                         common-lisp:nil)
                        (content-length :initarg :content-length :type
                         (common-lisp:or non-negative-long common-lisp:null)
                         :accessor %get-object-response-content-length
                         :initform common-lisp:nil)
                        (content-range :initarg :content-range :type
                         (common-lisp:or content-range-pattern
                                         common-lisp:null)
                         :accessor %get-object-response-content-range :initform
                         common-lisp:nil)
                        (cache-control :initarg :cache-control :type
                         (common-lisp:or string-primitive common-lisp:null)
                         :accessor %get-object-response-cache-control :initform
                         common-lisp:nil)
                        (body :initarg :body :type
                         (common-lisp:or payload-blob common-lisp:null)
                         :accessor %get-object-response-body :initform
                         common-lisp:nil)))
 (common-lisp:export
  (common-lisp:list 'get-object-response 'make-get-object-response))
 (common-lisp:defun make-get-object-response
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key status-code last-modified etag
                     content-type content-length content-range cache-control
                     body)
   (common-lisp:apply #'common-lisp:make-instance 'get-object-response
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input get-object-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'cache-control))
      (common-lisp:cons "Cache-Control" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-range))
      (common-lisp:cons "Content-Range" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-length))
      (common-lisp:cons "Content-Length" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-type))
      (common-lisp:cons "Content-Type" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'etag))
      (common-lisp:cons "ETag" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'last-modified))
      (common-lisp:cons "Last-Modified" aws-sdk/generator/shape::value))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input get-object-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'body))
      (common-lisp:list
       (common-lisp:cons "Body"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input get-object-response))
   (com.inuoe.jzon:stringify
    (common-lisp:slot-value aws-sdk/generator/shape::input 'body))))
(common-lisp:progn
 (common-lisp:define-condition internal-server-error
     (mediastore-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-server-error-message)))
 (common-lisp:export
  (common-lisp:list 'internal-server-error 'internal-server-error-message)))
(common-lisp:progn
 (common-lisp:defclass item common-lisp:nil
                       ((content-length :initarg :content-length :type
                         (common-lisp:or non-negative-long common-lisp:null)
                         :accessor %item-content-length :initform
                         common-lisp:nil)
                        (content-type :initarg :content-type :type
                         (common-lisp:or content-type common-lisp:null)
                         :accessor %item-content-type :initform
                         common-lisp:nil)
                        (last-modified :initarg :last-modified :type
                         (common-lisp:or time-stamp common-lisp:null) :accessor
                         %item-last-modified :initform common-lisp:nil)
                        (etag :initarg :etag :type
                         (common-lisp:or etag common-lisp:null) :accessor
                         %item-etag :initform common-lisp:nil)
                        (type :initarg :type :type
                         (common-lisp:or item-type common-lisp:null) :accessor
                         %item-type :initform common-lisp:nil)
                        (name :initarg :name :type
                         (common-lisp:or item-name common-lisp:null) :accessor
                         %item-name :initform common-lisp:nil)))
 (common-lisp:export (common-lisp:list 'item 'make-item))
 (common-lisp:defun make-item
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key content-length content-type last-modified
                     etag type name)
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
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'etag))
      (common-lisp:list
       (common-lisp:cons "ETag"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'last-modified))
      (common-lisp:list
       (common-lisp:cons "LastModified"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-type))
      (common-lisp:list
       (common-lisp:cons "ContentType"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-length))
      (common-lisp:list
       (common-lisp:cons "ContentLength"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input item))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype item-list () '(trivial-types:proper-list item))
 (common-lisp:defun |make-item-list|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list item))
   aws-sdk/generator/shape::members))
(common-lisp:deftype item-name () 'common-lisp:string)
(common-lisp:deftype item-type () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass list-items-request common-lisp:nil
                       ((next-token :initarg :next-token :type
                         (common-lisp:or pagination-token common-lisp:null)
                         :accessor %list-items-request-next-token :initform
                         common-lisp:nil)
                        (max-results :initarg :max-results :type
                         (common-lisp:or list-limit common-lisp:null) :accessor
                         %list-items-request-max-results :initform
                         common-lisp:nil)
                        (path :initarg :path :type
                         (common-lisp:or list-path-naming common-lisp:null)
                         :accessor %list-items-request-path :initform
                         common-lisp:nil)))
 (common-lisp:export
  (common-lisp:list 'list-items-request 'make-list-items-request))
 (common-lisp:defun make-list-items-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key next-token max-results path)
   (common-lisp:apply #'common-lisp:make-instance 'list-items-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input list-items-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input list-items-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input list-items-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass list-items-response common-lisp:nil
                       ((next-token :initarg :next-token :type
                         (common-lisp:or pagination-token common-lisp:null)
                         :accessor %list-items-response-next-token :initform
                         common-lisp:nil)
                        (items :initarg :items :type
                         (common-lisp:or item-list common-lisp:null) :accessor
                         %list-items-response-items :initform
                         common-lisp:nil)))
 (common-lisp:export
  (common-lisp:list 'list-items-response 'make-list-items-response))
 (common-lisp:defun make-list-items-response
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key next-token items)
   (common-lisp:apply #'common-lisp:make-instance 'list-items-response
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input list-items-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input list-items-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'items))
      (common-lisp:list
       (common-lisp:cons "Items"
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
                        ((aws-sdk/generator/shape::input list-items-response))
   common-lisp:nil))
(common-lisp:deftype list-limit () 'common-lisp:integer)
(common-lisp:deftype list-path-naming () 'common-lisp:string)
(common-lisp:deftype non-negative-long () 'common-lisp:integer)
(common-lisp:progn
 (common-lisp:define-condition object-not-found-exception
     (mediastore-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       object-not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'object-not-found-exception
                    'object-not-found-exception-message)))
(common-lisp:deftype pagination-token () 'common-lisp:string)
(common-lisp:deftype path-naming () 'common-lisp:string)
(common-lisp:deftype payload-blob ()
  '(common-lisp:simple-array (common-lisp:unsigned-byte 8) (common-lisp:*)))
(common-lisp:progn
 (common-lisp:defclass put-object-request common-lisp:nil
                       ((upload-availability :initarg :upload-availability
                         :type
                         (common-lisp:or upload-availability common-lisp:null)
                         :accessor %put-object-request-upload-availability
                         :initform common-lisp:nil)
                        (storage-class :initarg :storage-class :type
                         (common-lisp:or storage-class common-lisp:null)
                         :accessor %put-object-request-storage-class :initform
                         common-lisp:nil)
                        (cache-control :initarg :cache-control :type
                         (common-lisp:or string-primitive common-lisp:null)
                         :accessor %put-object-request-cache-control :initform
                         common-lisp:nil)
                        (content-type :initarg :content-type :type
                         (common-lisp:or content-type common-lisp:null)
                         :accessor %put-object-request-content-type :initform
                         common-lisp:nil)
                        (path :initarg :path :type
                         (common-lisp:or path-naming common-lisp:null)
                         :accessor %put-object-request-path :initform
                         (common-lisp:error ":path is required"))
                        (body :initarg :body :type
                         (common-lisp:or payload-blob common-lisp:null)
                         :accessor %put-object-request-body :initform
                         (common-lisp:error ":body is required"))))
 (common-lisp:export
  (common-lisp:list 'put-object-request 'make-put-object-request))
 (common-lisp:defun make-put-object-request
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key upload-availability storage-class
                     cache-control content-type path body)
   (common-lisp:apply #'common-lisp:make-instance 'put-object-request
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input put-object-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-type))
      (common-lisp:cons "Content-Type" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'cache-control))
      (common-lisp:cons "Cache-Control" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'storage-class))
      (common-lisp:cons "x-amz-storage-class" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'upload-availability))
      (common-lisp:cons "x-amz-upload-availability"
                        aws-sdk/generator/shape::value))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input put-object-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'body))
      (common-lisp:list
       (common-lisp:cons "Body"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input put-object-request))
   (com.inuoe.jzon:stringify
    (common-lisp:slot-value aws-sdk/generator/shape::input 'body))))
(common-lisp:progn
 (common-lisp:defclass put-object-response common-lisp:nil
                       ((storage-class :initarg :storage-class :type
                         (common-lisp:or storage-class common-lisp:null)
                         :accessor %put-object-response-storage-class :initform
                         common-lisp:nil)
                        (etag :initarg :etag :type
                         (common-lisp:or etag common-lisp:null) :accessor
                         %put-object-response-etag :initform common-lisp:nil)
                        (content-sha256 :initarg :content-sha256 :type
                         (common-lisp:or sha256hash common-lisp:null) :accessor
                         %put-object-response-content-sha256 :initform
                         common-lisp:nil)))
 (common-lisp:export
  (common-lisp:list 'put-object-response 'make-put-object-response))
 (common-lisp:defun make-put-object-response
                    (
                     common-lisp:&rest aws-sdk/generator/shape::args
                     common-lisp:&key storage-class etag content-sha256)
   (common-lisp:apply #'common-lisp:make-instance 'put-object-response
                      aws-sdk/generator/shape::args))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input put-object-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input put-object-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-sha256))
      (common-lisp:list
       (common-lisp:cons "ContentSHA256"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'etag))
      (common-lisp:list
       (common-lisp:cons "ETag"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'storage-class))
      (common-lisp:list
       (common-lisp:cons "StorageClass"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input put-object-response))
   common-lisp:nil))
(common-lisp:deftype range-pattern () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition requested-range-not-satisfiable-exception
     (mediastore-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       requested-range-not-satisfiable-exception-message)))
 (common-lisp:export
  (common-lisp:list 'requested-range-not-satisfiable-exception
                    'requested-range-not-satisfiable-exception-message)))
(common-lisp:deftype sha256hash () 'common-lisp:string)
(common-lisp:deftype storage-class () 'common-lisp:string)
(common-lisp:deftype string-primitive () 'common-lisp:string)
(common-lisp:deftype time-stamp () 'common-lisp:string)
(common-lisp:deftype upload-availability () 'common-lisp:string)
(common-lisp:deftype |statusCode| () 'common-lisp:integer)
(common-lisp:progn
 (common-lisp:defun delete-object
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key path)
   (common-lisp:declare (common-lisp:ignorable path))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-delete-object-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'mediastore-data-request aws-sdk/generator/operation::input "DELETE"
        :rest-json
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/~A"
                              (common-lisp:slot-value
                               aws-sdk/generator/operation::input 'path)))
        "DeleteObject" "2017-09-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'delete-object))
(common-lisp:progn
 (common-lisp:defun describe-object
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key path)
   (common-lisp:declare (common-lisp:ignorable path))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-describe-object-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'mediastore-data-request aws-sdk/generator/operation::input "HEAD"
        :rest-json
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/~A"
                              (common-lisp:slot-value
                               aws-sdk/generator/operation::input 'path)))
        "DescribeObject" "2017-09-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'describe-object))
(common-lisp:progn
 (common-lisp:defun get-object
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key path range)
   (common-lisp:declare (common-lisp:ignorable path range))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-get-object-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'mediastore-data-request aws-sdk/generator/operation::input "GET"
        :rest-json
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/~A"
                              (common-lisp:slot-value
                               aws-sdk/generator/operation::input 'path)))
        "GetObject" "2017-09-01")
       :want-stream common-lisp:t)
      "blob" common-lisp:nil *error-map*)))
 (common-lisp:export 'get-object))
(common-lisp:progn
 (common-lisp:defun list-items
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key path max-results next-token)
   (common-lisp:declare (common-lisp:ignorable path max-results next-token))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-list-items-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'mediastore-data-request aws-sdk/generator/operation::input "GET"
        :rest-json "/" "ListItems" "2017-09-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'list-items))
(common-lisp:progn
 (common-lisp:defun put-object
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key body path content-type cache-control
                     storage-class upload-availability)
   (common-lisp:declare
    (common-lisp:ignorable body path content-type cache-control storage-class
     upload-availability))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-put-object-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'mediastore-data-request aws-sdk/generator/operation::input "PUT"
        :rest-json
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/~A"
                              (common-lisp:slot-value
                               aws-sdk/generator/operation::input 'path)))
        "PutObject" "2017-09-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'put-object))
