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
                       (:default-initargs :service "mediastore-data"))
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
 (common-lisp:defstruct
     (delete-object-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-delete-object-request-"))
   (path (common-lisp:error ":path is required") :type
    (common-lisp:or path-naming common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'delete-object-request 'make-delete-object-request))
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
 (common-lisp:defstruct
     (delete-object-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-delete-object-response-")))
 (common-lisp:export
  (common-lisp:list 'delete-object-response 'make-delete-object-response))
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
 (common-lisp:defstruct
     (describe-object-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-describe-object-request-"))
   (path (common-lisp:error ":path is required") :type
    (common-lisp:or path-naming common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'describe-object-request 'make-describe-object-request))
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
 (common-lisp:defstruct
     (describe-object-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-describe-object-response-"))
   (etag common-lisp:nil :type (common-lisp:or etag common-lisp:null))
   (content-type common-lisp:nil :type
    (common-lisp:or content-type common-lisp:null))
   (content-length common-lisp:nil :type
    (common-lisp:or non-negative-long common-lisp:null))
   (cache-control common-lisp:nil :type
    (common-lisp:or string-primitive common-lisp:null))
   (last-modified common-lisp:nil :type
    (common-lisp:or time-stamp common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'describe-object-response 'make-describe-object-response))
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
 (common-lisp:defstruct
     (get-object-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-object-request-"))
   (path (common-lisp:error ":path is required") :type
    (common-lisp:or path-naming common-lisp:null))
   (range common-lisp:nil :type
    (common-lisp:or range-pattern common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-object-request 'make-get-object-request))
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
 (common-lisp:defstruct
     (get-object-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-get-object-response-"))
   (body common-lisp:nil :type (common-lisp:or payload-blob common-lisp:null))
   (cache-control common-lisp:nil :type
    (common-lisp:or string-primitive common-lisp:null))
   (content-range common-lisp:nil :type
    (common-lisp:or content-range-pattern common-lisp:null))
   (content-length common-lisp:nil :type
    (common-lisp:or non-negative-long common-lisp:null))
   (content-type common-lisp:nil :type
    (common-lisp:or content-type common-lisp:null))
   (etag common-lisp:nil :type (common-lisp:or etag common-lisp:null))
   (last-modified common-lisp:nil :type
    (common-lisp:or time-stamp common-lisp:null))
   (status-code (common-lisp:error ":status-code is required") :type
    (common-lisp:or |statusCode| common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'get-object-response 'make-get-object-response))
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
   (common-lisp:slot-value aws-sdk/generator/shape::input 'body)))
(common-lisp:progn
 (common-lisp:define-condition internal-server-error
     (mediastore-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-server-error-message)))
 (common-lisp:export
  (common-lisp:list 'internal-server-error 'internal-server-error-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (item (:copier common-lisp:nil) (:conc-name "struct-shape-item-"))
   (name common-lisp:nil :type (common-lisp:or item-name common-lisp:null))
   (type common-lisp:nil :type (common-lisp:or item-type common-lisp:null))
   (etag common-lisp:nil :type (common-lisp:or etag common-lisp:null))
   (last-modified common-lisp:nil :type
    (common-lisp:or time-stamp common-lisp:null))
   (content-type common-lisp:nil :type
    (common-lisp:or content-type common-lisp:null))
   (content-length common-lisp:nil :type
    (common-lisp:or non-negative-long common-lisp:null)))
 (common-lisp:export (common-lisp:list 'item 'make-item))
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
 (common-lisp:defstruct
     (list-items-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-items-request-"))
   (path common-lisp:nil :type
    (common-lisp:or list-path-naming common-lisp:null))
   (max-results common-lisp:nil :type
    (common-lisp:or list-limit common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or pagination-token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-items-request 'make-list-items-request))
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
 (common-lisp:defstruct
     (list-items-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-list-items-response-"))
   (items common-lisp:nil :type (common-lisp:or item-list common-lisp:null))
   (next-token common-lisp:nil :type
    (common-lisp:or pagination-token common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'list-items-response 'make-list-items-response))
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
 (common-lisp:defstruct
     (put-object-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-put-object-request-"))
   (body (common-lisp:error ":body is required") :type
    (common-lisp:or payload-blob common-lisp:null))
   (path (common-lisp:error ":path is required") :type
    (common-lisp:or path-naming common-lisp:null))
   (content-type common-lisp:nil :type
    (common-lisp:or content-type common-lisp:null))
   (cache-control common-lisp:nil :type
    (common-lisp:or string-primitive common-lisp:null))
   (storage-class common-lisp:nil :type
    (common-lisp:or storage-class common-lisp:null))
   (upload-availability common-lisp:nil :type
    (common-lisp:or upload-availability common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'put-object-request 'make-put-object-request))
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
   (common-lisp:slot-value aws-sdk/generator/shape::input 'body)))
(common-lisp:progn
 (common-lisp:defstruct
     (put-object-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-put-object-response-"))
   (content-sha256 common-lisp:nil :type
    (common-lisp:or sha256hash common-lisp:null))
   (etag common-lisp:nil :type (common-lisp:or etag common-lisp:null))
   (storage-class common-lisp:nil :type
    (common-lisp:or storage-class common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'put-object-response 'make-put-object-response))
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
        'mediastore-data-request aws-sdk/generator/operation::input "GET" "/"
        "ListItems" "2017-09-01"))
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
        (common-lisp:lambda (aws-sdk/generator/operation::input)
          (common-lisp:format common-lisp:nil "/~A"
                              (common-lisp:slot-value
                               aws-sdk/generator/operation::input 'path)))
        "PutObject" "2017-09-01"))
      common-lisp:nil common-lisp:nil *error-map*)))
 (common-lisp:export 'put-object))
