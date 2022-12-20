;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(common-lisp:defpackage #:aws-sdk/services/runtime.lex/api
  (:use)
  (:nicknames #:aws/runtime.lex)
  (:import-from #:aws-sdk/generator/shape)
  (:import-from #:aws-sdk/generator/operation)
  (:import-from #:aws-sdk/api)
  (:import-from #:aws-sdk/request)
  (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/runtime.lex/api)
(common-lisp:progn
 (common-lisp:defclass runtime.lex-request (aws-sdk/request:request)
                       common-lisp:nil
                       (:default-initargs :service "runtime.lex"))
 (common-lisp:export 'runtime.lex-request))
(common-lisp:progn
 (common-lisp:define-condition runtime.lex-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'runtime.lex-error))
(common-lisp:deftype accept () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition bad-gateway-exception
     (runtime.lex-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       bad-gateway-exception-message)))
 (common-lisp:export
  (common-lisp:list 'bad-gateway-exception 'bad-gateway-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition bad-request-exception
     (runtime.lex-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       bad-request-exception-message)))
 (common-lisp:export
  (common-lisp:list 'bad-request-exception 'bad-request-exception-message)))
(common-lisp:deftype blob-stream ()
  '(common-lisp:simple-array (common-lisp:unsigned-byte 8) (common-lisp:*)))
(common-lisp:deftype bot-alias () 'common-lisp:string)
(common-lisp:deftype bot-name () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (button (:copier common-lisp:nil) (:conc-name "struct-shape-button-"))
   (text (common-lisp:error ":text is required") :type
    (common-lisp:or button-text-string-with-length common-lisp:null))
   (value (common-lisp:error ":value is required") :type
    (common-lisp:or button-value-string-with-length common-lisp:null)))
 (common-lisp:export (common-lisp:list 'button 'make-button))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input button))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input button))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'text))
      (common-lisp:list
       (common-lisp:cons "text"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'value))
      (common-lisp:list
       (common-lisp:cons "value"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input button))
   common-lisp:nil))
(common-lisp:deftype button-text-string-with-length () 'common-lisp:string)
(common-lisp:deftype button-value-string-with-length () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition conflict-exception
     (runtime.lex-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       conflict-exception-message)))
 (common-lisp:export
  (common-lisp:list 'conflict-exception 'conflict-exception-message)))
(common-lisp:deftype content-type () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition dependency-failed-exception
     (runtime.lex-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       dependency-failed-exception-message)))
 (common-lisp:export
  (common-lisp:list 'dependency-failed-exception
                    'dependency-failed-exception-message)))
(common-lisp:deftype dialog-state () 'common-lisp:string)
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defstruct
     (generic-attachment (:copier common-lisp:nil)
      (:conc-name "struct-shape-generic-attachment-"))
   (title common-lisp:nil :type
    (common-lisp:or string-with-length common-lisp:null))
   (sub-title common-lisp:nil :type
    (common-lisp:or string-with-length common-lisp:null))
   (attachment-link-url common-lisp:nil :type
    (common-lisp:or string-url-with-length common-lisp:null))
   (image-url common-lisp:nil :type
    (common-lisp:or string-url-with-length common-lisp:null))
   (buttons common-lisp:nil :type
    (common-lisp:or |listOfButtons| common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'generic-attachment 'make-generic-attachment))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input generic-attachment))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input generic-attachment))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'title))
      (common-lisp:list
       (common-lisp:cons "title"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'sub-title))
      (common-lisp:list
       (common-lisp:cons "subTitle"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'attachment-link-url))
      (common-lisp:list
       (common-lisp:cons "attachmentLinkUrl"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'image-url))
      (common-lisp:list
       (common-lisp:cons "imageUrl"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'buttons))
      (common-lisp:list
       (common-lisp:cons "buttons"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input generic-attachment))
   common-lisp:nil))
(common-lisp:deftype http-content-type () 'common-lisp:string)
(common-lisp:deftype intent-name () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition internal-failure-exception
     (runtime.lex-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-failure-exception-message)))
 (common-lisp:export
  (common-lisp:list 'internal-failure-exception
                    'internal-failure-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition limit-exceeded-exception
     (runtime.lex-error)
     ((retry-after-seconds :initarg :retry-after-seconds :initform
       common-lisp:nil :reader limit-exceeded-exception-retry-after-seconds)
      (message :initarg :message :initform common-lisp:nil :reader
       limit-exceeded-exception-message)))
 (common-lisp:export
  (common-lisp:list 'limit-exceeded-exception
                    'limit-exceeded-exception-retry-after-seconds
                    'limit-exceeded-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition loop-detected-exception
     (runtime.lex-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       loop-detected-exception-message)))
 (common-lisp:export
  (common-lisp:list 'loop-detected-exception 'loop-detected-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition not-acceptable-exception
     (runtime.lex-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       not-acceptable-exception-message)))
 (common-lisp:export
  (common-lisp:list 'not-acceptable-exception
                    'not-acceptable-exception-message)))
(common-lisp:progn
 (common-lisp:define-condition not-found-exception
     (runtime.lex-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       not-found-exception-message)))
 (common-lisp:export
  (common-lisp:list 'not-found-exception 'not-found-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (post-content-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-post-content-request-"))
   (bot-name (common-lisp:error ":botname is required") :type
    (common-lisp:or bot-name common-lisp:null))
   (bot-alias (common-lisp:error ":botalias is required") :type
    (common-lisp:or bot-alias common-lisp:null))
   (user-id (common-lisp:error ":userid is required") :type
    (common-lisp:or user-id common-lisp:null))
   (session-attributes common-lisp:nil :type
    (common-lisp:or string common-lisp:null))
   (content-type (common-lisp:error ":contenttype is required") :type
    (common-lisp:or http-content-type common-lisp:null))
   (accept common-lisp:nil :type (common-lisp:or accept common-lisp:null))
   (input-stream (common-lisp:error ":inputstream is required") :type
    (common-lisp:or blob-stream common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'post-content-request 'make-post-content-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input post-content-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'session-attributes))
      (common-lisp:cons "x-amz-lex-session-attributes"
                        aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-type))
      (common-lisp:cons "Content-Type" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'accept))
      (common-lisp:cons "Accept" aws-sdk/generator/shape::value))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input post-content-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'input-stream))
      (common-lisp:list
       (common-lisp:cons "inputStream"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input post-content-request))
   (common-lisp:slot-value aws-sdk/generator/shape::input 'input-stream)))
(common-lisp:progn
 (common-lisp:defstruct
     (post-content-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-post-content-response-"))
   (content-type common-lisp:nil :type
    (common-lisp:or http-content-type common-lisp:null))
   (intent-name common-lisp:nil :type
    (common-lisp:or intent-name common-lisp:null))
   (slots common-lisp:nil :type (common-lisp:or string common-lisp:null))
   (session-attributes common-lisp:nil :type
    (common-lisp:or string common-lisp:null))
   (message common-lisp:nil :type (common-lisp:or text common-lisp:null))
   (dialog-state common-lisp:nil :type
    (common-lisp:or dialog-state common-lisp:null))
   (slot-to-elicit common-lisp:nil :type
    (common-lisp:or string common-lisp:null))
   (input-transcript common-lisp:nil :type
    (common-lisp:or string common-lisp:null))
   (audio-stream common-lisp:nil :type
    (common-lisp:or blob-stream common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'post-content-response 'make-post-content-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          post-content-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-type))
      (common-lisp:cons "Content-Type" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'intent-name))
      (common-lisp:cons "x-amz-lex-intent-name"
                        aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'slots))
      (common-lisp:cons "x-amz-lex-slots" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'session-attributes))
      (common-lisp:cons "x-amz-lex-session-attributes"
                        aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'message))
      (common-lisp:cons "x-amz-lex-message" aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'dialog-state))
      (common-lisp:cons "x-amz-lex-dialog-state"
                        aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'slot-to-elicit))
      (common-lisp:cons "x-amz-lex-slot-to-elicit"
                        aws-sdk/generator/shape::value))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'input-transcript))
      (common-lisp:cons "x-amz-lex-input-transcript"
                        aws-sdk/generator/shape::value))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          post-content-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'audio-stream))
      (common-lisp:list
       (common-lisp:cons "audioStream"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          post-content-response))
   (common-lisp:slot-value aws-sdk/generator/shape::input 'audio-stream)))
(common-lisp:progn
 (common-lisp:defstruct
     (post-text-request (:copier common-lisp:nil)
      (:conc-name "struct-shape-post-text-request-"))
   (bot-name (common-lisp:error ":botname is required") :type
    (common-lisp:or bot-name common-lisp:null))
   (bot-alias (common-lisp:error ":botalias is required") :type
    (common-lisp:or bot-alias common-lisp:null))
   (user-id (common-lisp:error ":userid is required") :type
    (common-lisp:or user-id common-lisp:null))
   (session-attributes common-lisp:nil :type
    (common-lisp:or string-map common-lisp:null))
   (input-text (common-lisp:error ":inputtext is required") :type
    (common-lisp:or text common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'post-text-request 'make-post-text-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input post-text-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input post-text-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'session-attributes))
      (common-lisp:list
       (common-lisp:cons "sessionAttributes"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'input-text))
      (common-lisp:list
       (common-lisp:cons "inputText"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input post-text-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defstruct
     (post-text-response (:copier common-lisp:nil)
      (:conc-name "struct-shape-post-text-response-"))
   (intent-name common-lisp:nil :type
    (common-lisp:or intent-name common-lisp:null))
   (slots common-lisp:nil :type (common-lisp:or string-map common-lisp:null))
   (session-attributes common-lisp:nil :type
    (common-lisp:or string-map common-lisp:null))
   (message common-lisp:nil :type (common-lisp:or text common-lisp:null))
   (dialog-state common-lisp:nil :type
    (common-lisp:or dialog-state common-lisp:null))
   (slot-to-elicit common-lisp:nil :type
    (common-lisp:or string common-lisp:null))
   (response-card common-lisp:nil :type
    (common-lisp:or response-card common-lisp:null)))
 (common-lisp:export
  (common-lisp:list 'post-text-response 'make-post-text-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input post-text-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input post-text-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'intent-name))
      (common-lisp:list
       (common-lisp:cons "intentName"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'slots))
      (common-lisp:list
       (common-lisp:cons "slots"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'session-attributes))
      (common-lisp:list
       (common-lisp:cons "sessionAttributes"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'message))
      (common-lisp:list
       (common-lisp:cons "message"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'dialog-state))
      (common-lisp:list
       (common-lisp:cons "dialogState"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'slot-to-elicit))
      (common-lisp:list
       (common-lisp:cons "slotToElicit"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'response-card))
      (common-lisp:list
       (common-lisp:cons "responseCard"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input post-text-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:define-condition request-timeout-exception
     (runtime.lex-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       request-timeout-exception-message)))
 (common-lisp:export
  (common-lisp:list 'request-timeout-exception
                    'request-timeout-exception-message)))
(common-lisp:progn
 (common-lisp:defstruct
     (response-card (:copier common-lisp:nil)
      (:conc-name "struct-shape-response-card-"))
   (version common-lisp:nil :type (common-lisp:or string common-lisp:null))
   (content-type common-lisp:nil :type
    (common-lisp:or content-type common-lisp:null))
   (generic-attachments common-lisp:nil :type
    (common-lisp:or |genericAttachmentList| common-lisp:null)))
 (common-lisp:export (common-lisp:list 'response-card 'make-response-card))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input response-card))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input response-card))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'version))
      (common-lisp:list
       (common-lisp:cons "version"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'content-type))
      (common-lisp:list
       (common-lisp:cons "contentType"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'generic-attachments))
      (common-lisp:list
       (common-lisp:cons "genericAttachments"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input response-card))
   common-lisp:nil))
(common-lisp:deftype string () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype string-map () 'common-lisp:hash-table)
 (common-lisp:defun |make-string-map| (aws-sdk/generator/shape::key-values)
   (common-lisp:etypecase aws-sdk/generator/shape::key-values
     (common-lisp:hash-table aws-sdk/generator/shape::key-values)
     (common-lisp:list
      (alexandria:alist-hash-table aws-sdk/generator/shape::key-values)))))
(common-lisp:deftype string-url-with-length () 'common-lisp:string)
(common-lisp:deftype string-with-length () 'common-lisp:string)
(common-lisp:deftype text () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition unsupported-media-type-exception
     (runtime.lex-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       unsupported-media-type-exception-message)))
 (common-lisp:export
  (common-lisp:list 'unsupported-media-type-exception
                    'unsupported-media-type-exception-message)))
(common-lisp:deftype user-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:deftype |genericAttachmentList| ()
   '(trivial-types:proper-list generic-attachment))
 (common-lisp:defun |make-genericattachmentlist|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list generic-attachment))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:deftype |listOfButtons| () '(trivial-types:proper-list button))
 (common-lisp:defun |make-listofbuttons|
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list button))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defun post-content
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key bot-name bot-alias user-id
                     session-attributes content-type accept input-stream)
   (common-lisp:declare
    (common-lisp:ignorable bot-name bot-alias user-id session-attributes
     content-type accept input-stream))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-post-content-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 'runtime.lex-request
                                                        aws-sdk/generator/operation::input
                                                        "POST"
                                                        (common-lisp:lambda
                                                            (
                                                             aws-sdk/generator/operation::input)
                                                          (common-lisp:format
                                                           common-lisp:nil
                                                           "/bot/~A/alias/~A/user/~A/content"
                                                           (quri.encode:url-encode
                                                            (common-lisp:slot-value
                                                             aws-sdk/generator/operation::input
                                                             'bot-name))
                                                           (quri.encode:url-encode
                                                            (common-lisp:slot-value
                                                             aws-sdk/generator/operation::input
                                                             'bot-alias))
                                                           (quri.encode:url-encode
                                                            (common-lisp:slot-value
                                                             aws-sdk/generator/operation::input
                                                             'user-id))))
                                                        "PostContent"
                                                        "2016-11-28"))
      "blob" common-lisp:nil
      '(("BadGatewayException" . bad-gateway-exception)
        ("BadRequestException" . bad-request-exception)
        ("ConflictException" . conflict-exception)
        ("DependencyFailedException" . dependency-failed-exception)
        ("InternalFailureException" . internal-failure-exception)
        ("LimitExceededException" . limit-exceeded-exception)
        ("LoopDetectedException" . loop-detected-exception)
        ("NotAcceptableException" . not-acceptable-exception)
        ("NotFoundException" . not-found-exception)
        ("RequestTimeoutException" . request-timeout-exception)
        ("UnsupportedMediaTypeException"
         . unsupported-media-type-exception)))))
 (common-lisp:export 'post-content))
(common-lisp:progn
 (common-lisp:defun post-text
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key bot-name bot-alias user-id
                     session-attributes input-text)
   (common-lisp:declare
    (common-lisp:ignorable bot-name bot-alias user-id session-attributes
     input-text))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-post-text-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input 'runtime.lex-request
                                                        aws-sdk/generator/operation::input
                                                        "POST"
                                                        (common-lisp:lambda
                                                            (
                                                             aws-sdk/generator/operation::input)
                                                          (common-lisp:format
                                                           common-lisp:nil
                                                           "/bot/~A/alias/~A/user/~A/text"
                                                           (quri.encode:url-encode
                                                            (common-lisp:slot-value
                                                             aws-sdk/generator/operation::input
                                                             'bot-name))
                                                           (quri.encode:url-encode
                                                            (common-lisp:slot-value
                                                             aws-sdk/generator/operation::input
                                                             'bot-alias))
                                                           (quri.encode:url-encode
                                                            (common-lisp:slot-value
                                                             aws-sdk/generator/operation::input
                                                             'user-id))))
                                                        "PostText"
                                                        "2016-11-28"))
      common-lisp:nil common-lisp:nil
      '(("BadGatewayException" . bad-gateway-exception)
        ("BadRequestException" . bad-request-exception)
        ("ConflictException" . conflict-exception)
        ("DependencyFailedException" . dependency-failed-exception)
        ("InternalFailureException" . internal-failure-exception)
        ("LimitExceededException" . limit-exceeded-exception)
        ("LoopDetectedException" . loop-detected-exception)
        ("NotAcceptableException" . not-acceptable-exception)
        ("NotFoundException" . not-found-exception)
        ("RequestTimeoutException" . request-timeout-exception)
        ("UnsupportedMediaTypeException"
         . unsupported-media-type-exception)))))
 (common-lisp:export 'post-text))
