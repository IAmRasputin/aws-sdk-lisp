(defpackage #:aws-sdk/generator/service
  (:use #:cl
        #:aws-sdk/generator/operation
        #:aws-sdk/generator/shape
        #:aws-sdk/error
        #:aws-sdk/utils)
  (:import-from #:aws-sdk/request
                #:request)
  (:import-from #:com.inuoe.jzon
                #:parse
                #:stringify)
  (:import-from #:alexandria
                #:when-let*)
  (:export #:dump-service
           #:load-service
           #:generate-service
           #:generate-all-services))
(in-package #:aws-sdk/generator/service)

(defpackage #:aws-sdk/generator/service/dump)

(defun dump-service-api-to-stream (service spec-dir &optional (stream *standard-output*))
  (let* ((*package* (find-package :aws-sdk/generator/service/dump))
         (*print-case* :downcase)
         (package-name (make-symbol (format nil "~:@(~A/~A/~A/~A~)" :aws-sdk :services service :api)))
         (hash (make-hash-table :test #'equal)))
    ;; Get all files in case the spec gets weird, but we should generally only need service-2.json
    (dolist (file-handle (uiop:directory-files (uiop:ensure-pathname spec-dir)))
      (setf (gethash (pathname-name file-handle) hash) (parse file-handle)))
    (let* ((exception-name (intern (format nil "~:@(~A-ERROR~)" service)))
           (request-name (intern (format nil "~:@(~A-REQUEST~)" service)))
           (service-json (gethash "service-2" hash))
           (service-protocol (intern (format nil "~:@(~A~)" (gethash+ '("metadata" "protocol") service-json)) :keyword)))
      (format stream "~&;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.~2%")
      (format stream "~&~S~%"
              `(defpackage ,package-name
                 (:use)
                 (:nicknames ,(make-symbol (format nil "~:@(~A/~A~)" :aws service)))
                 (:import-from #:aws-sdk/generator/shape)
                 (:import-from #:aws-sdk/generator/operation)
                 (:import-from #:aws-sdk/api)
                 (:import-from #:aws-sdk/request)
                 (:import-from #:aws-sdk/error)))
      (format stream "~&~S~%" `(in-package ,package-name))
      (format stream "~&~S~%"
              `(progn
                 (defclass ,request-name (request) ()
                   (:default-initargs :service ,service :protocol ,service-protocol))
                 (export ',request-name)))
      (format stream "~&~S~%"
              `(progn
                 (define-condition ,exception-name (aws-error) ())
                 (export ',exception-name)))
      (let* ((error-map (loop for name being each hash-key of (gethash "shapes" service-json)
                              using (hash-value options)
                              if (gethash "exception" options)
                              collect (cons name (lispify name)))))

        (format stream "~&~S~%"
                `(defvar ,(intern (string :*error-map*)) ',error-map))

        (loop for name being each hash-key of (gethash "shapes" service-json)
              using (hash-value options)
              do (format stream "~&~S~%" (compile-shape name options exception-name service-protocol)))

        (loop for action being each hash-key of (gethash "operations" service-json)
              using (hash-value options)
              for input = (gethash "input" options)
              for output = (gethash "output" options)
              do (format stream "~&~S~%"
                         (compile-operation
                           service
                           action
                           (gethash+ '("metadata" "apiVersion") service-json)
                           options
                           service-protocol
                           (and input
                                (loop for key being each hash-key of (gethash+ `("shapes" ,(gethash "shape" input) "members")
                                                                               service-json)
                                      collect (lispify key)))
                           (and output
                                (when-let* ((payload-shape
                                              (gethash+ `("shapes" ,(gethash "shape" output) "payload") service-json))
                                            (payload-shape (gethash+ `("shapes"
                                                                       ,(gethash "shape" output)
                                                                       "members"
                                                                       ,payload-shape)
                                                                     service-json)))
                                  (labels ((find-output-type (shape)
                                             (and shape
                                                  (or (gethash "type" shape)
                                                      (find-output-type
                                                        (gethash+ `("shapes" ,(gethash "shape" shape)) service-json))))))
                                    (find-output-type payload-shape))))
                           (intern (string :*error-map*)))))
        (force-output stream)))))

(defun dump-service-base-file-to-stream (service service-dir &optional (stream *standard-output*))
  (let ((*package* (find-package :aws-sdk/generator/service/dump))
        (*print-case* :downcase)
        (package-name (make-symbol (format nil "~:@(~A/~A/~A~)" :aws-sdk :services service))))
    (format stream "~&;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.~2%")
    (format stream "~&(uiop:define-package #:~A
  (:use)
  (:use-reexport ~{#:~A~^~%                 ~}))~%"
            package-name
            (mapcar (lambda (path)
                      (format nil "~A/~A" package-name (pathname-name path)))
                    (uiop:directory-files service-dir)))
    (force-output stream)))

(defun dump-service (service spec-dir output-dir)
  (let* ((service-directory (uiop:ensure-directory-pathname (merge-pathnames service output-dir)))
         (api-path (merge-pathnames "api.lisp" service-directory))
         (base-path (merge-pathnames (make-pathname :name service :type "lisp")
                                     output-dir)))
    (ensure-directories-exist service-directory)
    (with-open-file (out api-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dump-service-api-to-stream service spec-dir out))
    (with-open-file (out base-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (when out
        (dump-service-base-file-to-stream service service-directory out)))
    base-path))

(defun load-service (service json)
  (uiop:with-temporary-file (:pathname file :direction :output)
    (dump-service service json file)
    (compile-file file)
    (load file)))

(defun generate-service (service)
  (format t "~&Generating ~s~%" service)
  (let* ((output-dir (asdf:system-relative-pathname :aws-sdk #P"services/"))
         (service-dir
           (asdf:system-relative-pathname :aws-sdk
                                          (make-pathname :directory `(:relative "specs" "apis" ,service))))
         (output-version (car (last (uiop:subdirectories service-dir))))
         ;; I think, if nothing else, this file must exist.
         (service-2.json (merge-pathnames #P"service-2.json" output-version)))
    (assert (probe-file service-2.json)
            (service-2.json)
            "Service directory must have service-2.json file")
    (dump-service service output-version output-dir)))

(defun generate-all-services (&key silent)
  (dolist (service-dir
           (uiop:subdirectories (asdf:system-relative-pathname :aws-sdk #P"specs/apis/")))
    (let ((file (generate-service (car (last (pathname-directory service-dir))))))
      (unless silent
        (format t "~&Generated '~A'~%" file)))))
