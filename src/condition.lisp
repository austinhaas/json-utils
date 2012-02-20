(in-package #:json-utils)

(define-condition cannot-encode-to-json-condition (error)
  ((bad-input :initarg :bad-input :reader bad-input))
  (:report (lambda (c stream)
             (format stream "Can't encode ~A to json." (bad-input c))))
  (:documentation "This condition is signaled when there is a problem
 encoding something to json."))

(define-condition host-not-found-condition (error)
  ((host :initarg :host :reader host))
  (:report (lambda (c stream)
             (format stream "Can't connect to ~A." (host c))))
  (:documentation "This condition is signaled when an http request
 fails because it can't find the host."))

(define-condition connection-refused-condition (error)
  ((host :initarg :host :reader host)
   (port :initarg :port :reader port))
  (:report (lambda (c stream)
             (format stream "Connection to ~A:~A refused." (host c) (port c))))
  (:documentation "This condition is signaled when an http request
 fails because a connection is refused. Wrong port, maybe?"))

(define-condition http-error-condition (error)
  ((body :initarg :body :reader body))
  (:report (lambda (c stream)
             (let ((parsed-body (yason:parse (body c))))
               (format stream "~A; ~A"
                       (gethash "error" parsed-body)
                       (gethash "reason" parsed-body)))))
  (:documentation "A generic condition for http error messages."))

(define-condition unauthorized-condition (http-error-condition)
  ()
  (:documentation "This condition is signaled when an http request
 fails because of a 401:unauthorized response."))

(define-condition bad-request-condition (http-error-condition)
  ()
  (:documentation "This condition is signaled when an http request
 fails because of a 400:Bad Request response."))

(define-condition precondition-failed-condition (http-error-condition)
  ()
  (:documentation "This condition is signaled when a precondition
 fails, such as when you try to create a database where one already
 exists."))

(define-condition not-found-condition (http-error-condition)
  ()
  (:documentation "This condition is signaled when an operation fails
 because the target resource is not found."))

(define-condition conflict-condition (http-error-condition)
  ()
  (:documentation "This condition is signaled when an operation fails
 because of a conflict."))
