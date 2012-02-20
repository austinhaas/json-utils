(defpackage #:json-utils
  (:use #:cl)
  (:export
   #:cannot-encode-to-json-condition
   #:host-not-found-condition
   #:connection-refused-condition
   #:http-error-condition
   #:unauthorized-condition
   #:bad-request-condition
   #:precondition-failed-condition
   #:not-found-condition
   #:conflict-condition
   #:json-request
   #:json-request*))
