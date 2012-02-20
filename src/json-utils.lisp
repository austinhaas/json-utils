(in-package #:json-utils)

(defmacro check-types (&rest type-declarations)
  "Do CHECK-TYPE for several items. Accepts any number of lists, which
should be in the same form as the arguments to CHECK-TYPE: place
typespec [string] and expands into a CHECK-TYPE call for each one."
  `(progn
     ,@(loop for (place typespec string) in type-declarations
          collecting (list 'check-type place typespec string))))

(defun qs (&rest args)
  "Turns args into a query string. If any value is null, that pair
will not be included."
  (assert (evenp (length args)))
  (with-output-to-string (s)
    (do ((key (car args) (car args))
         (val (cadr args) (cadr args))
         (args (cddr args) (cddr args)))
        ((null key) s)
      (unless (null val)
       (format s "~A=~A" key val)
       (when args
         (format s "&"))))))

(defun yason-encode-to-string (thing)
  "Use yason:encode to convert thing to json and return the result as
a string."
  (with-output-to-string (s)
    (handler-case
        (yason:encode thing s)
      ;; yason doesn't define a condition for the case when there is
      ;; no generic function to handle an input, so we catch
      ;; simple-error and signal a more informative error local to
      ;; this package.
      (simple-error () (error 'cannot-encode-to-json-condition :bad-input thing)))
    s))

(defun json-request (method host port resource &rest keyword-args &key query-args content ssl &allow-other-keys)
  "This is a thin wrapper around drakma:http-request intended to
simplify JSON requests and paper over some weaknesses. It performs
the following functions:

The uri is constructed from the supplied parameters.

If query-args are supplied, they should be a plist. The list will be
converted into an appropriate query string and appended to the uri.

The request is initialized with the proper parameters for a JSON
request, e.g., setting up the content-type, setting the format to
UTF-8, and instructing Drakma to treat the response as text.

If content is supplied, then the content-length parameter is
calculated using Babel. This overcomes a shortcoming in Drakma, which
uses LENGTH to determine content-length. LENGTH returns the number of
characters in a string, not the number of octets, and thus is not
suitable for determining the content-length of UTF-8 strings.

Finally, it wraps the drakma:http-request with some condition handlers
to catch common errors signaled by one of Drakma's dependencies and
resignals them as conditions defined in this package.

method should be a keyword, like :GET or :POST.

If ssl is non-nil, then https will be used in the URI, rather than
http.

This function accepts additional keywords, which will be forwarded to
drakma:http-request. See drakma:http-request for a description of the
possible keyword arguments and the return values.

Example:

 (json-request :get \"api.twitter.com\" 80 \"1/statuses/public_timeline.json\" :query-args '(\"trim_user\" \"t\"))
"
  (check-types (method symbol) (host string) (port integer) (resource string)
               (query-args list) (content (or null string)))
  (handler-case
      (let ((drakma:*text-content-types* (cons (cons "application" "json") drakma:*text-content-types*))
            (uri (format nil "http~:[~;s~]://~A:~A/~A?~A" ssl host port resource (apply #'qs query-args)))
            (args (nconc
                   (list :method method
                         :content-type "application/json; charset=utf-8"
                         :external-format-out :UTF-8
                         :external-format-in :UTF-8
                         :content-length (if content
                                             (babel:string-size-in-octets content :encoding :utf-8)
                                             0)
                         :allow-other-keys t)
                   keyword-args)))
        (apply #'drakma:http-request uri args))
    (USOCKET:NS-HOST-NOT-FOUND-ERROR () (error 'host-not-found-condition :host host))
    (USOCKET:CONNECTION-REFUSED-ERROR () (error 'connection-refused-condition :host host :port port))))

(defmacro json-request* ((response-body-var (method host port resource
                                             &rest keyword-args
                                             &key query-args
                                               (content nil content-supplied-p)
                                               ssl
                                               (encode-content t)
                                               (parse-response-body t)
                                             &allow-other-keys))
                         &body body)
  "This macro provides a compact template for a making a JSON request
and dispatching on the status code of the reply. Within body,
response-body-var is bound to the body of the reply.

Most arguments are passed directly to json-request. See that function
for their documentation.

If content is supplied and encode-content is t, then content will be
encoded into JSON, using yason:encode, before creating the request. If
content cannot be encoded, CANNOT-ENCODE-TO-JSON-CONDITION will be
signaled.

If parse-response-body is t, then the response will be converted from
JSON to a hash-table, using yason:parse, before being bound to
response-body-var.

Handlers for several error statuses are provided, but they can be
overridden. The default handlers simply signal a corresponding
condition, such as NOT-FOUND-CONDITION when the status code is 404.

Example:

 (json-request* (response (:get \"api.twitter.com\" 80 \"1/statuses/public_timeline.json\" :query-args '(\"trim_user\" \"t\")))
   (200 (mapcar (lambda (r) (gethash \"text\" r)) response))
   (404 (print \"Not found\")))
"
  (let ((status-code (gensym))
        (response-body (gensym))
        (declarations (loop for item in body
                         while (and (listp item) (eql (first item) 'declare))
                         collecting (pop body))))
    `(multiple-value-bind (,response-body ,status-code)
         (json-request ,method ,host ,port ,resource
                       :query-args ,query-args
                       :content ,(if (and content-supplied-p encode-content)
                                     `(yason-encode-to-string ,content)
                                      content)
                       :ssl ,ssl
                       ,@keyword-args)
       (let ((,response-body-var ,(if parse-response-body
                                      `(yason:parse ,response-body)
                                      response-body)))
         ,@declarations
         (ecase ,status-code
           ,@body
           (400 (error 'bad-request-condition :body ,response-body))
           (401 (error 'unauthorized-condition :body ,response-body))
           (404 (error 'not-found-condition :body ,response-body))
           (409 (error 'conflict-condition :body ,response-body))
           (412 (error 'precondition-failed-condition :body ,response-body)))))))
