----------------------------------------------------------------------
Description

This package contains a couple functions that make my life easier when
writing JSON requests to RESTful APIs, such as CouchDB and
Twitter. They basically paper over Drakma and Yason to eliminate
boilerplate and normalize signaled conditions.

----------------------------------------------------------------------
API

[Function]
json-request method host port resource &rest keyword-args &keyword-args &key query-args content ssl &allow-other-keys
        => body-or-stream status-code headers uri stream must-close reason-phrase

This is a thin wrapper around drakma:http-request intended to simplify
JSON requests and paper over some weaknesses. It performs the
following functions:

  The uri is constructed from the supplied parameters.

  If query-args are supplied, they should be a plist. The list will be
  converted into an appropriate query string and appended to the uri.

  The request is initialized with the proper parameters for a JSON
  request, e.g., setting up the content-type, setting the format to
  UTF-8, and instructing Drakma to treat the response as text.

  If content is supplied, then the content-length parameter is
  calculated using Babel. This overcomes a shortcoming in Drakma,
  which uses LENGTH to determine content-length. LENGTH returns the
  number of characters in a string, not the number of octets, and thus
  is not suitable for determining the content-length of UTF-8 strings.

  Finally, it wraps the drakma:http-request with some condition
  handlers to catch common errors signaled by one of Drakma's
  dependencies and resignals them as conditions defined in this
  package.

method should be a keyword, like :GET or :POST.

If ssl is non-nil, then https will be used in the URI, rather than
http.

This function accepts additional keywords, which will be forwarded to
drakma:http-request. See drakma:http-request for a description of the
possible keyword arguments and the return values.

Example:

 (json-request :get "api.twitter.com" 80 "1/statuses/public_timeline.json" :query-args '("trim_user" "t"))

[Macro]
json-request* ((response-body-var (method host port resource
                                   &rest keyword-args
                                   &key query-args content ssl encode-content parse-response-body
                                   &allow-other-keys)) body
        => result from body

This macro provides a compact template for a making a JSON request and
dispatching on the status code of the reply. Within body,
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

 (json-request* (response (:get "api.twitter.com" 80 "1/statuses/public_timeline.json" :query-args '("trim_user" "t")))
   (200 (mapcar (lambda (r) (gethash "text" r)) response))
   (404 (print "Not found")))
