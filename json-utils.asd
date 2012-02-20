(asdf:defsystem #:json-utils
  :description "A few utilities for easy JSON requests."
  :author "Austin Haas <austin@pettomato.com>"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (#:drakma
               #:yason
               #:babel)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "condition")
                             (:file "json-utils")))))
