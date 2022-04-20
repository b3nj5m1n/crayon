;;;; crayon.asd

(asdf:defsystem #:crayon
  :description "Terminal string styling for common lisp"
  :author "b3nj5m1n <b3nj4m1n@gmx.net>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "crayon")))
