;;;; crayon.asd

(asdf:defsystem #:crayon
  :description "Terminal string styling for common lisp"
  :author "b3nj5m1n <b3nj4m1n@gmx.net>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components
  ((:module "base"
            :pathname ""
            :components
            ((:file "package")
             (:file "crayon")))
   (:module "fg"
           :pathname ""
           :components
           ((:file "package_fg")
            (:file "fg")))
   (:module "bg"
           :pathname ""
           :components
           ((:file "package_bg")
            (:file "bg")))
   (:module "fx"
           :pathname ""
           :components
           ((:file "package_fx")
            (:file "fx")))))
