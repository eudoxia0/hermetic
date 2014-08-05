(in-package :cl-user)
(defpackage hermetic-crane-demo-asd
  (:use :cl :asdf))
(in-package :hermetic-crane-demo-asd)

(defsystem hermetic-crane-demo
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:hermetic-crane
               :cl-markup
               :ningle)
  :components ((:module "contrib/crane/demo"
                :components
                ((:file "app")))))
