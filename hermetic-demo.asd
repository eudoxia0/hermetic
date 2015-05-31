(in-package :cl-user)
(defpackage hermetic-demo
  (:use :cl :asdf))
(in-package :hermetic-demo)

(defsystem hermetic-demo
  :version "0.1"
  :author "Fernando Borretti"
  :description "Demo for Hermetic"
  :license "MIT"
  :depends-on (:hermetic
               :ningle
               :cl-markup)
  :components ((:module "demo"
                :components
                ((:file "app"))))
  :description "A simple ningle app to showcase hermetic's features")
