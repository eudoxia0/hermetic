(in-package :cl-user)
(defpackage hermetic-test-asd
  (:use :cl :asdf))
(in-package :hermetic-test-asd)

(defsystem hermetic-test
  :author "Fernando Borretti"
  :license "LLGPL"
  :depends-on (:hermetic
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "hermetic"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
