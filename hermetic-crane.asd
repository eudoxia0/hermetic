(in-package :cl-user)
(defpackage hermetic-crane-asd
  (:use :cl :asdf))
(in-package :hermetic-crane-asd)

(defsystem hermetic-crane
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:hermetic
	       :crane)
  :components ((:module "contrib/crane"
                :components
                ((:file "crane"))))
  :description "ORM-backed user objects"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
