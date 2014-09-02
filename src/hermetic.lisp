(defpackage hermetic
  (:use :cl :cl-pass)
  (:export :setup
           :login
           :logged-in-p
           :username
           :roles
           :role-p
           :auth))
(in-package :hermetic)

(defparameter *user-p* nil
  "A function that takes a username string, and returns t
if a user by that name exists in the database, otherwise nil.")
(defparameter *user-pass* nil
  "A function to retrieve the hash of a user's password
from its username")
(defparameter *user-roles* nil
  "A function that maps a username to a list of roles.")
(defparameter *session* nil
  "The expression for accessing the session object.")
(defparameter *denied-page* nil
  "A function that gets called when a user tries to access a page without
sufficient privileges")

(defun authorize (user pass)
  (if (funcall *user-p* user)
      (check-password pass (funcall *user-pass* user))
      :no-user))

(defmacro setup (&key user-p user-pass user-roles session denied)
  "Provide functions for *user-p* and *user-pass*"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf hermetic::*user-p* ,user-p
           hermetic::*user-pass* ,user-pass
           hermetic::*user-roles* ,user-roles
           hermetic::*session* ',session
           hermetic::*denied-page* ,denied)))

(defmacro login (params on-success on-failure on-no-user)
  `(let ((user (getf ,params :|username|))
         (pass (getf ,params :|password|)))
     (declare (string user pass))
     (case (hermetic::authorize user pass)
       ((t) (progn
              ;; Store login data on the session
              (setf (gethash :username ,hermetic::*session*) user)
              (setf (gethash :roles ,hermetic::*session*) (funcall hermetic::*user-roles* user))
              ,on-success))
       ((nil) ,on-failure)
       (:no-user ,on-no-user))))

(defmacro logout (on-success on-failure)
  `(progn
     (if (logged-in-p)
         (progn (remhash :username ,hermetic::*session*)
                (remhash :roles ,hermetic::*session*)
                ,on-success)
         ,on-failure)))

;;; Functions for getting information about the logged-in user

(defmacro logged-in-p ()
  `(gethash :username ,hermetic::*session*))

(defmacro user-name ()
  `(logged-in-p))

(defmacro roles ()
  `(gethash :roles ,hermetic::*session*))

(defmacro role-p (role)
  `(member ,role (gethash :roles ,hermetic::*session*)))

(defmacro auth ((&rest roles) page &optional denied-page)
  `(if (intersection (list ,@roles) (roles))
       ,page
       ,(if denied-page
            denied-page
            `(funcall hermetic::*denied-page*))))
