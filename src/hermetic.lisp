(defpackage hermetic
  (:use :cl)
  (:export :setup
           :login
           :logged-in-p
           :username
           :roles
           :role-p))
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

(defun digest (str type)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence type
     (flexi-streams:string-to-octets str))))

(defun hash (str type iters)
  (if (= iters 1)
      (digest str type)
      (hash (digest str type) type (1- iters))))

(defparameter +known-digests+
  (mapcar (lambda (sym) (intern (symbol-name sym) :keyword))
          (ironclad:list-all-digests)))

(defun authorize (user pass type iters)
  (if (not (member type +known-digests+ :test #'eq))
      (error "Unknown digest type: ~A" type)
      (if (funcall *user-p* user)
          (equal (funcall *user-pass* user) (hash pass type iters))
          :no-user)))

(defmacro setup (&key user-p user-pass user-roles session)
  "Provide functions for *user-p* and *user-pass*"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf hermetic::*user-p* ,user-p
           hermetic::*user-pass* ,user-pass
           hermetic::*user-roles* ,user-roles
           hermetic::*session* ',session)))

(defmacro login (params (&key (hash :sha256) (iters 15000))
                 on-success on-failure on-no-user)
  `(let ((user (getf ,params :|username|))
         (pass (getf ,params :|password|)))
     (declare (string user pass))
     (case (hermetic::authorize user pass ,hash ,iters)
       ((t) (progn
              ;; Store login data on the session
              (setf (gethash :username ,hermetic::*session*) user)
              (setf (gethash :roles ,hermetic::*session*) (funcall hermetic::*user-roles* user))
              ,on-success))
       ((nil) ,on-failure)
       (:no-user ,on-no-user))))

(defmacro logout ()
  `(progn
     (if (logged-in-p)
         (progn (remhash :username ,hermetic::*session*)
                (remhash :roles ,hermetic::*session*)
                t))))

;;; Functions for getting information about the logged-in user

(defmacro logged-in-p ()
  `(gethash :username ,hermetic::*session*))

(defmacro user-name ()
  `(logged-in-p))

(defmacro roles ()
  `(gethash :roles ,hermetic::*session*))

(defmacro role-p (role)
  `(member ,role (gethash :roles ,hermetic::*session*)))
