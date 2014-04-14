(defpackage hermetic
  (:use :cl)
  (:export :hash
           :setup
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

(defparameter +known-digests+
  (list :pbkdf2-sha1 :pbkdf2-sha256 :pbkdf2-sha512))

(defun salt (&optional (size 16))
  (ironclad:make-random-salt size))

(defun pbkdf2 (password salt digest iterations)
  (ironclad:pbkdf2-hash-password-to-combined-string password
                                                    :salt salt
                                                    :digest digest
                                                    :iterations iterations))

(defun hash (password &key (type :pbkdf2-sha256)
                      (salt (salt 16))
                      (iterations 80000))
  (let ((pass (trivial-utf-8:string-to-utf-8-bytes password)))
    (case type
      (:pbkdf2-sha1
       (pbkdf2 pass salt :sha1 iterations))
      (:pbkdf2-sha256
       (pbkdf2 pass salt :sha256 iterations))
      (:pbkdf2-sha512
       (pbkdf2 pass salt :sha512 iterations))
      (t
       (error "No such digest: ~A. Available digests: ~A."
              type
              +known-digests+)))))

(defun parse-password-hash (password-hash)
  "Because Ironclad's pbkdf2-check-password is broken."
  (let* ((split (split-sequence:split-sequence #\$ password-hash))
         (function (first split))
         (digest (first
                  (split-sequence:split-sequence #\: (second split)))))
    (list :digest (intern (string-upcase (concatenate 'string function "-" digest))
                          :keyword)
          :iterations (parse-integer
                       (second (split-sequence:split-sequence #\: (second split))))
          :salt (third split)
          :hash (fourth split))))

(defun check-password (pass password-hash)
  (let ((parsed (parse-password-hash password-hash)))
    (equal password-hash
           (hash pass
                 :type (getf parsed :digest)
                 :salt (ironclad:hex-string-to-byte-array (getf parsed :salt))
                 :iterations (getf parsed :iterations)))))
                 

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
