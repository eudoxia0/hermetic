(in-package :cl-user)
(defpackage hermetic-crane
  (:use :cl)
  (:import-from :crane
                :deftable)
  (:import-from :hermetic
                :login
                :logged-in-p
                :user-name
                :logout
                :auth)
  (:export :<abstract-user>
           :<role>
           :<user-role>
           :setup
           :username
           :full-name
           :email
           :password
           :joined
           :login
           :logged-in-p
           :user-name
           :logout
           :auth))
(in-package :hermetic-crane)

(deftable <abstract-user> ()
  (username :type text :uniquep t)
  (full-name :type text)
  (email :type text)
  (password :type text)
  (joined :type timestamp)
  (:abstractp t))

(defmacro setup (&key user-table session denied)
  `(progn
     (deftable <role> ()
       (name :type text :uniquep t)
       (:deferredp t))

     (deftable <user-role> ()
       (user :type integer :foreign (,user-table))
       (role :type integer :foreign (<role>))
       (:deferredp t))

     (crane:build 'hermetic-crane:<role>)
     (crane:build 'hermetic-crane:<user-role>)

     (hermetic:setup
      :user-p (lambda (username)
                (crane:exists ',user-table :username username))
      :user-pass (lambda (username)
                   (password (crane:single ',user-table :username username)))
      :user-roles (lambda (username)
                    (let ((user-roles (crane:filter '<user-role>
                                              :user
                                              (,(intern "ID"
                                                        *package*)
                                               (crane:single ',user-table
                                                             :username
                                                             username)))))
                      (when user-roles
                        (mapcar #'role user-roles))))
      :session ,session
      :denied ,denied)))
