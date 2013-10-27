(defpackage hermetic-demo
  (:use :cl)
  (:import-from :cl-markup
                :html5)
  (:import-from :ningle
                :*session*)
  (:import-from :clack.builder
                :builder)
  (:import-from :clack.middleware.session
                :<clack-middleware-session>)
  (:import-from :hermetic
                :setup
                :login
                :logged-in-p
                :user-name
                :logout
                :auth)
  (:export :stop))
(in-package :hermetic-demo)

(defvar *app* (make-instance 'ningle:<app>))

(defparameter *users* (make-hash-table :test #'equal))

(defun make-user (username pass roles)
  (setf (gethash username *users*)
        (list :pass (hermetic::hash pass :sha256 15000)
              :roles roles)))

(make-user "admin" "admin" (list :user :staff :admin))
(make-user "joe.avg" "pass" (list :user))

(defmacro get-user (username)
  `(gethash ,username *users*))

(setup
 :user-p #'(lambda (user) (get-user user))
 :user-pass #'(lambda (user) (getf (get-user user) :pass))
 :user-roles #'(lambda (user) (getf (get-user user) :roles))
 :session *session*
 :denied #'(lambda (&optional params)
             (html5 (:h1 "Generic auth denied page"))))

(setf (ningle:route *app* "/")
      (lambda (params)
        (if (logged-in-p)
            (html5 (:p (format nil "Welcome, ~A!" (user-name)))
                   (:a :href "/logout" "Logout"))
            (html5
             (:form :action "/login" :method "post"
                    "Username:" (:input :type "text" :name "username") (:br)
                    "Password:" (:input :type "text" :name "password") (:br)
                    (:input :type "submit" :value "Login"))))))

(setf (ningle:route *app* "/login" :method :POST)
      (lambda (params)
        (login params ()
               (html5 (:h1 "You are logged in"))
               (html5 (:h1 "Wrong password :c"))
               (html5 (:h1 "No such username")))))

(setf (ningle:route *app* "/logout" :method :GET)
      (lambda (params)
        (logout
         (html5 (:h1 "You are logged out"))
         (html5 (:h1 "You are not logged in.")))))

(setf (ningle:route *app* "/users-only" :method :GET)
      (lambda (params)
        (auth (:user)
              (html5 (:h1 "If you are seeing this, you are an admin.")))))

(setf (ningle:route *app* "/admins-only" :method :GET)
      (lambda (params)
        (auth (:admin)
              (html5 (:h1 "If you are seeing this, you are a user."))
              (html5 (:h1 "Custom auth denied page. You are not authorized!")))))

(defparameter *handler*
  (clack:clackup
   (builder
    <clack-middleware-session>
    *app*)))

(defun stop () (clack:stop *handler*))
