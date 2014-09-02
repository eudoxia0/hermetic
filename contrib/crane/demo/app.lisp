(in-package :cl-user)
(defpackage hermetic-crane-demo
  (:use :cl :hermetic-crane)
  (:import-from :crane
                :connect
                :deftable)
  (:import-from :cl-markup
                :html5)
  (:import-from :ningle
                :*session*
                :route)
  (:import-from :clack.builder
                :builder)
  (:import-from :clack.middleware.session
                :<clack-middleware-session>)
  (:export :stop))
(in-package :hermetic-crane-demo)

;; Configuration

(crane:setup
 :migrations-directory
 (merge-pathnames
  (asdf:system-relative-pathname :hermetic-crane-demo #p"contrib/crane/demo/migrations/"))
 :databases
 '(:main
   (:type :sqlite3
    :name ":memory:")))

(connect)

(crane:delete-migrations t)

;; hermetic-crane stuff

(deftable <my-users> (<abstract-user>)
  (age :type integer))

(hermetic-crane:setup
 :user-table <my-users>
 :session *session*
 :denied #'(lambda (&optional params)
             (html5 (:h1 "Generic auth denied page"))))

;; Setting up the web application

(defvar *app* (make-instance 'ningle:<app>))

;; The actual views

(setf (route *app* "/")
      (lambda (params)
        (if (logged-in-p)
            (html5 (:p (format nil "Welcome, ~A!" (user-name)))
                   (:a :href "/logout" "Logout"))
            (html5
             (:form :action "/login" :method "post"
                    "Username:" (:input :type "text" :name "username") (:br)
                    "Password:" (:input :type "text" :name "password") (:br)
                    (:input :type "submit" :value "Login"))))))

(setf (route *app* "/login" :method :POST)
      (lambda (params)
        (login params
               (html5 (:h1 "You are logged in"))
               (html5 (:h1 "Wrong password :c"))
               (html5 (:h1 "No such username")))))

(setf (route *app* "/logout" :method :GET)
      (lambda (params)
        (logout
         (html5 (:h1 "You are logged out"))
         (html5 (:h1 "You are not logged in.")))))

(setf (route *app* "/users-only" :method :GET)
      (lambda (params)
        (auth (:user)
              (html5 (:h1 "If you are seeing this, you are a user.")))))

(setf (route *app* "/admins-only" :method :GET)
      (lambda (params)
        (auth (:admin)
              (html5 (:h1 "If you are seeing this, you are an admin."))
              (html5 (:h1 "Custom auth denied page. You are not authorized!")))))

;; Register a few users

(crane:create '<my-users>
              :username "admin"
              :full-name "Admin"
              :email "admin@initech.com"
              :password (hermetic:hash "herp")
              :age 25)

;; Fire it up

(defparameter *handler*
  (clack:clackup
   (builder
    <clack-middleware-session>
    *app*)
   :port 8000))

;; Utility

(defun stop () (clack:stop *handler*))
