# Hermetic

Simple authentication for [Clack](http://clacklisp.org/)-based Common Lisp web applications.

# Usage

See the demo app for a complete example.

# Structure

Hermetic implements authentication and authorization, respectively, using
*strategies* and *roles*.

## Strategies

A strategy represents a pluggable module that decides whether requests are
authorized to go through the web application or should be denied.

At the moment, Hermetic only supports trivial, Cookie-based authentication in
the `hermetic-cookie` module.

## Roles


## Available Password-Hashing Functions

To mitigate the risks of the NSA convincing people to hash passwords with things like SHA-256, only PBKDF2 (And eventually scrypt) is supported

* `:pbkdf2-sha1`
* `:pbkdf2-sha256`
* `:pbkdf2-sha512`

## `setup`

Hermetic is not opinionated, doesn't integrate into an existing database or create any models.
As such, it needs to be told how to find a user's information to provide authentication. This is what `setup` is for:

```lisp
(setup
    :user-p        ;; str->bool, t if a username exists, nil otherwise
    :user-pass     ;; str->str, maps a username to a password (hash, hopefully)
    :user-roles    ;; str->(list sym), maps a username to a list of roles,
                   ;; for example: (:user) (:user :tester :staff) (:user :admin)
    :session       ;; the /expression/ for the session object. ningle:*session* on
                   ;; Ningle <https://github.com/fukamachi/ningle>.
    :denied        ;; A function that displays an "access denied" message
                   )
```

For example, if your users are stored in a simple in-memory hash-table as in the demo app:

```lisp
(defmacro get-user (username)
  `(gethash ,username *users*))

(setup
 :user-p #'(lambda (user) (get-user user))
 :user-pass #'(lambda (user) (getf (get-user user) :pass))
 :user-roles #'(lambda (user) (getf (get-user user) :roles))
 :session *session*)
```

## `login`

When creating your login view, the `login` macro handles most of the work for you.

## `auth`

Grants access to a site only to users whose roles intersect with the roles in the first argument.

If an access denied page is not provided, the global one is used instead.

Example:

```lisp
(setf (route *app* "/user/profile/:userid" :method :GET)
      (lambda (params
        (auth (:user)
              (render-template "templates/profile.html")
              (render-error "You have to log in to view user profiles.")))))
```

## Misc.

When `auth` isn't enough to determine who gets to use what, Hermetic provides a few
functions for accessing user data from inside a view.

* `logged-in-p`: Exactly what it says on the tin.
* `user-name`: Returns the username of the current user.
* `roles`: Returns the list of roles of the current user.
* `role-p`: Checks if a user has a role.

## `logout`

Logs the user out. Takes two expressions, `on-success` and `on-failure`.

# License

Copyright (c) 2013 Fernando Borretti (eudoxiahp@gmail.com).

Licensed under the MIT License.
