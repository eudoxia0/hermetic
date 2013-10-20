# Hermetic

Simple authentication for [Clack](http://clacklisp.org/)-based Common Lisp web applications.

# Usage

See the demo app for a complete example.

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

## `auth` decorator

TODO

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

Licensed under the [LLGPL License](http://www.cliki.net/llgpl).
