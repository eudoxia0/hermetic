(in-package :cl-user)
(defpackage hermetic-test
  (:use :cl
        :hermetic
        :fiveam))
(in-package :hermetic-test)

;(def-suite hash
;  :description "Testing the hash function")

(defparameter +hash-iters+
  (list
   "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"
   "7b3d979ca8330a94fa7e9e1b466d8b99e0bcdea1ec90596c0dcc8d7ef6b4300c"
   "5b24f7aa99f1e1da5698a4f91ae0f4b45651a1b625c61ed669dd25ff5b937972"
   "2ace3a22375fdf5c60d78b612ccc70c88e31cfa7c3f9be023388980a2326f2fd"
   "d32b3b15471a3ddfa23c5d6d147958e8e817f65878f3df30436e61fa639127b1")
  "The SHA256 of the string 'test', position in the list is
the number of iterations")

(loop for i from 1 to (length +hash-iters+) do
  (princ (equal (hermetic::hash "test" :sha256 i) (nth (1- i) +hash-iters+))))
