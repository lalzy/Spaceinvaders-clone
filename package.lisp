;;;; package.lisp

(defpackage #:spaceinvaders
  (:use #:cl #:iterate)
  (:export #:main #:create-exe)) ; Create-exe is made for CCL, and only CCL.
