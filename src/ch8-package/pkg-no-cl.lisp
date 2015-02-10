;;;; the control package 
(defpackage "MY-CONTROL"
  (:use "COMMON-LISP" "MY-APP")
  (:export "*NAME*"))
(in-package my-control)
(defparameter *name* "Will")

