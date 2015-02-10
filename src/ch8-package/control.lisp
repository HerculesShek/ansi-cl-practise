;;;; the control package 
(defpackage "MY-CONTROL"
  (:use "COMMON-LISP" "MY-APP")
  (:export "MAIN" "*NAME*"))
(in-package my-control)

(defun main ()
  (format t "the value of lose is ~S" my-app:LOSE))

(defparameter *name* "Will")

