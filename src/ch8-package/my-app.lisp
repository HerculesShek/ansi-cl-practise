;;;; this file contains the code of package "MY-APP"
(defpackage "MY-APPLICATION"
  (:use "COMMON-LISP")
  (:nicknames "MY-APP")
  (:export "WIN" "LOSE" "DRAW" "LST"))

(in-package my-app)

(defparameter lst '(1 2 3 4 5))
