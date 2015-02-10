;;;; this file contains the code of package "MY-APP"
(defpackage "MY-APPLICATION"
  (:use "COMMON-LISP")
  (:nicknames "MY-APP")
  (:export "WIN" "LOSE" "DRAW" "LST"))
(in-package my-app)

(defparameter win 1)
(defparameter draw 0)
(defparameter lose -1)
(defparameter lst '(1 2 3 4 5))
