(defpackage "MATH"
  (:use "COMMON-LISP")
  (:nicknames "m")
  (:export "FLOOR" "ADD" "SUBTRACT"))

(in-package math)

(symbol-package 'math)

(symbol-value 'math)
