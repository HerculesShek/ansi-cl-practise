;;;; source file for chapter 8 of ANSI Common Lisp

;; test symbol-name
(defun test-symbol-name ()
  (symbol-name '||))

;; test special symbols
;; => (|Lisp 1.5| || |will| NAME)
(defun visit-special-symbols () 
  (list '|Lisp 1.5| '|| '|will| '|NAME|))

;; special symbols & let  
(defun special-symbols-test () ; => 9
  (let ((|| 2)
        (|#\Newline| 3)
        (|42| 4))
    (+ || |#\Newline| |42|)))

;; define a package 
;; the pacakge my-utilities must exist before compile 
(defpackage "MY-APPLICATION"
  (:use "COMMON-LISP" "MY-UTILITIES")
  (:nicknames "APP")
  (:export "WIN" "LOSE" "DRAW"))

(in-package my-application)

;; error lexical variable is not bound to a symbol 
(defun variable-symbol-test ()
  (let ((v 42))
    (symbol-value 'v)))
