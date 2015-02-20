;;;; part of chapter 11 

;;; Encapsulation
(defpackage "CTR"
  (:use "COMMON-LISP")
  (:export "COUNTER" "INCREMENT" "CLEAR"))
(in-package ctr)

(defclass counter () ((state :initform 0)))

(defmethod increment ((c counter))
  (incf (slot-value c 'state)))

(defmethod clear ((c counter))
  (setf (slot-value c 'state) 0))

;; test 
(defpackage "MAIN"
  (:use "COMMON-LISP" "CTR")
  (:export "TEST"))
(in-package main)
(defun test()
  (let ((c (make-instance 'counter)))
    (slot-value c 'state))) ; 这里是错误的！封装起了很好的作用
