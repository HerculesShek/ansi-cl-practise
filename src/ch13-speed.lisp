;;;; this file contains demos and exercise of chapter 13

;; If a major bottleneck occurred in the inner loop of some function, add a declaration
(defun bottleneck (&rest xx)
  (do ()
      ()
    (do ()
        ()
      (declare (optimize (speed 3) (safety 0)))
      )))

;; To ask globally for the fastest possible code, regardless of the consequences,
(declaim (optimize (speed 3)
                   (compilation-speed 0)
                   (safety 0)
                   (debug 0)))

;; not tail call
(defun length/r (lst)
  (if (null lst)
      0
      (1+ (length/r (cdr lst)))))
;; tail call 
(defun length/rt (lst)
  (labels ((len (lst acc)
             (if (null lst)
                 acc
                 (len (cdr lst) (1+ acc)))))
    (len lst 0)))


;; inline function
(declaim (inline single?))

(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun foo (x)
  (single? (bar x)))
(defun foo (x)
  (let ((lst (bar x)))
    (and (consp lst) (null (cdr lst)))))
(defun bar (x)
  (list (+ 1 x) (+ 2 x)))

(declaim (type fixnum *count*))
(declaim (fixnum *count*))

(defun poly (a b x)
  (declare (fixnum a b x))
  (+ (* a (expt x 2)) (* b x)))

(defun poly (a b x)
  (declare (fixnum a b x))
  (the fixnum (+ (the fixnum (* a (the fixnum (expt x 2))))
                 (the fixnum (* b x)))))


(setf x (vector 1.234d0 2.345d0 3.456d0)
      y (make-array 3 :element-type 'double-float)
      (aref y 0) 1.234d0
      (aref y 1) 2.345d0
      (aref y 2) 3.456d0)


(declare (type (vector fixnum 20) v))

(setf a (make-array '(1000 1000)
                    :element-type 'single-float
                    :initial-element 1.0s0))

(defun sum-elts (a)
  (declare (type (simple-array single-float (1000 1000)) a))
  (let ((sum 0.0s0))
    (declare (type single-float sum))
    (dotimes (r 1000)
      (dotimes (c 1000)
        (incf sum (aref a r c))))
    sum))

;;;  Generating a rhyming dictionary. 
(defconstant dict (make-array 25000 :fill-pointer 0))

(defun read-words (from)
  (setf (fill-pointer dict) 0)
  (with-open-file (in from :direction :input)
    (do ((w (read-line in nil :eof)
            (read-line in nil :eof)))
        ((eql w :eof))
      (vector-push w dict))))

(defun xform (fn seq) (map-into seq fn seq))

(defun write-words (to)
  (with-open-file (out to :direction :output
                       :if-exists :supersede)
    (map nil #'(lambda (x)
                 (fresh-line out)
                 (princ x out))
         (xform #'nreverse
                (sort (xform #'nreverse dict)
                      #'string<)))))

;; value last longer than the variable
(defun our-reverse (lst)
  (let ((rev nil))
    (dolist (x lst)
      (push x rev))
    rev))

;; variable's value need not last any longer than the variable does. 
(defun our-adjoin (obj lst &rest args)
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))
(defun our-adjoin (obj lst &rest args)
  (declare (dynamic-extent args))
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))

;;; Pools
;; ship structure
(defstruct ship
  name flag tons)
;; V1 quick prototype of a program
(defparameter *harbor* nil)

(defun enter (n f d)
  (push (make-ship :name n :flag f :tons d)
        *harbor*))

(defun find-ship (n)
  (find n *harbor* :key #'ship-name))

(defun leave (n)
  (setf *harbor*
        (delete (find-ship n) *harbor*)))

;; V2 without consing in fly 
(defconstant pool (make-array 1000 :fill-pointer t))

(dotimes (i 1000)
  (setf (aref pool i) (make-ship)))

(defconstant harbor (make-hash-table :size 1100
                                     :test #'eq))

(defun enter (n f d)
  (let ((s (if (plusp (length pool))
               (vector-pop pool)
               (make-ship))))
    (setf (ship-name s)        n
          (ship-flag s)        f
          (ship-tons s)        d
          (gethash n harbor) s)))

(defun find-ship (n) (gethash n harbor))

(defun leave (n)
  (let ((s (gethash n harbor)))
    (remhash n harbor)
    (vector-push s pool)))


;;; Exercises 
;; ex1 
(declaim (inline my-add))
(defun my-add (n)
  (+ n 1))

(defun call-my-add (n)
  (my-add n))
(defun see-res ()
  (disassemble 'call-my-add))

;; ex2 
;; original
(defun foo (x)
  (if (zerop x)
      0
      (1+ (foo (1- x)))))
;; tail call 
(defun foo-t (x)
  (labels ((f (x res)
             (if (zerop x)
                 res
                 (f (1- x) (1+ res)))))
    (f x 0)))

(defun test-foo ()
  (time (foo 20000))
  (time (foo-t 20000)))

;; ex3 
;; ex3-a Add declarations to The date arithmetic code in Section 5.7. 
;;; Date Arithmetic
;; 借助这个向量 [start, end) 
(defconstant month #(0 31 59 90 120 151 181 212 243 273 304 334 365))
;; the year of 2000 is based 约定2000年1月1日为坐标原点，为第0天
(defconstant yzero 2000)
;;; first part : transfer date to a number 
;;; (day->num 5 2 2015) => 5514
(defun date->num (d m y)
  (declare (type fixnum d m y))
  (and (date-validp d m y)
       (+ (- d 1) (month-num m y) (year-num y))))
;; leap year?
(defun leap? (y)
  (declare (type fixnum y))
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))
;;判断日期的合法性 
(defun date-validp (d m y)
  (declare (type fixnum d m y))
  (and (every #'integerp (list d m y))
       (< 0 m 13)
       (case m
         ((1 3 5 7 8 10 12) (< 0 d 32))
         ((4 6 9 11) (< 0 d 31))
         ((2) (< 0 d (if (leap? y) 30 29))))))
;; how many days before the month of m in the year of y
(defun month-num (m y)
  (declare (type m y))
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))
;; how many days between the year of y and 2000
(defun year-num (y)
  (let ((d 0))
    (declare (type fixnum d y))
    (if (>= y yzero)
        (dotimes (i (- y yzero) d)
          (incf d (year-days (+ yzero i))))
        (dotimes (i (- yzero y) (- d))
          (incf d (year-days (+ y i)))))))
;; how many days in the year of y
(defun year-days (y)
  (declare (type fixnum y))
  (if (leap? y) 366 365))

;;; second part: transfer a number to a date
;;; num -> date
(defun num->date (n)
  (declare (type fixnum n))
  (and (integerp n)
       (multiple-value-bind (y left) (num-year n)
         (multiple-value-bind (d m) (num-month left y)
           (values d m y)))))
;; use a number to calculate the year
(defun num-year (n)
    (declare (type fixnum n))
  (if (> n 0)
      (do* ((y yzero (+ y 1))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev))))
      (do* ((y (- yzero 1) (- y 1))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d))))))
;; use a number to calculate the month 
(defun num-month (n y)
    (declare (type fixnum n y))
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
            ((> n 59) (nmon (- n 1)))
            (t (nmon n)))
      (nmon n)))
;; get the month and day
(defun nmon (n)
  (declare (type fixnum n))
  (let ((m (position n month :test #'<)))
    (values (+ 1 (- n (svref month (- m 1)))) m)))
;; add n day to a date
(defun date+ (d m y n)
  (declare (type fixnum d m y n))
  (num->date (+ n (date->num d m y))))


;; ex3-b  Add declarations to The ray-tracer in Section 9.8. 


;; ex4 Rewrite the breadth-first  search code in Section 3.15 so that it conses as little as possible. 


;; ex5 Modify the binary search tree code in Section 4.7 to use pools.


