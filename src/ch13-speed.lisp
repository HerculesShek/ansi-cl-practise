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
