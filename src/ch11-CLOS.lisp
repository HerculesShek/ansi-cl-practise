;;;; source file for chapter 11 of ANSI Common Lisp

;;; structures and functions
(defstruct rectangle
  height width)

(defstruct circle
  radius)

(defun area (x)
  (cond ((rectangle-p x)
         (* (rectangle-height x) (rectangle-width x)))
        ((circle-p x)
         (* pi (expt (circle-radius x) 2)))))

(defun area-test ()
  (let ((r (make-rectangle))
        (c (make-circle)))
    (setf (rectangle-height r) 2
          (rectangle-width r) 3
          (circle-radius c) 1)
    (format t "~A, ~A, ~A" (area r) (area c) (area 'a))))


;;; an equivalent program using CLOS
;;; Area with classes and methods
(defclass crectangle ()
  (height width))

(defclass ccircle ()
  ((radius :reader r-radius
           :writer w-radius
           :accessor a-radius
           :initarg :radius
           :initform 1.0)
   (center :accessor c-center
           :initarg :center
           :initform (cons 0.0 0.0))))

(defmethod carea ((x crectangle))
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod carea ((x ccircle))
  (* pi (expt (slot-value x 'radius) 2)))

(defun carea-test ()
  (let ((r (make-instance 'crectangle))
        (c (make-instance 'ccircle)))
    (setf (slot-value r 'height) 2
          (slot-value r 'width) 3
          (slot-value c 'radius) 1)
    (format t "~A ~A" (carea r) (carea c))))

;;; colored circle 
(defclass colored ()
  (color))

(defclass colored-circle (ccircle colored)
  ())

;; test default slot value 
(defun test-default-slot ()
  (let ((c (make-instance 'colored-circle)))
    (slot-value c 'color))) ; error !

;; shared slot 
(defclass tabloid ()
  ((top-story :accessor tabloid-story
              :allocation :class)))
;; shred slot test 
(defun shared-slot-test ()
  (let ((daily-blab (make-instance 'tabloid))
        (unsolicited-mail (make-instance 'tabloid)))
    (setf (tabloid-story daily-blab) 'adultery-of-senator)
    (tabloid-story unsolicited-mail)))

(defclass graphic ()
  ((color :accessor graphic-color :initarg :color)
   (visible :accessor graphic-visible :initarg :visible
            :initform t)))
(defclass screen-circle (ccircle graphic) 
  ((color :initform 'purple)))


;;; precedence
(defclass sculpture () (height width depth)) ; 雕塑

(defclass statue (sculpture) (subject)) ; 雕像

(defclass metalwork () (metal-type)) ; 金属制品

(defclass casting (metalwork) ()) ; 铸件

(defclass cast-statue (statue casting) ()) ; 青铜雕像

(defmethod value ((x sculpture)) 1)
(defmethod value ((x casting)) 4)

;;; precedence test
(defmethod add ((a integer) (b integer))
  (+ a b))
(defmethod add ((a (eql 3)) b)
  (+ a b 10))
(defmethod add ((a integer) (b symbol))
  42)
(defun pre-test () ; 17 7 error! The value A is not of the expected type NUMBER.
  (format t "~A ~A ~A" (add 3 4) (add 4 3) (add 3 'a)))
;; 直观上看 (add 3 'a)应该调用第三个add方法，但是结果是调用的第二个！因为'a同时属于symbol和t类型
;; 的情况下，有个eql满足的优先级会高很多！

;;; Generic Fuctions 
(defmethod combine (x y)
  (list x y))
(defclass stuff () ((name :accessor name :initarg :name)))
(defclass ice-cream (stuff) ())
(defclass topping (stuff) ())

;; 现在下面是替 combine 定义的第二个方法：
(defmethod combine ((ic ice-cream) (top topping))
  (format nil "~A ice-cream with ~A topping."
          (name ic)
          (name top)))
(defmethod combine ((ic ice-cream) x)
  (format nil "~A ice-cream with ~A."
          (name ic)
          x))
(defmethod combine ((x number) (y number))
  (+ x y))
(defmethod combine ((x (eql 'powder)) (y (eql 'spark)))
  'boom)

;;; Generic Fuctions Test
(defmethod combine ((a cast-statue) (b t))
  1)
(defmethod combone ((a statue) (b casting))
  2)

;;; Auxiliary Methods 
(defclass speaker () ())

(defmethod speak ((s speaker) string)
  (format t "~A" string))

(defclass intellectual (speaker) ())

(defmethod speak :before ((i intellectual) string)
  (princ "Perhaps "))

(defmethod speak :after ((i intellectual) string)
  (princ " in some sense"))

(defmethod speak :before ((s speaker) string)
  (princ "I think "))

(defmethod speak :after ((s speaker) string)
  (princ " indeed"))

(defun Auxiliary-test ()
  (speak (make-instance 'intellectual)
         "I'm hungry "))

;;; 奉承者
(defclass courtier (speaker) ())

(defmethod speak :around ((c courtier) string)
  (format t "Does the King believe that ~A?" string)
  (if (eql (read) 'yes)
      (if (next-method-p) (call-next-method))
      (format t "Indeed, it is a preposterous idea. ~%"))
  'bow)


;;; Method Combination
(defgeneric price (x)
  (:method-combination +))

(defclass jacket () ()) ; 夹克
(defclass trousers () ()) ; 裤子
(defclass suit (jacket trousers) ()) ; 套装

(defmethod price + ((jk jacket)) 350)
(defmethod price + ((tr trousers)) 200)
(defmethod price + ((s suit)) 500)

(defun m-c-test ()
  (price (make-instance 'suit)))

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


;;; Exerceses
;; ex1 
(defclass rectangle ()
  ((height :accessor rect-height :initform 0 :initarg :height) 
   (width :accessor rect-width :initform 0 :initarg :width)))

(defclass circle ()
  ((radius :accessor circle-radius :initform 0 :initarg :radius)))

(defmethod area ((x rectangle))
  (* (rect-height x) (rect-width x)))

(defmethod area ((x circle))
  (* pi (expt (circle-radius x) 2)))

;; ex2 go to ch11-CLOS-ex2.lisp

;; ex3 
;;                h
;;               / \ 
;;           e  f   g 
;;            \  \ /
;;             \  |
;;           c   d
;;            \ /
;;             a
;; order of specification (descending): a, c, d, e, f, g, h
;;                h
;;               / \ 
;;           e  f   g 
;;            \  \ /
;;             \  |
;;               d   c
;;                \ /
;;                 b
;; order of specification (descending): b, d, e, f, g, h, c

;; ex4 Suppose that you already have the following  functions; 
;; `precedence`: takes an object and returns its precedence list, a list of 
;;   classes ordered from most specific to least specific.  
;; `methods`:  takes a generic function and returns a list of all its methods. 
;; `specializations`:  takes a method and returns a list of the specializations 
;;   of the parameters. Each element of the returned list will be either a 
;;   class, or a list of the form (eql x), or t (indicating that the 
;;   parameter is unspecialized). 
;; Using these functions (and not compute-applicable-methods or find-method),  
;; define a function most-spec-app-meth that takes a generic function and a 
;; list of the arguments with which it has been called, and returns the most 
;; specific applicable method, if any. 
(defun score-it (meth classlist args)
  (let ((specs (specialization meth))
        (score 0))
    (if (/= (length specs) (length classlist))
        nil
        (do ((i 0 (1+ i))
             (spec (car specs) (car spes))
             (classes (car classlist) (car clist))
             (spes (cdr specs) (cdr spes))
             (clist (cdr classlist) (cdr clist)))
            ((not spec) score)
          (if (and (consp spec) (eql (car spec) 'eql))
              (decf score most-positive-fixnum)
              (let ((p (position spec classes)))
                (if p
                    (incf score (* 10 (1+ p)))
                    (return-from score-it nil))))))))

(defun most-spec-app-meth (gf args)
  (let* ((classlist (mapcar #'precedence args)) ; 实参的所有的类别的优先级
         (all-meths (method gf))
         (curr-score (score-it (car all-meths) classlist args))
         (curr-meth (if curr-score (car all-meths) nil)))
    (dolist (meth (cdr all-meths))
      (let ((score (score-it meth classlist args)))
        (if (and score (< score curr-score))
            (setf curr-score score curr-meth meth))))
    curr-meth))
;; TODO 自己写三个函数 precedence method specialization
   
;; ex5
(defparameter *area-counter* 0)
(defmethod area :before (obj)
  (declare (ignore obj))
  (incf *area-counter*))

;; ex7 If a function contains several classes as arguments. For example, 
;; the following method cannot be defined by the message passing model.
(defclass stuff () ((name :accessor name :initarg :name)))
(defclass ice-cream (stuff) ())
(defclass topping (stuff) ())

;; 现在下面是替 combine 定义的第二个方法：
(defmethod combine ((ic ice-cream) (top topping))
  (format nil "~A ice-cream with ~A topping."
          (name ic)
          (name top)))
