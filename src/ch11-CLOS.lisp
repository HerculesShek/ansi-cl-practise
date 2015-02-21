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

;; ex2 ch11-CLOS-ex2.lisp

;; ex3 


