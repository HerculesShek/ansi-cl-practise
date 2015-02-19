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
(defmethod combine ((a cast-statue) b)
  (format nil "~A and ~A" (name a) b))
