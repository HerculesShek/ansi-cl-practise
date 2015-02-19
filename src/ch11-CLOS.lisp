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
  ((radius :accessor circle-radius)
   (center :accessor circle-center)))

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



