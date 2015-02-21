;;;; the solution of exercise 2 of chapter 11 

;;; Ray-Tracing
(defpackage "RAY-TRACING"
  (:use "COMMON-LISP")
  (:export "RAY-TEST"))
(in-package ray-tracing)

;;; Math utilities. 
;; returns the square of its argument.  
(defun sq (x) (* x x))

;; returns the magnitude of a vector given its x, y, and z components. 
;; This function is used in unit-vector and distance 
(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

;; returns three values representing the components of a unit vector--单位向量
;; with the same direction
(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defclass point ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (z :accessor z :initarg :z :initform 0)))

;; returns the distance between two points in 3-space.
(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

;; 一元二次方程求解取最小的解
(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a))))))))

;;; ray tracing 
;; The surface structure will be used to represent the objects in the simulated 
;; world. More precisely, it will be included in the structures defined to 
;; represent specific kinds of objects, like spheres. The surface structure itself 
;; contains only a single field: a color ranging from 0 (black) to 1 (white).
(defclass surface ()
  ((color :accessor surface-color :initarg color)))

(defparameter *world* nil)
(defconstant eye (make-instance 'point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at x y) p))))))
;;; 加入下面的这个代码是为了解决 make-load-form问题 
(defmethod make-load-form ((p point) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots p))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
                   (- y (y eye))
                   (- 0 (z eye)))
    (round (* (sendray eye xr yr zr) 255))))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
        (* (lambert s int xr yr zr) (surface-color s))
        0)))

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
        (when h
          (let ((d (distance h pt)))
            (when (or (null dist) (< d dist))
              (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

;;; Spheres
(defclass sphere (surface)
  ((radius :accessor sphere-radius :initarg :radius :initform 0)
   (center :accessor sphere-center :initarg :center 
           :initform (make-instance 'point :x 0 :y 0 :z 0))
   (color :initarg :color)))

(defun defsphere (x y z r c)
  (let ((s (make-instance 'sphere
                          :radius r
                          :center (make-instance 'point :x x :y y :z z)
                          :color c)))
    (push s *world*)
    s))

(defmethod intersect ((s sphere) (pt point) xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n (make-instance 'point 
                         :x (+ (x pt) (* n xr))
                         :y (+ (y pt) (* n yr))
                         :z (+ (z pt) (* n zr))))))

(defmethod normal ((s sphere) (pt point))
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))

;;; ray tracing test 
(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
        ((> z 8))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))


