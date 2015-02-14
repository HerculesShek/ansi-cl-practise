;;;; source file for chapter 9 of ANSI Common Lisp

;; upgrade version of mirror?
(defun palindrome? (x)
  (let ((mid (/ (length x) 2)))
    (equal (subseq x 0 (floor mid))
           (reverse (subseq x (ceiling mid))))))

;; out truncate
(defun our-truncate (n &optional (d 1))
  (if (> (/ n d) 0)
      (floor n d)
      (ceiling n d)))


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

(defstruct (point (:conc-name nil))
  x y z)

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
(defstruct surface color)

(defparameter *world* nil)
(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at x y) p))))))

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
(defstruct (sphere (:include surface))
  radius center)

(defun defsphere (x y z r c)
  (let ((s (make-sphere :radius r
                        :center (make-point :x x :y y :z z)
                        :color c)))
    (push s *world*)
    s))

(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
           s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-point :x (+ (x pt) (* n xr))
                    :y (+ (y pt) (* n yr))
                    :z (+ (z pt) (* n zr))))))

(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
           s pt))

(defun sphere-normal (s pt)
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


;;; Exercises 
(defpackage "CH9-EX"
  (:use "COMMON-LISP")
  (:exoprt ""))
(in-package ch9-ex)

;; ex1 takes a list of reals and returns true iff they are 
;; in nondecreasing order.
(defun non-decreasingp (lst)
  (and (every #'realp lst)
       (apply #'<= lst)))

;; ex2  takes an integer number of cents and returns four 
;; values showing how to make that number out of 25-, 10-, 5- and 
;; 1-cent pieces, using the smallest total number of coins. 
(defun coins (n &optional (model '(25 10 5)) res)
  (if model
      (multiple-value-bind (n r) (floor n (car model))
        (coins r (cdr model) (cons n res)))
      (nreverse (cons n res))))


;; ex3 
(defun comp-res (&optional (y 10))
  (if (> y 0)
      (let ((n (+ (random 3) 4)))
        (format t "WIGGLIES : ~A, " n)
        (format t "WOBBLIES : ~A~%" (- 10 n))
        (comp-res (1- y)))))
  
;; ex4 给出4个点的坐标表示2个线段，判断是否相交，如果相交，返回相交的点
;; p(x1, y1) q(x3, y3)
;; 这个题目十分有意思！！算法参考 http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
;; 但是他的回答的第一条是不合理的，我会针对此问题写一篇博客
(defun segment-intersect (x1 y1 x2 y2 x3 y3 x4 y4)
  (labels ((D2-cross-product (px1 py1 px2 py2)
             (- (* px1 py2) (* py1 px2)))
           (D2-vector-product (px1 py1 px2 py2)
             (+ (* px1 px2) (* py1 py2)))
           (same-direction? (px1 py1 px2 py2)
             (if (zerop px1)
                 (if (zerop py1)
                     (error "zero vector!")
                     (eql (signum py1) (signum py2)))
                 (= (signum px1) (signum px2)))))
    (if (or (and (= x1 x2) (= y1 y2))
            (and (= x3 x4) (= y3 y4)))
        (error "not 2 segments!!!"))
    (let* ((r-x (- x2 x1))
           (r-y (- y2 y1))
           (s-x (- x4 x3))
           (s-y (- y4 y3))
           (pq-x (- x3 x1))
           (pq-y (- y3 y1))
           (qp-x (- pq-x))
           (qp-y (- pq-y))
           (rXs (D2-cross-product r-x r-y s-x s-y))
           (pqXr (D2-cross-product pq-x pq-y r-x r-y)))
      (if (zerop rXs) ; 方向相同或者相反
          (if (zerop pqXr) ; 共线
              (if (same-direction? r-x r-y s-x s-y) ; 共线并且同向
                  (let ((pq-dot-r (D2-vector-product pq-x pq-y r-x r-y))
                        (r-dot-r (D2-vector-product r-x r-y r-x r-y)))
                    (if (<= 0 pq-dot-r r-dot-r)
                        (cond 
                          ((= 0 pq-dot-r) ; 两个线段的起点一样
                           (if (> (D2-vector-product r-x r-y r-x r-y) 
                                  (D2-vector-product s-x s-y s-x s-y))
                               (values "overlap" x3 y3 x4 y4)
                               (values "overlap" x1 y1 x2 y2)))
                          ((= pq-dot-r r-dot-r) (values "overlap at one point" x2 y2 nil nil))
                          (t (segment-intersect x3 y3 x2 y2 x3 y3 x4 y4)))
                        (let ((qp-dot-s (D2-vector-product qp-x qp-y s-x s-y))
                              (s-dot-s (D2-vector-product s-x s-y s-x s-y)))
                          (if (<= qp-dot-s s-dot-s)
                              (segment-intersect x3 y3 x4 y4 x1 y1 x2 y2)
                              (values "collinear but disjoint" nil nil nil nil)))))
                  (segment-intersect x1 y1 x2 y2 x4 y4 x3 y3)) ; 共线不同向
              (values "parallel and non-intersecting" nil nil nil nil)) ; 平行 但是不共线
          (let ((scalar-t (/ (D2-cross-product pq-x pq-y s-x s-y) rXs)) ; 两个线段所在的直线是相交的
                (scalar-u (/ (D2-cross-product pq-x pq-y r-x r-y) rXs)))
            (if (and (<= 0 scalar-t 1) (<= 0 scalar-u 1))
                (values "meet" (+ x1 (* scalar-t r-x)) (+ y1 (* scalar-t r-y)) nil nil)
                (values "not parallel but do not intersect" nil nil nil nil)))))))


;; ex5 Suppose f is a function of one (real) argument, and that min and 
;; max are nonzero reals with different signs such that f has a 
;; root (returns zero) for one argument i such that min < i < max.  
;; Define a function that takes four arguments, f, min, max, and epsilon,  
;; and returns an approximation of i accurate to within plus or minus  
;; epsilon. 
;; 假使函数f接受一个实数，min和max是非零的不同符号的2个实数：min<0<max，有个
;; 实数i属于(min, max)--开区间，使得f(i)=0，也就是i是函数f的根。
;; 定义一个函数，接受4个参数：f, min, max, epsilon，返回i的近似值，误差在
;; epsilon之内

;;; 其实这个问题的原述不合理，应该是f(minx)<0<f(max)或者f(max)<0<f(min)，并
;;; 且，min和max之间i不一定是函数f的唯一的根
(defun approx (f min max epsilon)
  (let* ((range (- max min))
         (mid (+ min (/ range 2))))
    (if (< 0 (* (funcall f min) (funcall f max)))
        (error "wrong f or wrong range (min, max) !!!"))
    (if (< range epsilon)
        min
        (let* ((min-val (funcall f min))
               (mid-val (funcall f mid))
               (flag (* min-val mid-val)))
          (cond
            ((= mid-val 0) mid)
            ((< flag 0) (approx f min mid epsilon))
            (t (approx f mid max epsilon)))))))

;; test 
(defun approx-test ()
   (approx #'(lambda (x) x) -2.4 1 0.05))


;; ex6 
(defun horner (x &rest parms)
  (labels ((rec (parms acc)
             (if parms
                 (rec (cdr parms) (+ (* acc x) (car parms)))
                 acc)))
    (rec parms 0)))

;; ex7 
(defun fixnums-bits()
  (log (1+ most-positive-fixnum) 2))

;; ex8
(defun float-types ()
  (
