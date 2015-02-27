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
;;; 加入下面的这个代码是为了解决 make-load-form问题 
(defmethod make-load-form ((s ship) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots s))

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
(defmacro with-type (type expr)
  (or
   (leave-it expr)
   `(the ,type ,(if (atom expr) 
                    expr
                    (expand-call type (binarize expr))))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun leave-it (expr)
    (if (atom expr)
        (if (symbolp expr)
            (if (char= #\? (char (symbol-name expr) 0)) expr)
            expr)))

  (defun expand-call (type expr)
    `(,(car expr) ,@(mapcar #'(lambda (a)
                                `(with-type ,type ,a))
                            (cdr expr))))

  (defun binarize (expr)
    (if (and (nthcdr 3 expr)
             (member (car expr) '(+ - * /)))
        (destructuring-bind (op a1 a2 . rest) expr
          (binarize `(,op (,op ,a1 ,a2) ,@rest)))
        expr)))

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


;; ex4 Rewrite the breadth-first  search code in Section 3.15 so that it conses as little as possible. 
;;; breadth-first search
;;; 网络可以这么表示: '((a b c) (b c) (c d)) --关联列表
;;; 就是a节点可直达b和c，b可以直达c，c可以直达d
;;; 调用的时候 (shortest-path 'a 'd net) => (a c d)
;; 最短路径算法的入口
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;; 核心算法 广度优先搜索
(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (equal end node)
              (reverse path)
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))

;; 功能函数
(defun new-paths (path node net)
  (mapcar #'(lambda (x) (cons x path))
          (cdr (assoc node net))))
;; 测试函数
(defun bfs-test ()
  (let ((net '((a b c) (b c) (c d))))
    (shortest-path 'a 'd net)))

;; ex5 Modify the binary search tree code in Section 4.7 to use pools.

;;; BST -- Binary Search Tree
;; node 
(defstruct (node (:print-function
                  (lambda (node s d) 
                    (declare (ignore d))
                    (format s "#<~A>" (node-elt node)))))
  elt
  (l nil)
  (r nil))

;; pool storing the nodes of BST
(defconstant node-pool (make-array 2048 :fill-pointer t))

;; initialize the node-pool before fly
(dotimes (i 2048)
  (setf (aref pool i) (make-node)))

;; bst hashtable contains pairs [obj->node]
(defconstant bsth (make-hash-table :size 2048
                                   :test #'eq))
;; insert 非平衡的
;; obj---要查入到树中的元素的值
;; bst---二叉搜索树
;; <---表示的是比较函数 不是小于运算符
(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql elt obj) ; 已经存在此元素则不再插入 base case
            bst
            (if (funcall < obj elt)
                (make-node :elt elt
                           :l (bst-insert obj (node-l bst) <)
                           :r (node-r bst))
                (make-node :elt elt
                           :l (node-l bst)
                           :r (bst-insert obj (node-r bst) <)))))))
;; search
(defun bst-find (obj bst <)
  (if bst
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))
;; min
(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))
;; max 
(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))
;; remove min
(defun bst-remove-min (bst)
  (if (null (node-l bst))
      (node-r bst)
      (make-node :elt (node-elt bst)
                 :l (bst-remove-min (node-l bst))
                 :r (node-r bst))))
;; remove max 
(defun bst-remove-max (bst)
  (if (null (node-r bst))
      (node-l bst)
      (make-node :elt (node-elt bst)
                 :l (node-l bst)
                 :r (bst-remove-max (node-r bst)))))
;; remove obj from bst
(defun bst-remove (obj bst <)
  (if bst
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            (percolate bst)
            (if (funcall < obj elt)
                (make-node :elt elt
                           :l (bst-remove obj (node-l bst) <)
                           :r (node-r bst))
                (make-node :elt elt
                           :l (node-l bst)
                           :r (bst-remove obj (node-r bst) <)))))))
(defun percolate (bst)
  (let ((l (node-l bst)) (r (node-r bst)))
    (cond ((null l) r)
          ((null r) l)
          (t (if (zerop (random 2)) ;; 这里随机选择前驱或者是后继
                 (make-node :elt (node-elt (bst-max l))
                            :l (bst-remove-max l)
                            :r r)
                 (make-node :elt (node-elt (bst-max r))
                            :l l
                            :r (bst-remove-min r)))))))
;; print 前序
(defun print-bst (bst)
  (when bst
    (format t "~A <--~A--> ~A~%" (node-l bst) bst (node-r bst))
    (print-bst (node-l bst))
    (print-bst (node-r bst))))
;; 中序输出 排序
(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

;; BST的测试函数
(defun bst-test()
  (let ((nums))
    (progn
      (format t "insert 5 8 4 2 1 9 6 7 3 ...~%")
      (dolist (x '(5 8 4 2 1 9 6 7 3)) ; insert nums 
        (setf nums (bst-insert x nums #'<)))
      (format t "if 12 exists? ~A" 
              (if (bst-find 12 nums #'<) "yes" "no"))
      (format t "~%if 9 exists? ~A" 
              (if (bst-find 9 nums #'<) "yes" "no"))
      (format t "~%min:~t~A" (bst-min nums))
      (format t "~%max:~t~A" (bst-max nums))
      (format t "~%before remove:~%print:~%")
      (print-bst nums)
      (format t "bst-traverse: ")
      (bst-traverse #'(lambda (x) (format t "~A, " x)) nums)
      (setf nums (bst-remove 2 nums #'<))
      (format t "~%after remove 2:~%print:~%")
      (print-bst nums)
      (format t "bst-traverse:~%~T")
      (bst-traverse #'princ nums))))

