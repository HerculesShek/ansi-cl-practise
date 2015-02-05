;;;; this source file contains all demos and exercises of chapter 5

;; return test
(dolist (x '(a b c) x)
  (format t "~A" x)
  (if (eql x 'c)
      (return 42))) ; (return-from nil 42)))

;; 测试默认的return在嵌套的隐式的nil block中的行为
;; 是只返回给最近的那个nil block
(defun nest-dolist-test ()
  (dolist (x '((1 2 3) (4 5 6) (7 8 9)) nil)
    (dolist (y x)
      (format t "~A~T" y)
      (if (eql y 5)
          (return 42)))))

;; str->integer :return-from
(defun str->integer (str)
  (let ((accum 0))
    (dotimes (pos (length str))
      (let ((i (digit-char-p (char str pos))))
        (if i
            (setf accum (+ (* accum 10) i))
            (return-from str->integer nil))))
    accum))

;; tagbody test 
(defun tagbody-test ()
  (let ((x 0))
    (tagbody
     top
       (incf x)
       (format t "~A " x)
       (if (< x 10) 
           (go top)))))

;; cond and if
(defun our-member (obj lst)
  (if (atom lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst)))))
(defun our-member-cond (obj lst)
  (cond ((atom lst) nil)
        ((eql obj (car lst)) lst)
        (t (our-member-cond obj (cdr lst)))))

;; case 
(defun month-length (mon)
  (case mon
    ((jan mar may jul aug oct dec) 31)
    ((apr jun sept nov) 30)
    (feb (if (leap? 
              (progn (format t "Please enter the year:~%") (read))) 
             29 28))
    (otherwise "unknown month")))
;; leap year?
(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

;; typecase
(defun what-is-it (x)
  (format t "~&~S is ~A.~%"
          x (typecase x
              (simple-base-string "a string")
              (integer "an integer")
              (float "a float")
              (null "a symbol, boolean false, or the empty list")
              (list "a list")
              (hash-table "a hash-table")
              (t (format nil "a(n) ~(~A~)" (type-of x))))))
(defun wii-test ()
  (map nil #'what-is-it (list "abc" 99 'a 33.23 3/5 :rt :4 #\a nil
                              (make-hash-table)
                              (vector 1 2)
                              (list "a" 'b)
                              (make-array '(2 2)))))


;;; do 测试return go return-from 
;; > (do-return-test )
;; i is 1
;; i is 2
;; i is 3
;; 99
(defun do-return-test ()
  (do ((i 1 (incf i)))
      ((= i 10) 'done)
    (format t "i is ~A~%" i)
    (if (= i 3)
        (return 99))))
;; > (do-return-from-test )
;; i is 1
;; i is 2
;; i is 3
;; 99
(defun do-return-from-test ()
  (do ((i 1 (incf i)))
      ((= i 10) 'done)
    (format t "i is ~A~%" i)
    (if (= i 3)
        (return-from nil 99))))
;; 打印九九乘法表
;; 这说明do的body部分是在一个tagbody中的
;; tagbody给出了在一层循环中写双层循环的方式，但是不推荐使用
(defun do-go-test ()
  (do ((i 1 (incf i)))
      ((= i 10) 'done)
    (setf j 0)
   top
    (incf j)
    (format t "~A*~A=~A~2t" j i (* i j))
    (when (< j i)
      (go top))
    (format t "~%")))

;;; 下面的3个测试充分说明了do宏中局部变量的关系 
;;; 以及局部变量和外部变量的关系
;; > (do-var-test)
;; (1 A) (2 1) (3 2) (4 3) (5 4) 
;; A
(defun do-var-test ()
  (let ((x 'a))
    (do ((x 1 (+ x 1))
         (y x x))
        ((> x 5))
      (format t "(~A ~A) " x y))
    x))
(defun do-test-1 () 
  (do ((m 1 (+ m 1))
       (n m m)) ;; ERROR! m is Undeclared!
      ((> m 5))
    (format t "(~A ~A) " m n)))
(defun do-test-2 ()
  (let ((x 'a))
    (do ((x 1 (incf x)) ;; incf modifid the old value after last iteration
         (y x x))       ;; so y is the new value of x
        ((> x 5))
      (format t "(~A ~A) " x y))
    x))

;; do* test
(defun do*-test ()
  (do* ((x 1 (+ y 1))
        (y (+ x 1) (+ 1 x)))
       ((> x 5))
    (format t "(~A ~A) " x y)))
(defun do*-test-1 ()
  (do* ((y x (+ 1 x)) ;; 错误！因为是顺序执行的 
        (x 1 (+ y 1)))
       ((> x 5))
    (format t "(~A ~A) " x y)))

;; mapc
(defun mapc-test()
  (mapc #'(lambda (x y z)
            (format t "~A ~A ~S, " x y z))
        '(hip flip slip)
        '(hop flop slop top)
        '(1 2 3 4)))

;; values 是访问器 个人认为作为访问器意义不大
(defun values-test ()
  (setf (values a b c) (values 42 "Jack" :good))
  (list a b c))

;; multiple-value-bind test print time 
(defun multiple-value-bind-test ()
  (multiple-value-bind (s m h) (get-decoded-time)
    (format t "time is ~2,'0d:~2,'0d:~2,'0d" h m s)))

;; :catch :throw
(defun super ()
  (catch 'abort
    (sub)
    (format t "We'll never see this.")))
(defun sub ()
  (throw 'abort 99))

;; :error
(defun factorial (x)
  (cond ((or (not (typep x 'integer)) (minusp x))
         (error "~S is not a valid argument to FACTORIAL." x))
        ((zerop x) 1)
        (t (* x (factorial (- x 1))))))

;; :unwind-protect test #1
(defun unwind-protect-test ()
  (let ((var 10))
    (setf var (catch 'cat
                (upt-sub var)
                12))
    var))
(defun upt-sub (var)
  (unwind-protect
       (throw 'cat 13)
    (incf var)
    (format t "The value of var is ~A~%" var)))

;; unwind-protect test #2
(defun up-test ()
  (let ((n 10))
    (setf n (catch 'up
              (u-sub n)
              100))
    n))
(defun u-sub (n)
  (unwind-protect
       (throw 'up (incf n 5))
    (format t "u-sub is still going...~%")
    (format t "n is ~A" (incf n 3))))

;; unwind-protect test #3
(defun up-test-lst ()
  (let ((lst '(10)))
    (setf lst (catch 'up
                (u-sub-lst lst)
                42))
    lst))
(defun u-sub-lst (lst)
  (unwind-protect
       (throw 'up (push 40 lst))
    (format t "u-sub-lst is still going....~%")
    (format t "n is ~A" (incf (car lst) 2))))


;; unwind-protect test #4
(defun up-test-list ()
  (let ((lst '(10)))
    (setf lst (catch 'up
                (u-sub-list lst)
                42))
    lst))
(defun u-sub-list (lst)
  (unwind-protect
       (format t "u-sub-list is still going....~%")
    (throw 'up (push 40 lst))
    (format t "n is ~A" (incf (car lst) 2))))


;;; Date Arithmetic
;; 借助这个向量 [start, end) 
(defconstant month #(0 31 59 90 120 151 181 212 243 273 304 334 365)) 
;; the year of 2000 is based 约定2000年1月1日为坐标原点，为第0天
(defconstant yzero 2000)
;;; first part : transfer date to a number 
;;; (day->num 5 2 2015) => 5514
(defun date->num (d m y)
  (and (date-validp d m y)
       (+ (- d 1) (month-num m y) (year-num y))))
;;判断日期的合法性 
(defun date-validp (d m y)
  (and (every #'integerp (list d m y))
       (< 0 m 13)
       (case m
         ((1 3 5 7 8 10 12) (< 0 d 32))
         ((4 6 9 11) (< 0 d 31))
         ((2) (< 0 d (if (leap? y) 30 29))))))
;; how many days before the month of m in the year of y
(defun month-num (m y)
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))
;; how many days between the year of y and 2000
(defun year-num (y)
  (let ((d 0))
    (if (>= y yzero)
        (dotimes (i (- y yzero) d)
          (incf d (year-days (+ yzero i))))
        (dotimes (i (- yzero y) (- d))
          (incf d (year-days (+ y i)))))))
;; how many days in the year of y
(defun year-days (y)
  (if (leap? y) 366 365))

;;; second part: transfer a number to a date
;;; num -> date
(defun num->date (n)
  (and (integerp n)
       (multiple-value-bind (y left) (num-year n)
         (multiple-value-bind (d m) (num-month left y)
           (values d m y)))))
;; use a number to calculate the year
(defun num-year (n)
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
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
            ((> n 59) (nmon (- n 1)))
            (t (nmon n)))
      (nmon n)))
;; get the month and day
(defun nmon (n)
  (let ((m (position n month :test #'<)))
    (values (+ 1 (- n (svref month (- m 1)))) m)))
;; add n day to a date
(defun date+ (d m y n)
  (num->date (+ n (day->num d m y))))


;;; Exercises
;; 1-a
((lambda (x) (cons x x)) (car y))
;; 1-b
((lambda (w)
   (lambda (y)
     (cons w y))
   (+ w 2))
 (car x))

;; 2 获取x在y中首次出现的位置
(defun mystery (x y)
  (cond 
    ((null y) nil)
    ((eql (car y) x) 0)
    (t (let ((z (mystery x (cdr y))))
         (and z (+ z 1))))))

;; 3
(defun square (x)
  (if (and (< 0 x 6) (integerp x))
      x
      (* x x)))

;; 4 Rewrite num-month to use case instead of svref.
(defun month-num-case(m y)
	(+ (case m
       (1 0)
       (2 31)
       (3 59)
       (4 90)
       (5 120)
       (6 151)
       (7 181)
       (8 212)
       (9 243)
       (10 273)
       (11 304)
       (12 334)
       (13 365))
     (if (and (> m 2) (leap? y)) 1 0)))

;;; ex5 定义一个迭代与递归版本的函数，接受一个对象x与向量v，并返回一个
;;; 列表，包含了向量v当中，所有直接在x之前的对象：
;; ex5 iteration
;; process string sequece and vector 
;; transfer string or sequence to vector because of using svref 
;; 可以看出，string或者是sequence都可以转为simple-vector，代码中的
;; 'vector 可以换作 ’simple-vector 
(defun precedes-iter (obj v)
  (typecase v
    (simple-base-string (precedes-iter obj (concatenate 'vector v)))
    (vector
     (let (pres
           (len (length v)))
       (dotimes (i (1- len) pres)
         (if (eql (svref v (1+ i)) obj)
             (pushnew (svref v i) pres))))) ; avoid duplicates 
    (sequence (precedes-iter obj (concatenate 'vector v)))))
;; ex5 recursive
(defun precedes-recur (obj v)
  (typecase v
    (simple-base-string 
     (pre obj (concatenate 'vector v) (- (length v) 1) 0 nil))
    (vector 
     (pre obj v (- (length v) 1) 0 nil))
    (sequence 
     (pre obj (concatenate 'vector v) (- (length v) 1) 0 nil))))
(defun pre (obj v len n pres)
  (if (= n len)
      pres
      (progn 
        (if (eql obj (svref v (+ 1 n)))
            (pushnew (svref v n) pres))
        (pre obj v len (+ n 1) pres))))

;;; ex6 定义一个迭代与递归版本的函数，接受一个对象与列表，并返回
;;; 一个新的列表，在原本列表的对象之间加上传入的对象
;; ex6 iteration
(defun intersperse (obj lst)
  (let (res)
    (dolist (e lst)
      (push e res)
      (push obj res))
    (reverse (subseq res 1))))
;; ex6 recursive
(defun intersperse-r (obj lst)
  (if (consp lst)
      (cons (car lst) (inter obj (cdr lst)))))
(defun inter (obj lst)
  (and lst
       (append (list obj (car lst)) (inter obj (cdr lst)))))

;; ex7 (a) 递归
(defun compare-pair-recur (lst)
  (and (listp lst)
       (every #'numberp lst)
       (< 1 (length lst))
       (cpr-u lst)))
(defun cpr-u (lst)
  (or (null (cdr lst))
      (and (= 1 (abs (- (car lst) (cadr lst))))
           (cpr-u (cdr lst)))))
;; ex7 (b) do 
(defun compare-pair-do (lst)
  (and (listp lst)
       (every #'numberp lst)
       (< 1 (length lst))
       (do ((rest (cdr lst) (cdr rest))
            (flag (= 1 (abs (- (car lst) (cadr lst))))
                  (= 1 (abs (- (car rest) (cadr rest))))))
           ((or (not flag) (null (cdr rest))) flag))))
;; ex7 (c) mapc & return
(defun compare-pair-mapc (lst)
  (and (listp lst)
       (every #'numberp lst)
       (< 1 (length lst))
       (block nil
         (let ((curr (car lst)))
           (mapc #'(lambda (x)
                     (if (= 1 (abs (- curr x)))
                         (setf curr x)
                         (return nil)))
                 (cdr lst))
           t))))

;; ex8 定义一个单递归函数，返回两个值，分别是向量的最大与最小值。
(defun extrame (v)
  (let* ((len (length v))
         (first (if (> len 0) (svref v 0) nil)))
    (cond 
      ((= 0 len) (values nil nil))
      ((= 1 len) (values first first))
      (t (extrame-exc 1 (length v) v first first)))))
(defun extrame-exc (i n v min max)
  (if (= i n)
      (values min max)
      (let ((curr (svref v i)))
        (extrame-exc (incf i) n v (if (< curr min) curr min) (if (> curr max) curr max)))))

;; 9 a version of catch and throw
(defun shortest-path (start end net)
  (and (consp net)
       (if (eql start end)
           (list start)
           (catch 'found
             (bfs end (list (list start)) net)))))

(defun bfs (end queue net)
  (if queue
      (let* ((path (car queue)) (node (car path)))
        (bfs end
             (append (cdr queue)
                     (new-paths path node net end)) ; 在函数调用的时候，参数自求值也是可以打断函数调用的！
             net))))

;; throw when found
(defun new-paths (path node net end) 
  (mapcar #'(lambda (n)
              (let ((curr-path (cons n path)))
                (if (eql n end)
                    (throw 'found (reverse curr-path))  
                    curr-path)))
          (cdr (assoc node net))))

;; 9 b version without catch and throw but tagbody
(defun shortest-path (start end net)
  (and (consp net)
       (if (eql start end)
           (list start)
           (let ((queue (list (list start)))
                 (res nil))
             (tagbody
              bfs
                (let* ((path (pop queue)) (node (car path)))
                  (setf queue
                        (append (cdr queue)
                                (mapcar #'(lambda (n)
                                            (let ((curr-path (cons n path)))
                                              (if (eql n end)
                                                  (progn
                                                    (setf res (reverse curr-path))
                                                    (go found))
                                                  curr-path)))
                                        (cdr (assoc node net))))))
                (if queue
                    (go bfs))
              found
                (format t "the shortest path is found!"))
             res))))

;;; Other Demo fo Lisp research
;; function variable context test
;; test #1
(defun let-name ()
  (let ((name "orig"))
    (change-name name)
    name))
(defun change-name (name)
  (setf name "change")
  (format t "the value of name is ~A" name))

;; test #2
(defstruct (student (:conc-name stu-)
                    (:print-function print-stu))
  (name "Jack" :type simple-base-string)
  (age 18 :type integer)
  (score 0 :type double))

(defun print-stu (stu stream depth)
  (format stream "<~A, ~A, ~A d--~A>" (stu-name stu) 
          (stu-age stu) (stu-score stu) depth))

(defun let-student (stu)
  (change-stu-name stu)
  stu)
(defun change-stu-name (stu)
  (setf (stu-name stu) "Will"))
;; > (test-2 )
;; <Will, 18, 0 d--0>
;; 测试2证明：对于自定义的类的对象，函数传递的是引用！
(defun test-2 ()
  (let-student (make-student)))
