(defun read-integer (str)
  (let ((accum 0))
	(dotimes (pos (length str))
	  (let ((i (digit-char-p (char str pos))))
		(if i
			(setf accum (+ (* accum 10) i))
			(return-from read-integer nil))))
	accum))

(defun our-member (obj lst)
  (if (atom lst)
	  nil
	  (if (eql (car lst) obj)
		  lst
		  (our-member obj (cdr lst)))))

(defun our-member-cond (obj lst)
  (cond ((atom lst) lst)
		((eql obj (car lst)) lst)
		(t (our-member-cond obj (cdr lst)))))

(defun month-length (mon)
  (case mon
	((jan mar may jul aug oct dec) 31)
	((apr jun sept nov) 30)
	(feb (if (leap? (progn (format t "Please enter the year:~%") (read))) 29 28))
	(otherwise "unknown month")))

(defun leap? (y)
  (and (zerop (mod y 4))
	   (or (zerop (mod y 400))
		   (not (zerop (mod y 100))))))

(defun what-is-it (x)
  (format t "~&~S is ~A.~%"
		  x (typecase x
			  (float "a float")
			  (null "a symbol, boolean false, or the empty list")
			  (list "a list")
			  (t (format nil "a(n) ~(~A~)" (type-of x))))))


(defun do-test ()
  (let ((x 'a))
	(do ((x 1 (+ x 1))
		 (y x x))
		((> x 5))
	  (format t "(~A ~A) " x y))
	x))


(defun do-test-1 () 
  (do ((m 1 (+ m 1))
	   (n m m)) ;; ERROR! 没有m的定义！
	  ((> m 5))
	(format t "(~A ~A) " m n)))


(defun do-test-2 ()
  (let ((x 'a))
	(do ((x 1 (incf x))
		 (y x x))
		((> x 5))
	  (format t "(~A ~A) " x y))
	x))


(defun do*-test ()
  (do* ((x 1 (+ y 1))
		(y x (+ 1 x)))
	   ((> x 5))
	(format t "(~A ~A) " x y)))

(defun do*-test-1 ()
  (do* ((y x (+ 1 x)) ;; 错误！因为是顺序执行的 
		(x 1 (+ y 1)))
	   ((> x 5))
	(format t "(~A ~A) " x y)))

(defun mapc-test()
  (mapc #'(lambda (x y)
			(format t "~A ~A" x y))
		'(hip flip slip)
		'(hop flop slop)))


(defun super ()
  (catch 'abort
	(sub)
	(format t "We'll never see this.")))

(defun sub ()
  (throw 'abort 99))


(defun factorial (x)
  (cond ((or (not (typep x 'integer)) (minusp x))
		 (error "~S is not a valid argument to FACTORIAL." x))
		((zerop x) 1)
		(t (* x (factorial (- x 1))))))


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
	(format t "The value of var is ~A" var)))
  
  
;; Date Arithmetic 
(defconstant month #(0 31 59 90 120 151 181 212 243 273 304 334 365))
(defconstant yzero 2000)

(defun day->num (d m y)
  (+ (- d 1) (month-num m y) (year-num y)))

(defun month-num (m y)
  (+ (svref month (- m 1))
	 (if (and (> m 2) (leap? y)) 1 0)))

(defun year-num (y)
  (let ((d 0))
	(if (>= y yzero)
		(dotimes (i (- y yzero) d)
		  (incf d (year-days (+ yzero i))))
		(dotimes (i (- yzero y) (- d))
		  (incf d (year-days (+ y i)))))))

(defun year-days (y)
  (if (leap? y) 366 365))


(defun num->date (n)
  (multiple-value-bind (y left) (num-year n)
	(multiple-value-bind (d m) (num-month left y)
	  (values d m y))))

(defun num-year (n)
  (if (> n 0)
	  (do* ((y yzero (+ y 1))
			(prev 0 d)
			(d (year-days y) (+ d (year-days y))))
		   ((> d n) (values y (- n prev))))
	  (do* ((y (- yzero 1) (- y 1))
			(d (- (year-days y)) (- d (year-days y))))
		   ((<= d n) (values y (- n d))))))

(defun num-month (n y)
  (if (leap? y)
	  (cond ((= n 59) (values 2 29))
			((> n 59) (nmon (- n 1)))
			(t (nmon n)))
	  (nmon n)))

(defun nmon (n)
  (let ((m (position n month :test #'<)))
	(values m (+ 1 (- n (svref month (- m 1)))))))


(defun date+ (d m y n)
  (num->date (+ n (day->num d m y))))
