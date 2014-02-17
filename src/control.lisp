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
	(feb (if (leap-year (progn (format t "Please enter the year:~%") (read))) 29 28))
	(otherwise "unknown month")))

(defun leap-year (y)
  (if (numberp y)
	  (if (typep (/ y 400) 'integer)
		  t
		  (if (typep (/ y 100) 'integer)
			  nil
			  (if (typep (/ y 4) 'integer)
				  t)))))

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
