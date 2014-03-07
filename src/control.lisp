;; str->integer :return-from
(defun read-integer (str)
  (let ((accum 0))
	(dotimes (pos (length str))
	  (let ((i (digit-char-p (char str pos))))
		(if i
			(setf accum (+ (* accum 10) i))
			(return-from read-integer nil))))
	accum))

;; :cond and if
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

;; :case 
(defun month-length (mon)
  (case mon
	((jan mar may jul aug oct dec) 31)
	((apr jun sept nov) 30)
	(feb (if (leap? (progn (format t "Please enter the year:~%") (read))) 29 28))
	(otherwise "unknown month")))
;; leap year?
(defun leap? (y)
  (and (zerop (mod y 4))
	   (or (zerop (mod y 400))
		   (not (zerop (mod y 100))))))

;; :typecase
(defun what-is-it (x)
  (format t "~&~S is ~A.~%"
		  x (typecase x
			  (float "a float")
			  (null "a symbol, boolean false, or the empty list")
			  (list "a list")
			  (t (format nil "a(n) ~(~A~)" (type-of x))))))

;; :do test => (1 A) (2 1) (3 2) (4 3) (5 4) 
;;			=>	A
(defun do-test ()
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

;; :do* test
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

;; :mapc
(defun mapc-test()
  (mapc #'(lambda (x y)
			(format t "~A ~A" x y))
		'(hip flip slip)
		'(hop flop slop)))

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

;; :unwind-protect test
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

  
;; Date Arithmetic 
(defconstant month #(0 31 59 90 120 151 181 212 243 273 304 334 365)) ;; 借助这个[start, end)
(defconstant yzero 2000)
;; date -> num 
(defun day->num (d m y)
  (+ (- d 1) (month-num m y) (year-num y)))
;;
(defun month-num (m y)
  (+ (svref month (- m 1))
	 (if (and (> m 2) (leap? y)) 1 0)))
;;
(defun year-num (y)
  (let ((d 0))
	(if (>= y yzero)
		(dotimes (i (- y yzero) d)
		  (incf d (year-days (+ yzero i))))
		(dotimes (i (- yzero y) (- d))
		  (incf d (year-days (+ y i)))))))
;;
(defun year-days (y)
  (if (leap? y) 366 365))
;; num -> date
(defun num->date (n)
  (multiple-value-bind (y left) (num-year n)
	(multiple-value-bind (d m) (num-month left y)
	  (values d m y))))
;;
(defun num-year (n)
  (if (> n 0)
	  (do* ((y yzero (+ y 1))
			(prev 0 d)
			(d (year-days y) (+ d (year-days y))))
		   ((> d n) (values y (- n prev))))
	  (do* ((y (- yzero 1) (- y 1))
			(d (- (year-days y)) (- d (year-days y))))
		   ((<= d n) (values y (- n d))))))
;;
(defun num-month (n y)
  (if (leap? y)
	  (cond ((= n 59) (values 2 29))
			((> n 59) (nmon (- n 1)))
			(t (nmon n)))
	  (nmon n)))
;;
(defun nmon (n)
  (let ((m (position n month :test #'<)))
	(values (+ 1 (- n (svref month (- m 1)))) m)))
;; add n day to a date
(defun date+ (d m y n)
  (num->date (+ n (day->num d m y))))


;; Exercises
;; 1 a
((lambda (x) (cons x x)) (cdr y))
;; 1 b
((lambda (w)
   (lambda (y)
	 (cons w y))
   (+ w 2))
 (car x))
;; 2
(defun mystery (x y)
  (cond ((null y) nil)
		((eql (car y) x) 0)
		(t (let ((z (mystery x (cdr y))))
			 (and z (+ z 1))))))
;; 3
(defun square (x)
  (if (and (< 0 x 6) (integerp x))
	  x
	  (* x x)))
;; 4
(defun month-num-case-svref(m y)
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
;; 5 iter
(defun precedes (obj v)
  (when (typep v 'sequence)
	(let ((len (length v))
		  (pres nil))
	  (dotimes (i (- len 1) pres)
		(if (eql (elt v (+ i 1)) obj)
			(pushnew (elt v i) pres))))))
;; 5 recur
(defun precedes-r (obj v)
  (when (typep v 'sequence)
	(pre obj v 0 nil)))
(defun pre (obj v n lst)
  (if (= n (- (length v) 1))
	  lst
	  (progn 
		(if (eql obj (elt v (+ 1 n)))
			(pushnew (elt v n) lst))
		(pre obj v (+ n 1) lst))))
;; 6 iteration
(defun intersperse (obj lst)
  (let ((ll nil))
	(dolist (e lst)
	  (push e ll)
	  (push obj ll))
	(reverse (subseq ll 1))))
;; 6 recur
(defun intersperse-r (obj lst)
  (if (listp lst)
	  (cons (car lst) (inter obj (cdr lst)))))
(defun inter (obj lst)
  (and lst
	   (append (list obj (car lst)) (inter obj (cdr lst)))))
;; 7 (a)
(defun compare-pair-recur (lst)
  (and lst
	   (every #'numberp lst)
	   (< 1 (length lst))
	   (cpr-u lst)))
(defun cpr-u(lst)
  (if (null (cdr lst))
	  t
	  (and (= 1 (abs (- (car lst) (cadr lst))))
		   (cpr-u (cdr lst)))))
;; 7 (b)
(defun compare-pair-do (lst)
  (and (listp lst)
	   (every #'numberp lst)
	   (< 1 (length lst))
	   (do ((ll (cdr lst) (cdr ll))
			(flag (= 1 (abs (- (car lst) (cadr lst))))
				  (= 1 (abs (- (car ll) (cadr ll))))))
		   ((or (not flag)
				(null (cdr ll)))
			flag))))
;; 7 (c)
(defun compare-pair-mapc (lst)
  (and lst
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
;; 8
(defun extrame (v)
  (extrame-exc 0 (length v) v (svref v 0) (svref v 0)))
(defun extrame-exc (i n v min max)
  (if (= i n)
	  (values min max)
	  (let ((curr (svref v i)))
		(extrame-exc (incf i) n v (if (< curr min) curr min) (if (> curr max) curr max)))))
;; 9 version of catch and throw
(defun shortest-path (start end net)
  (and (consp net)
	   (if (eql start end)
		   (list start)
		   (catch 'found
			 (bfs end (list (list start)) net)))))

(defun bfs (end queue net)
  (if (null queue)
      nil
	  (let* ((path (car queue)) (node (car path)))
		(bfs end
			 (append (cdr queue)
					 (new-paths path node net end))
			 net))))

(defun new-paths (path node net end)
  (mapcar #'(lambda (n)
              (let ((path1 (cons n path)))
                (if (eql n end)
                    (throw 'found (reverse path1))  
					path1)))
          (cdr (assoc node net))))


;;; Other Demo fo Lisp research
;; function variable context test
(defun let-name ()
  (let ((name "orig"))
	(change-name name)
	name))
(defun change-name (name)
  (setf name "change"))
