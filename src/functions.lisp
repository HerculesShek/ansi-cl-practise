;; symbol-function
(defun (setf primo) (val lst)
  (setf (car lst) val))

(defun foo (x)
  "Implements an enhanced paradigm of diversity"
  x)

;; labels
(defun labels-test()
  (labels ((a (x) (b x))
		   (b (x) (a x)))
	(a 3)))

;; rest demo
(defun do-rest(a &rest args)
  (list a (type-of args)))
(defun rest-demo(&rest args)
  args)

;; optional demo
(defun do-optional1(&optional (a 'fun)  b)
  (list a b))

;; Utilities
(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (fn n)
  (let ((acc nil))
	(dotimes (i n)
	  (push (funcall fn i) acc))
	(nreverse acc)))

(defun filter (fn lst)
  (let ((acc nil))
	(dolist (x lst)
	  (let ((var (funcall fn x)))
		(if var (push var acc))))
	(nreverse acc)))

(defun most(fn lst)
  (if (null lst)
	  (values nil nil)
	  (let* ((wins (car lst))
			 (max (funcall fn wins)))
		(dolist (obj (cdr lst))
		  (let ((score (funcall fn obj)))
			(when (> score max)
			  (setf wins obj
					max score))))
		(values wins max))))

;; Closures
(defun combiner (x)
  (typecase x
	(number #'+)
	(list #'append)
	(t #'list)))
(defun combine(&rest args)
  (apply (combiner (car args))
		 args))

(defun add-to-list(num lst)
  (mapcar #'(lambda (x)
			  (+ x num))
		  lst))

(let ((counter 0))
  (defun reset()
	(setf counter 0))
  (defun stamp()
	(incf counter)))

(defun our-complement (f)
  #'(lambda (&rest args)
	  (not (apply f args))))

;; Dylan methods implemented in Common Lisp, Awesome!
;; functions returning functions
;; (compose #'a #'b #'c) == #'(lambda (&rest args) (a (b (apply #'c args))))
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns) ;; from end
	#'(lambda (&rest args)
		(reduce #'(lambda (res f)
					(funcall f res)) ;; here is funcall, the precedes functions only take 1 argument
				rest
				:initial-value (apply fn1 args))))) ;; here is apply, so the last functions to compose can take any number of arguments

(defun disjoin (fn &rest fns)
  (if (null fns)
	  fn
	  (let ((disj (apply #'disjoin fns)))
		#'(lambda (&rest args)
			(or (apply fn args) (apply disj args)))))) ;; it's not effective 

(defun conjoin (fn &rest fns)
  (if (null fns)
	  fn
	  (let ((conj (apply #'conjoin fns)))
		#'(lambda (&rest args)
			(and (apply fn args) (apply conj args))))))
		
(defun curry (fn &rest args)
  #'(lambda (&rest args2)
	  (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
	  (apply fn (append args2 args))))

(defun always (x)
  #'(lambda (&rest args) ;; warnings for unused lexical variable args
	  x))

;; dynamic scope
(defun ds-1()
  (let ((x 10))
	(defun foo()
	  x))
  
  (let ((x 20))
	(foo)))

(defun ds-2()
  (let ((x 10))
	(defun foo()
	  (declare (special x))
	  x))
  
  (let ((x 20))
	(declare (special x))
	(foo)))

(let ((*print-base* 16))
  (princ 32))

;; using recursion
(defun fib (n)
  (if (<= n 1)
	  1
	  (+ (fib (- n 1))
		 (fib (- n 2)))))

(defun fib-do (n)
  (do ((i n (- i 1))
	   (f1 1 (+ f1 f2))
	   (f2 1 f1))
	  ((<= i 1) f1)))

;; exercises
;; ex1
(defun tokens (str &key (test #'constituent) (start 0)) ;; 
  (let ((p1 (position-if test str :start start)))
	(if p1
		(let ((p2 (position-if #'(lambda (c)
								   (not (funcall test c)))
							   str :start p1)))
		  (cons (subseq str p1 p2)
				(if p2
					(tokens str :test test :start p2))))))) ;; 
(defun constituent (c)
  (and (graphic-char-p c)
	   (not (char= c #\Space))))
;; ex2
(defun bin-search (obj vec &key (key #'identity) (test #'equal) (start 0) end) ;; [start end]
  (let* ((len (length vec))
		 (e (if (and end (integerp end)) end (- len 1)))) 
	(and (not (zerop len))
		 (<= start e)
		 (finder obj vec start e key test))))

(defun finder (obj vec start end key test)
  (let ((range (- end start)))
	(if (zerop range)
		(if (funcall test obj (funcall key (svref vec start)))
			(svref vec start)
			nil)
		(let ((mid (+ start (round (/ range 2)))))
		  (let ((obj2 (funcall key (svref vec mid))))
			(if (> obj obj2)
				(finder obj vec (+ mid 1) end key test)
				(if (< obj obj2)
					(finder obj vec start (- mid 1) key test)
					(svref vec mid))))))))
;; ex3
(defun n-params (&rest args)
  (length args))
;; ex4
(defun most-2 (fn lst)
  (cond 
	((null lst) (values nil nil))
	((null (cdr lst)) (values (car lst) nil))
	(t 
	 (let* ((e1 (car lst))
			(e2 (cadr lst))
			(v1 (funcall fn e1))
			(v2 (funcall fn e2)))
	   (when (< v1 v2)
		 (rotatef e1 e2)
		 (rotatef v1 v2))
	   (dolist (elt (cddr lst))
		 (let ((v (funcall fn elt)))
		   (cond 
			 ((< v1 v) (setf e2 e1 v2 v1 e1 elt v1 v))
			 ((< v2 v) (setf e2 elt v2 v))
			 (t nil))))
	   (values e1 e2)))))
;; ex5
(defun remove-if-1 (fn lst)
  (filter-5 #'(lambda (x) (not (funcall fn x))) lst))
(defun filter-5 (fn lst)
  (let ((acc nil))
	(dolist (x lst)
	  (let ((var (funcall fn x)))
		(if var (push x acc)))) ;; modified
	(nreverse acc)))
(defun remove-if-2 (fn lst)
  (let ((acc nil))
	(dolist (obj lst)
	  (if (not (funcall fn obj))
		  (push obj acc)))
	(nreverse acc)))
;; ex6
(let ((max 0))
  (defun max-fo-far (n)
	(and (numberp n)
		 (progn 
		   (setf max (if (< max n) n max))
		   max))))
;; ex7
(let ((pre))
  (defun pre (n)
	(if (not (numberp n))
		(error "You should pass a number!")
		(if pre
			(progn 
			  (let ((temp pre))
				(setf pre n)
				(< temp pre)))
			(progn
			  (setf pre n)
			  nil)))))
;; ex7-prog1
(let ((pre))
  (defun pre-prog1 (n)
	(if (not (numberp n))
		(error "You should pass a number!")
		(prog1
			(and pre (< pre n))
		  (setf pre n)))))
;; ex8
(defun expensive (n)
  (multiple-value-bind (res) (round (/ (* n (random 100)) 100))
	res))
(let ((respo (make-array 101 :initial-element nil)))
  (defun frugal (n)
	(or (svref respo n)
		(setf (svref respo n) (expensive n))))))
;; ex9 
(defun apply-octal (&rest args)
  (let ((*print-base* 8))
	(apply #'apply args)))
  
