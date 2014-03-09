(defun fabo (n)
  (if (<= n 2)
	  1
	  (+ (fabo (- n 1)) (fabo (- n 2))))) 

(defun labels-test ()
  (labels ((f2 (c d) (+ (f1 100 c) d))
		   (f1 (a b) (+ a b)))
	(+ (f1 2 3) (f2 3 4))))

(defun main()
  (labels-test))
  
(defun re-test()
  (return-from re-test 'a))

(defun let-test()
  (let ((x 'a))
	(if (member x l)
		x)))

(defun a()
  (setf l '(a b c))
  (let-test))


(defun nilt()
  (dolist (e '(a b c) )
	(format t "~A" e)
	(if (eql e 'bb)
		(return 'aa))))

(defun let*t()
  (let ((x 1))
	(let ((y (+ x 1)))
	  (+ x y))))
(defun let*t2()
  ((lambda (x) ((lambda (y) (+ x y)) (+ x 1))) 1))
  
