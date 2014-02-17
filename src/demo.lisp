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
  (return re-test 'a))
