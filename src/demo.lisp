(defun fabo (n)
  (if (<= n 2)
	  1
	  (+ (fabo (- n 1)) (fabo (- n 2))))) 

(defun main()
  
