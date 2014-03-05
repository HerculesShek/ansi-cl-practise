;;; 如果列表y中含有x，则返回x在y中第一次出现的位置，否则返回nil
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
		  0
		  (let ((z (mystery x (cdr y))))
			(and z (+ z 1))))))



;; 解压 (uncompress '((3 r) u (2 0) p (2 t))) => (R R R U 0 0 P T T)
(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
			(rest (uncompress (cdr lst)))) ;; 这里的这种递归方式是不是就是尾递归？
	    (if (consp elt)
			(append (apply #'list-of elt) rest) ;; 这里的apply的传参很灵活
			(cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))
