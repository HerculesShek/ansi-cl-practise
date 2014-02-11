;; 对顺序排列的 vec 进行二叉搜索
(defun bin-search (obj vec)
  (let ((len (length vec)))
	(and (not (zerop len))
		 (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (let ((range (- end start)))
	(if (zerop range)
		(if (eql obj (svref vec start))
			obj
			nil)
		(let ((mid (+ start (round (/ range 2)))))
		  (let ((obj2 (svref vec mid)))
			(if (> obj obj2)
				(finder obj vec (+ mid 1) end)
				(if (< obj obj2)
					(finder obj vec start (- mid 1))
					obj)))))))


(defun mirror? (s)
  (do ((forward 0 (+ forward 1))
	   (back (- (length s) 1) (- back 1)))
	  ((or (>= forward back)
		   (not (eql (elt s forward)
					 (elt s back)))))
	(>= forward back)))
