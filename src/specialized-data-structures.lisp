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

;; 针对向量的回文判断
(defun mirror? (s)
  (do ((forward 0 (+ forward 1))
	   (back (- (length s) 1) (- back 1)))
	  ((or (>= forward back)
		   (not (eql (elt s forward)
					 (elt s back))))
	   (>= forward back))))

;; 将字符串分段，取出第二部分
(defun second-word (s)
  (let ((p1 (+ (position #\  s) 1))) ;; 注意这里空格的表示 “#\ ” 里面有个空格
	(subseq s p1 (position #\  s :start p1))))

;; 根据规则将字符串分割
(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
	(if p1
		(let ((p2 (position-if #'(lambda (c)
										 (not (funcall test c)))
							   str :start p1)))
		  (cons (subseq str p1 p2)
				(if p2
					(tokens str test p2)))))))

(defun constituent (c)
  (and (graphic-char-p c)
	   (not (char= c #\Space))))

(defun parse-date (date-str)
  (let ((toks (tokens date-str #'constituent 0)))
	(list (parse-integer (first toks))
		  (parse-month (second toks))
		  (my-parse-integer (third toks)))))
(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun"
	"jul" "aug" "sep" "oct" "nov" "dec"))
(defun parse-month (str)
  (let ((p (position str month-names :test #'string-equal)))
	(if p
		(incf p)
		nil)))

;; 自定义 parse-integer
(defun my-parse-integer (str)
  (and (every #'digit-char-p str)
	   (let ((res 0))
		 (dotimes (index (length str))
		   (setf res (+ (* 10 res)
						(digit-char-p (char str index)))))
		 res)))



;; BST
;; node 
(defstruct (node (:print-function
				  (lambda (node s d)
					(format s "#<~A>" (node-elt node)))))
  elt
  (l nil)
  (r nil))
;; insert 非平衡的
(defun bst-insert (obj bst <)
  (if (null bst)
	  (make-node :elt obj)
	  (let ((elt (node-elt bst)))
		(if (eql elt obj)
			bst
			(if (funcall < obj elt)
				(make-node :elt elt
						   :l (bst-insert obj (node-l bst) <)
						   :r (node-r bst))
				(make-node :elt elt
						   :l (node-l bst)
						   :r (bst-insert obj (node-r bst) <)))))))
;; search
(defun bst-find (obj bst <)
  (if bst
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			bst
			(if (funcall < obj elt)
				(bst-find obj (node-l bst) <)
				(bst-find obj (node-r bst) <))))))
;; min
(defun bst-min (bst)
  (and bst
	   (or (bst-min (node-l bst)) bst)))
;; max 
(defun bst-max (bst)
  (and bst
	   (or (bst-max (node-r bst)) bst)))
