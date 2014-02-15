;; 对顺序排列的 vec 进行二叉搜索
(defun bin-search (obj vec)
  (let ((len (length vec)))
	(and (not (zerop len))
		 (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end) ;; 闭区间
  (let ((range (- end start)))
	(if (zerop range) ;; 区间内只有一个元素
		(if (eql obj (svref vec start))
			obj
			nil)
		(let ((mid (+ start (round (/ range 2)))))
		  (let ((obj2 (svref vec mid)))
			(if (> obj obj2)  ;; 其实这里用cond比较好
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
;; remove min
(defun bst-remove-min (bst)
  (if (null (node-l bst))
	  (node-r bst)
	  (make-node :elt (node-elt bst)
				  :l (bst-remove-min (node-l bst))
				  :r (node-r bst))))
;; remove max 
(defun bst-remove-max (bst)
  (if (null (node-r bst))
	  (node-l bst)
	  (make-node :elt (node-elt bst)
				 :l (node-l bst)
				 :r (bst-remove-max (node-r bst)))))

(defun bst-remove (obj bst <)
  (if bst
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			(percolate bst)
			(if (funcall < obj elt)
				(make-node :elt elt
						   :l (bst-remove obj (node-l bst) <)
						   :r (node-r bst))
				(make-node :elt elt
						   :l (node-l bst)
						   :r (bst-remove obj (node-r bst) <)))))))
(defun percolate (bst)
  (let ((l (node-l bst)) (r (node-r bst)))
	(cond ((null l) r)
		  ((null r) l)
		  (t (if (zerop (random 2))
				 (make-node :elt (node-elt (bst-max l))
							:l (bst-remove-max l)
							:r r)
				 (make-node :elt (node-elt (bst-max r))
							:l l
							:r (bst-remove-min r)))))))

;; print 
(defun print-bst (bst)
  (if bst
	  (progn
	  (format t "~A <--~A--> ~A~%" (node-l bst) bst (node-r bst))
	  (print-bst (node-l bst))
	  (print-bst (node-r bst)))))
;; 中序输出 排序
(defun bst-traverse (fn bst)
  (when bst
	(bst-traverse fn (node-l bst))
	(funcall fn (node-elt bst))
	(bst-traverse fn (node-r bst))))


;; Exercises
;; ex1
;; 这里主要是找到一个规律
;; 思路是在旋转的时候把原来的一个整行当作一个整体考虑
;; 坐标的值：顺时针旋转一个正方的二维数组的话，原来的列坐标变为现在的行坐标
;; 行数-1-原来的列坐标变为现在的行坐标 即可 
;; 逆时针则调换上面两条原则即可
(defun quarter-turn (arr)
  (and arr
	  (let ((dim (array-dimensions arr)))
		(let ((d (car dim))
			  (new-arr (make-array dim)))
			  (do ((i 0 (incf i))) ((= i d))
				(do ((j 0 (incf j))) ((= j d))
				  (setf (aref new-arr j (- d 1 i)) (aref arr i j))))
		  new-arr))))
			  
;; ex2 
;; a
(defun my-copy-list (lst)
  (reduce #'(lambda (lst obj) (append lst (list obj))) lst :initial-value nil))
(defun my-copy-list-c (lst)
  (reduce #'cons lst :from-end t :initial-value nil)) 
;; (reduce #'(lambda (a lst) (cons a lst)) '(a b c) :from-end t :initial-value nil)
;; 从此处看出，默认是左结合，加入 :from-end t 之后，变为了右结合，这时候要注意上面lambda中的参数的顺序

;; b
(defun my-reverse (lst)
	(reduce #'(lambda (lst obj) (cons obj lst)) lst :initial-value nil))

;; ex3
(defstruct my-node
  elt 
  left
  middle
  right)

(defun copy-tree-ex3 (tree)
  (and tree
	   (make-my-node :elt (my-node-elt tree)
					 :left (my-node-left tree)
					 :middle (my-node-middle tree)
					 :right (my-node-right tree))))
(defun value-test (obj tree)
  (if tree
	  (or
	   (eql (my-node-elt tree) obj)
	   (value-test (my-node-left tree) obj)
	   (value-test (my-node-middle tree) obj)
	   (value-test (my-node-right tree) obj))))

;; ex4
(defun bst-ordered-list (bst)
  (if bst
	  (append (bst-ordered-list (node-r bst))
			  (list (node-elt bst))
			  (bst-ordered-list (node-l bst)))))
;; labels and ex4-v2 ex4-v3
;; labels
(defun recursive-times (k n)
  (labels ((temp (n) 
			 (if (zerop n) 0 (+ k (temp (1- n))))))
	(temp n)))
;; ex4-v2
(defun bst->list (bst0)
  (labels ((rec (bst1 acc)
             (if bst1
                 (rec (node-r bst1) (cons (node-elt bst1) (rec (node-l bst1) acc)))
				 acc)))
    (rec bst0 nil)))
;; ex4-v3
(defun bst->lst (bst0)
  (labels ((rec (bst1 acc)
             (if bst1
                 (rec (node-l bst1) (append (rec (node-r bst1) acc) (list (node-elt bst1))))
				 acc)))
    (rec bst0 nil)))


;; ex5 上面的 bst-insert 与 bst-adjoin 功能一样
;; ex6
;; a
(defun assoc->hash (a)
  (if a
	  (let ((h (make-hash-table)))
		(dolist (e a)
		  (setf (gethash (car e) h) (cdr e)))
		h)))
;; b
(defun hash->assoc (h)
  (if h
	  (let ((lst nil))
		(maphash #'(lambda (k v)
					 (setf lst (cons (cons k v) lst)))
				 h)
		lst)))
(defun hash->lst (ht)
  (let ((acc nil))
	(maphash #'(lambda (k v) (push (cons k v) acc)) ht)
	acc))

