;; list compress (1 1 0 1 0 0 1)  ==> ((2 1) 0 1 (2 0) 1)
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
		(if (equal elt next)
			(compr elt (+ n 1) (cdr lst))
			(cons (n-elts elt n) (compr next 1 (cdr lst))))))) ;; 如果cons的第二个参数是list的话，就是把第一个参数添加到这个列表的开头

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

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


;; copy a tree
(defun my-copy-tree (root)
  (if (atom root)
	  root
	  (cons (my-copy-tree (car root))
			(my-copy-tree (cdr root)))))

;; 自定义subst
(defun my-subst (new old tree)
  (if (eql old tree)
	  new
	  (if (atom tree)
		  tree
		  (cons (my-subst new old (car tree))
				(my-subst new old (cdr tree))))))

;; 自定义member-if
(defun our-member-if (fn lst)
  (and (consp lst)
	   (if (funcall fn (car lst))
		   lst
		   (our-member-if fn (cdr lst)))))


;; 判断一个列表是不是回文的
(defun mirror? (s)
  (let ((len (length s)))
    (if (evenp len)
		(let ((mid (/ len 2)))
		  (equal (subseq s 0 mid)
				 (reverse (subseq s mid))))
		(or (equal len 0)
			(let ((mid (/ (- len 1) 2)))
			  (equal (subseq s 0 mid)
					 (reverse (subseq s (+ mid 1)))))))))

;; 获取列表中第n个最大的值
(defun nthmost (n lst)
  (nth (- n 1)
	   (sort (copy-list lst) #'>)))

;; 反序输出一个列表
(defun our-reverse (lst)
  (let ((acc)) ;; 这么写就是acc设为nil
	(dolist (elt lst)
	  (push elt acc))
	acc))

;; 自定义assoc 判断一个关联列表中是否包含指定的键值
(defun our-assoc (key lst)
  (and (consp lst)
	   (let ((pair (car lst)))
		 (if (eql key (car pair))
			 pair
			 (our-assoc key (cdr lst))))))
			  

;; breadth-first search
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net) ;; 核心算法
  (if (null queue)
	  nil  ;; 此处 (null queue) nil 可以去掉 用queue来代替即可
	  (let ((path (car queue)))
		(let ((node (car path)))
		  (if (equal end node)
			  (reverse path)
			  (bfs end
				   (append (cdr queue)
						   (new-paths path node net))
				   net))))))

(defun new-paths (path node net) ;; 功能函数
  (mapcar #'(lambda (x) (cons x path))
		  (cdr (assoc node net))))



;; Exercises 
;;; ex1 ...
;; ex2  a and b are lists 返回a b的并集，保持a的顺序
(defun new-union (a b)
  (let ((x (copy-list b)))
	(dolist (e a)
	  (setf x (remove e x)))
	(append a x)))

;; ex3 返回列表中每个元素出现的次数，并排序 (occurrences '(a b c 1 v a b 4)) => ((B . 2) (A . 2) (4 . 1) (V . 1) (1 . 1) (C . 1))
(defun occurrences (lst)
  (if (consp lst)
	  (let ((occ))
		(dolist (elt lst)
		  (let ((ac (assoc elt occ)))
			(if (null ac)
				(push (cons elt 1) occ)
				(setf (cdr ac) (+ 1 (cdr ac)))))) ;; 这里的赋值很重要，可以看出ac并不是一个副本！也就是说let操作不会返回副本，而是一个引用！
		(sort occ #'> :key #'cdr))
	  lst))
;; 同时，assoc返回的也是个引用，而不是副本，方便修改
;; 上面的 (setf (cdr ac) (+ 1 (cdr ac))) 是可以写成 (incf (cdr ac)) incf 函数的作用就是让某个变量的值加1 而decf则是减1

;; ex4 因为member默认使用的是eql比较规则 :test #'equal

;; ex5
;; (a) pos+recursive 练习5的递归版本，修改原来的列表，可以通过传入副本来实现不修改
;; a-v1 修改原来的列表
(defun pos+ (lst) 
  (and (consp lst)
	   (every #'numberp lst)
	   (p lst 0))
  lst)
(defun p (lst n)
  (when lst
	(setf (car lst) (+ (car lst) n)) ;; (incf (car lst) n)
	(p (cdr lst) (+ n 1))))
;; a-v2 不修改原来的列表，传入一个副本即可
(defun pos+ (lst) 
  (let ((llst (copy-list lst)))
	(and (consp llst)
		 (every #'numberp lst)
		 (p llst 0))
	llst))

;; (b) pos+iteration 练习5的迭代版本，不修改原来的列表
;; b-v1
(defun pos+i (lst)
  (and (consp lst)
	   (every #'numberp lst)
	   (let ((index 0) (new-list nil))
		 (dolist (e lst)
		   (push (+ e index) new-list)
		   (setf index (+ 1 index))) ;; (incf index)
		 (reverse new-list))))

;; (c) pos+mapcar
;; c-v1
(defun pos+m (lst)
  (and (consp lst)
	   (every #'numberp lst)
	   (let ((ind nil) (l (length lst)))
		 (do ((i 0 (+ 1 i)))
			 ((= i l))
		   (push i ind)) ;; 一个比较笨的存放索引序列的方法
		 (mapcar #'+ lst (reverse ind)))))
;; c-v2 相比v1 此版本简洁高效
(defun pos+mapcar (lst)
  (let ((i -1))
	(mapcar #'(lambda (x) (+ x (incf i))) lst)))

;;; ex6 ...
;; ex7  CL-USER> (dot-compress '(1 1 1 0 1 0 0 1 0)) =>  ((3 . 1) 0 1 (2 . 0) 1 0)
(defun dot-compress (lst)
  (if (consp lst)
	  (dot-compr (car lst) 1 (cdr lst))
	  lst))
(defun dot-compr (elt n lst)
  (if (null lst)
	  (list (dot-n-elts elt n))
	  (let ((next (car lst)))
		(if (eql next elt)
			(dot-compr elt (+ n 1) (cdr lst))
			(cons (dot-n-elts elt n) (dot-compr (car lst) 1 (cdr lst)))))))
(defun dot-n-elts (elt n)
  (if (= 1 n)
	  elt
	  (cons n elt))) ;; just change this piece
;; 这里还得看一下 list 函数的原理！


;; ex8 CL-USER> (showdots '(a b c))  ==>  (A . (B . (C . nil)))
(defun showdots (lst)
  (if (atom lst)
	  (format t "~a" lst)
	  (progn
		(format t "(")
		(showdots (car lst))
		(format t " . ")
		(showdots (cdr lst))
		(format t ")"))))

;; ex9
(defun longest-path (start end net)
  (bfs end (list (list start)) nil net))

(defun bfs (end queue res net) ;; res stores all the path starting from 'start' and ending with 'end'
  (if queue
	  (let ((path (car queue)))
		(let ((node (car path)))
		  (if (eql node end)
			  (progn
				(push (reverse path) res)
				(bfs end (cdr queue) res net))
			  (bfs end (append (cdr queue) (new-paths path node net)) res net))))
	  (if res
		  (car (sort res #'> :key #'length)))))

(defun new-paths (path node net)
  (remove nil (mapcar #'(lambda (x) (if (member x path)
										nil
										(cons x path)))
					  (cdr (assoc node net)))))
