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

;; 解压
(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
			(rest (uncompress (cdr lst))))
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
  (if (null a)
	  b
	  (if (null b)
		  a
		  (let ((x (copy-list b)))
			(dolist (e a)
			  (setf x (remove e x)))
			(append a x)))))

;; ex3
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
		
;; ex4 因为member默认使用的是eql比较规则
;; ex5
;; (a) pos+recursive 练习5的递归版本，修改原来的列表，可以通过传入副本来实现不修改
;; a-v1 修改原来的列表
(defun pos+ (lst) 
  (and (consp lst)
	   (every #'numberp lst)
	   (p lst 0))
  lst)
(defun p (lst n)
  (if (consp lst)
	  (progn
		(setf (car lst) (+ (car lst) n))
		(p (cdr lst) (+ n 1)))))
;; a-v2 不修改原来的列表
(defun pos+ (lst) 
  (let ((llst (copy-list lst)))
	(and (consp llst)
		 (every #'numberp lst)
		 (p llst 0))
	llst))
(defun p (lst n)
  (if (consp lst)
	  (progn
		(setf (car lst) (+ (car lst) n))
		(p (cdr lst) (+ n 1)))))

;; (b) pos+iteration 练习5的迭代版本，不修改原来的列表
;; b-v1
(defun pos+i (lst)
  (and (consp lst)
	   (every #'numberp lst)
	   (let ((index 0) (new-list nil))
		 (dolist (e lst)
		   (push (+ e index) new-list)
		   (setf index (+ 1 index)))
		 (reverse new-list))))
		   
;; (c) pos+mapcar 练习5的mapcar版本
;; c-v1
(defun pos+m (lst)
  (and (consp lst)
	   (every #'numberp lst)
	   (let ((ind nil) (l (length lst)))
		 (do ((i 0 (+ 1 i)))
			 ((= i l))
		   (push i ind))
		 (mapcar #'+ lst (reverse ind)))))


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
	  (cons n elt)))
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

(defun bfs (end queue res net)
  (if queue
	  (let ((path (car queue)))
		(let ((node (car path)))
		  (if (eql node end)
			  (progn
				(push (reverse path) res)
				(bfs end (cdr queue) res net))
			  (bfs end (append (cdr queue) (new-paths path node net)) res net))))
	  (if res
		  (progn
			(sort res #'> :key #'length)
			(pop res)))))
			  
(defun new-paths (path node net)
  ;; (let ((nodes (cdr (assoc node net))))
  (mapcar #'(lambda (x) (adjoin x path)) (assoc node net)))
	
  
  
