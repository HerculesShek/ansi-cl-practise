;;;; this source file contains all the demos and exercises in chapter 3 

;; whether x is a list 
(defun our-listp (x)
  (or (null x) (consp x)))

;; whether x is a atom 这里看出，lisp中的对象不是atom就是cons
(defun our-atom (x)
  (not (consp x)))

;; copy a list
(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))

;; list compress (1 1 0 1 0 0 1)  ==> ((2 1) 0 1 (2 0) 1)
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))
;; TODO compr 是不是尾递归？
(defun compr (elt n lst)
  (if (null lst) ; base case 
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (equal elt next)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst))))))) ; 如果cons的第二个参数是list的话，就是把第一个参数添加到这个列表的开头
;; > (n-elts 2 1) => 2  
;; > (n-elts 2 3) => (3 2)
(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

;; 解压 (uncompress '((3 r) u (2 0) p (2 t))) => (R R R U 0 0 P T T)
(defun uncompress (lst)
  (if (null lst) ; base case 
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst)))) ; 这里的这种递归方式是不是尾递归？
        (if (consp elt)
            (append (apply #'list-of elt) rest) ; 这里的apply的传参很灵活
            (cons elt rest)))))
;; > (list-of 2 'a) => (A A)
(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

;; the nth cdr of a list 
(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
      (our-nthcdr (- n 1) (cdr lst))))

;; the last n cdr of a list 
(defun our-last (lst &optional (n 1))
  (if (atom lst)
      lst
      (nthcdr (if (minusp (- (num-cons lst) n))
                  0
                  (- (num-cons lst) n))
              lst)))
;; calculate the number of cons in a list 
(defun num-cons (lst)
  (if (atom lst)
      0
      (+ 1 (num-cons (cdr lst)))))

;; copy a tree 注意和函数 our-copy-list 的区别
(defun my-copy-tree (root)
  (if (atom root)
      root
      (cons (my-copy-tree (car root))
            (my-copy-tree (cdr root)))))

;; 自定义subst 递归定义
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
;; "a" => T "aba" => T  "abba" => T  "" => T
(defun mirror? (s)
  (let ((len (length s)))
    (if (evenp len)  ; 长度是偶数
        (or (equal len 0)
            (let ((mid (/ len 2)))
              (equal (subseq s 0 mid)
                     (reverse (subseq s mid)))))
        (or (equal len 1)
            (let ((mid (/ (- len 1) 2)))  ; 长度是奇数
              (equal (subseq s 0 mid)
                     (reverse (subseq s (+ mid 1)))))))))

;; 获取列表中第n个最大的值
(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

;; 反序输出一个列表
(defun our-reverse (lst)
  (let ((acc)) ; acc为nil
    (dolist (elt lst)
      (push elt acc))
    acc))

;; 判断一个列表是不是正规列表--proper list
;; 其实这个不严格，因该加入排除环形列表--circular list
(defun proper-list? (lst)
  (or (null lst)
      (and (consp lst)
           (proper-list? (cdr lst)))))

;; 自定义assoc 判断一个关联列表中是否包含指定的键值
(defun our-assoc (key lst)
  (and (consp lst)
       (let ((pair (car lst)))
         (if (eql key (car pair))
             pair
             (our-assoc key (cdr lst))))))

;;; breadth-first search
;;; 网络可以这么表示: '((a b c) (b c) (c d)) --关联列表
;;; 就是a节点可直达b和c，b可以直达c，c可以直达d
;;; 调用的时候 (shortest-path 'a 'd net) => (a c d)
;; 最短路径算法的入口
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;; 核心算法 广度优先搜索
(defun bfs (end queue net)
  (if (null queue)
      nil  ; 此处 (null queue) nil 可以去掉 用queue来代替即可
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (equal end node)
              (reverse path)
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))

;; 功能函数
(defun new-paths (path node net)
  (mapcar #'(lambda (x) (cons x path))
          (cdr (assoc node net))))
;; 测试函数
(defun bfs-test ()
  (let ((net '((a b c) (b c) (c d))))
    (shortest-path 'a 'd net)))


;;; Exercises 
;; ex1 ...
;; ex2  a and b are lists 返回a b的并集，保持a的顺序
(defun new-union (a b)
  (let ((x (copy-list b)))
    (dolist (e a)
      (setf x (remove e x)))
    (append a x)))
;; 使用member和push的版本：
(defun new-union (a b)
  (let ((ra (reverse a)))
    (dolist (x b)
      (if (not (member x ra))
          (push x ra)))
    (reverse ra)))

;;; ex3 返回列表中每个元素出现的次数，并排序 
;;; (occurrences '(a b c 1 v a b 4)) 
;;; => ((B . 2) (A . 2) (4 . 1) (V . 1) (1 . 1) (C . 1))
(defun occurrences (lst)
  (if (consp lst)
      (let ((occ))
        (dolist (elt lst)
          (let ((ac (assoc elt occ)))
            (if (null ac) ; 可简写为(if ac 下面两行调换位置
                (push (cons elt 1) occ)
                (setf (cdr ac) (+ 1 (cdr ac)))))) ;; 这里的赋值很重要，可以看出ac并不是一个副本！也就是说let操作不会返回副本，而是一个引用！
        (sort occ #'> :key #'cdr))
      lst))
;; 同时，assoc返回的也是个引用，而不是副本，方便修改
;; 上面的 (setf (cdr ac) (+ 1 (cdr ac))) 是可以写成 (incf (cdr ac)) 
;; incf 函数的作用就是让某个变量的值加1 而decf则是减1

;; ex4 因为member默认使用的是eql比较规则 :test #'equal

;;; ex5 假设函数 pos+ 接受一个列表并返回把每个元素加上
;;; 自己的位置的列表：
;;; > (pos+ '(7 5 1 4))
;;; (7 6 3 7)
;; (a) pos+recursive 练习5的递归版本，修改原来的列表，可
;; 以通过传入副本来实现不修改
;; ex5-a-v1 修改原来的列表 有副作用
(defun pos+ (lst)
  (and (consp lst)
       (every #'numberp lst)
       (p lst 0))
  lst)
;; 仅仅是处理lst而已，返回值为nil
(defun p (lst n)
  (when lst
    (setf (car lst) (+ (car lst) n)) ; (incf (car lst) n)
    (p (cdr lst) (+ n 1))))

;; ex5-a-v2 不修改原列表，传入一个副本即可，还是使用p函数
(defun pos+ (lst)
  (and (consp lst)
       (every #'numberp lst)
       (let ((llst (copy-list lst)))
         (p llst 0)
         llst)))

;; ex5-a-v3 函数p的另一版本，不修改lst的
(defun pos+ (lst)
  (and (consp lst)
       (every #'numberp lst)
       (p-v2 lst 0)))
(defun p-v2 (lst n)
  (when lst
    (cons (+ n (car lst)) (p-v2 (cdr lst) (incf n)))))

;; (b) pos+iteration 练习5的迭代版本，不修改原来的列表
;; ex5-b-v1 dolist
(defun pos+i (lst)
  (and (consp lst)
       (every #'numberp lst)
       (let ((index 0) (new-list nil)) 
         (dolist (e lst)
           (push (+ e index) new-list)
           (setf index (+ 1 index))) ; (incf index)
         (reverse new-list))))
;; 第4行可简写为 (let ((index 0) (new-list))

;; ex5-b-v2 do
(defun pos+i (lst)
  (and (consp lst)
       (every #'numberp lst)
       (do ((llst lst (cdr llst))
            (index 0 (+ index 1))
            (acc nil (cons (+ index (car llst)) acc)))
           ((null llst) (reverse acc)))))

;; (c) pos+mapcar
;; ex5-c-v1
(defun pos+m (lst)
  (and (consp lst)
       (every #'numberp lst)
       (let ((ind nil) (l (length lst)))
         (do ((i 0 (+ 1 i)))
             ((= i l))
           (push i ind)) ; 一个比较笨的存放索引序列的方法
         (mapcar #'+ lst (reverse ind)))))

;; ex5-c-v2 相比v1 此版本简洁高效
(defun pos+mapcar (lst)
  (let ((i -1))
    (mapcar #'(lambda (x) (+ x (incf i))) lst)))

;; ex6 ...
;; ex7  
;; > (dot-compress '(1 1 1 0 1 0 0)) 
;; => ((3 . 1) 0 1 (2 . 0))
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
      (cons n elt))) ; just change this piece
;; 这里重温一下 list 函数的原理！


;; ex8  > (showdots '(a b c)) ==> (A . (B . (C . nil)))
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
;; 写一个程序来找到 3.15 节里表示的网络中，最长有限的路径(不重
;; 复)。网络可能包含循环。
(defparameter *net* '((a b c) (b a c) (c a b d) (d c)))

(defun longest-path (start end net)
  (bfs end (list (list start)) nil net))

;; res stores all the paths starting from 'start' and ending with 'end'
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
          (car (sort res #'> :key #'length)))))
;; 实际上上面的res的使用有些冗余了，其实使用res保存最
;; 后的1条路径就可以, 因为是广度优先搜索，queue后面
;; 的长度一定是最长的，新方案看下面

(defun new-paths (path node net)
  (remove nil (mapcar #'(lambda (x) (if (member x path)
                                        nil
                                        (cons x path)))
                      (cdr (assoc node net)))))

(longest-path 'a 'd *net*) ; (A B C D)


;; 新方案
;; 这个版本的 new-paths 比上个版本要简洁一些！
(defun new-paths (path node net)
  (let (acc)
    (dolist (x (cdr (assoc node net)))
      (or (member x path)
          (push (cons x path) acc)))
    acc))

(defun bfs-l (end queue net sol)
  (if queue
      (let ((path (car queue)))
        (let ((node (car path)))
          (bfs-l end
                 (append (cdr queue) (new-paths path node net))
                 net
                 (if (eql node end) path sol))))
      (reverse sol)))

(defun longest-path (start end net)
  (bfs-l end (list (list start)) net nil))

