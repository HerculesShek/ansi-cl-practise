;;; 第二章的例子和本节的练习

;; 判断列表x中是否有nil
(defun enigma (x)
  (and (not (null x)) ;;
       (or (null (car x))
		   (enigma (cdr x)))))

;;; 如果列表y中含有x，则返回x在y中第一次出现的位置，否则返回nil
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
		  0
		  (let ((z (mystery x (cdr y))))
			(and z (+ z 1))))))


;; 判断一个列表中是否包含列表 借鉴的是上面的函数 enigma
(defun has-list (lst)
  (and lst
       (or (listp (car lst))
		   (has-list (cdr lst)))))


;; 给出函数的迭代与递归版本：
;; 接受一个正整数，并打印出数字数量的点。
(defun print-dot-iter (n)
  (do ((i 0 (+ i 1)))
      ((= i n) 'done)
    (format t "~a " "*")))

(defun print-dot-r (n)
  (if (plusp n)
      (progn
		(format t "~a " "*")
		(print-dot-r (- n 1)))))

;; 接受一个列表，并返回 a 在列表里所出现的次数
;; 我的迭代版本，使用了setf这个不推荐的函数
(defun num-of-a-iter (lst)
  (let ((sum 0))
    (dolist (o lst)
      (if (eql 'a o) (setf sum (+ 1 sum))))
    sum))

;;; 这个是没有副作用（无setf）的 迭代 版本 很精巧！
(defun a-rep (ls)
  (do ((ls1 ls (cdr ls1))
       (n 0 (+ n (if (eq (car ls1) 'a) 1 0))))
      ((not ls1) n)
    ;; 注意到这里 do 的body代码 是空的 
    ))
;; 上面的do定义的第二个变量n，在迭代一次结束之后进行变化的时候，用到了第一个变量ls1，说明do在进行本地变量的update的时候也是按照顺序来的！-->也可以把第二个变量的定义部分当作do真正想做的事情。

(defun num-of-a-r (lst)
  (if lst
      (+ (if (eql (car lst) 'a) 1 0) (num-of-a-r (cdr lst)))
    0))
  

;; 计算一个list中数字的和，迭代和递归版本
(defun summit (lst)
  (do ((rs lst (cdr rs))
       (n 0 (if (numberp (car rs)) (+ (car rs) n) n)))
      ((null rs) n)))

(defun summit-r (lst)
  (if lst
      (+ (if (numberp (car lst)) (car lst) 0) (summit-r (cdr lst)))
      0))
