;;;; source file for demos and practise of chapter 2 

;; ask for a number 
(defun ask-number()
  (format t "Please enter a number: ")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

;; show squares ranges [i, end]
(defun show-squares-recursion(i end)
  (if (> i end)
      'done
      (progn
        (format t "~a ~a ~%" i (* i i))
        (show-squares-recursion (+ i 1) end))))

;; get the length of a list  
(defun our-length (lst)
  (let ((len 0))
    (dolist (o lst)
      (format t "~a~%" o)
      (setf len (+ len 1)))
    len))
;; recursive 
(defun our-length-r (lst)
  (if (null lst)
      0
      (+ (our-length-r (cdr lst)) 1)))

;;; Exercises
;; ex-2 
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a (cons 'b '(c)))
(cons 'a '(b c))

;; ex-3 
(defun our-fourth (lst)
  (car (cdr (cdr (cdr lst)))))

;; ex-4 
(defun larger (x y)
  (if (> x y) x y))

;; ex-5 a
;; 判断列表x中是否有nil
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))
;; 可以把and当作条件传递，碰到第一个“条件”为nil则终止，否则就
;; 一直执行接下来的表达式并继续传递

;; ex-5 b
;;; 如果列表y中含有x，则返回x在y中第一次出现的位置，0-based
;;; 没有则返回nil
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))

;;; TODO 确认一下上面哪种递归是尾递归

;; ex-6 car or apply

;; ex-7
;; 判断一个列表中是否包含列表 借鉴的是上面的函数 enigma
(defun has-list (lst)
  (and lst
       (or (listp (car lst))
           (has-list (cdr lst)))))

;; ex-8 给出函数的迭代与递归版本：
;; ex-8 a 接受一个正整数，并打印出数字数量的点。
(defun print-dot-iter (n)
  (do ((i 0 (+ i 1)))
      ((= i n) "print done")
    (format t "*")))

(defun print-dot-r (n)
  (if (plusp n)
      (progn
        (format t "*")
        (print-dot-r (- n 1)))
      "print done"))

;; ex-8 b 接受一个列表，并返回a在列表里所出现的次数
;; 我的迭代版本，使用了setf这个不推荐的函数
(defun num-of-a-iter (lst)
  (let ((sum 0))
    (dolist (o lst)
      (if (eql 'a o) (setf sum (+ 1 sum))))
    sum))
;; 下面这个是没有副作用（无setf）的迭代版本 很精巧！
(defun a-rep (ls)
  (do ((ls1 ls (cdr ls1))
       (n 0 (+ n (if (eq (car ls1) 'a) 1 0))))
      ((not ls1) n)
    ;; 注意到这里 do 的body代码 是空的 
    ))
;; 上面的do定义的第二个变量n的update部分是真正与业务逻辑有关的事情。这种写法
;; 比C++中的for循环灵活多了！
;; 递归版本
(defun num-of-a-r (lst)
  (if lst
      (+ (if (eql (car lst) 'a) 1 0) (num-of-a-r (cdr lst)))
      0))

;; ex-9
;; 计算一个list中数字的和，迭代和递归版本
(defun summit (lst)
  (do ((rs lst (cdr rs))
       (n 0 (if (numberp (car rs)) (+ (car rs) n) n)))
      ((null rs) n)))

(defun summit-r (lst)
  (if lst
      (+ (if (numberp (car lst)) (car lst) 0) (summit-r (cdr lst)))
      0))

