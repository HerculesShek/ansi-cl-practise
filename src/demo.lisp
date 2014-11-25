;;; 此文档是手记，想到的测试的东西都放在这个文件中

;;; 返回斐波纳契数列的第n个数字 
;;;  1 2 3 4 5 ...
;;;  1 1 2 3 5 ...
(defun fabo (n)
  (if (<= n 2)
      1
      (+ (fabo (- n 1)) (fabo (- n 2))))) 

;;; 测试 labels 函数 
(defun labels-test ()
  (labels ((f2 (c d) (+ (f1 100 c) d))
	   (f1 (a b) (+ a b)))
    (+ (f1 2 3) (f2 3 4))))

;;; the main method  
(defun main()
  (labels-test))

(defun re-test()
  (return-from re-test 'a))

(defun let-test()
  (let ((x 'a))
    (if (member x l)
	x)))

(defun a()
  (setf l '(a b c))
  (let-test))


(defun nilt()
  (dolist (e '(a b c) )
    (format t "~A" e)
    (if (eql e 'bb)
	(return 'aa))))

(defun let*t()
  (let ((x 1))
    (let ((y (+ x 1)))
      (+ x y))))
(defun let*t2()
  ((lambda (x) ((lambda (y) (+ x y)) (+ x 1))) 1))

;; symbol-plist demo
(defun foo (a)
  (1+ a))

(defun symbol-p-demo (fn)
  (symbol-plist fn))

(symbol-p-demo 'foo)
