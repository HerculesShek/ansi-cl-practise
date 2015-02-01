;;;; 此文档是手记，想到的测试的东西都放在这个文件中

;;; 返回斐波纳契数列的第n个数字
;;;  1 2 3 4 5 ...
;;;  1 1 2 3 5 ...
(defun fabo (n)
  (if (<= n 2)
      1
      (+ (fabo (- n 1)) (fabo (- n 2)))))

;; TODO 使用斐波纳契打印自然底数e
(defun print-e (n ht)
  )


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


;; calculate the sum of a word 
;; (sum-of-word "attitude")  =>  100
(defun sum-of-word (w &optional (index 0))
  (typecase w
    (simple-base-string
     (if (= index (length w))
         0
         (+ (- (char-code (char w index)) 96) 
            (sum-of-word w (1+ index)))))
    (t
     0)))
