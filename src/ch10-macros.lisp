;;;; source file for chapter 10 of ANSI Common Lisp

;; our toplevel 
(defun our-toplevel ()
  (do ()
      (nil)
    (format t "~&> ")
    (print (eval (read)))))

;; set its argument to nil
(defmacro nil! (x)
  (list 'setf x nil))

;; understand macroexpand-1
(defun macroexpand-1-nil!-puppet ()
  ((lambda (expr)
     (apply #'(lambda (x) (list 'setf x nil))
            (cdr expr))) '(nil a)))

;; backquote is a good way to define macros
(defmacro nil!-back (x)
  `(setf ,x nil))

;; my while macro
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))
(defun while-test ()
  (let ((x 0))
    (while (< x 10)
      (princ x)
      (incf x))))

;; do macro test 
(defun do-test ()
  (let ((x 11))
    (do ((i 0 (1+ i)))
        ((= i (decf x)))
      (format t "x is ~A i is ~A~&" x i))))

;; quick sort 
(defun quicksort (vec l r)
  (let ((i l)
        (j r)
        (p (svref vec (round (+ l r) 2))))
    (while (<= i j)
      (while (< (svref vec i) p) (incf i))
      (while (> (svref vec j) p) (decf j))
      (when (<= i j)
        (rotatef (svref vec i) (svref vec j))
        (incf i)
        (decf j)))
    (if (>= (- j l) 1) (quicksort vec l j))
    (if (>= (- r i) 1) (quicksort vec i r)))
  vec)

;; wrong macro
(defmacro ntimes (n &rest body)
  `(do ((x 0 (+ x 1)))
       ((>= x ,n))
     ,@body))

;; improved macro
(defmacro ntimes (n &rest body)
  (let ((g (gensym)))
    `(do ((,g 0 (+ ,g 1)))
         ((>= ,g ,n))
       ,@body)))

;; final macro
(defmacro ntimes (n &rest body)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,h))
         ,@body))))

;; my car
(defmacro cah (lst)
  `(car ,lst))

;; wrong incf 
(defmacro w-incf (x &optional (y 1))
  `(setf ,x (+ ,x ,y)))

;; correct incf
(define-modify-macro my-incf (&optional (y 1)) +)

;; push at the end of a list 
(define-modify-macro my-push (val)
  (lambda (lst val) (append lst (list val))))

;;; Macro Utilities
;; for macro 
(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))
;; for-test
(defun for-test ()
  (for x 1 8
    (princ x)))

;; in macro, returns true if its first argument is eql 
;; to any of the other arguments. 
(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))
;; if obj is a form, it will be multiple evaluated!
(defmacro in (obj &rest choices)
  `(or ,@(mapcar #'(lambda (c) `(eql ,obj ,c))
                 choices)))
;; in test 
(defun in-test ()
  (in (+ 20 4) 1 2 3 24))

;; random-choice macro, randomly chooses an argument to evaluate.
(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
            (mapcar #'(lambda (expr)
                        `(,(incf key) ,expr))
                    exprs))))
;; random-choice test 
(defun random-choice-test ()
  (setf (symbol-function 'funa) #'(lambda () 1))
  (setf (symbol-function 'funb) #'(lambda () 2))
  (for i 1 5
    (princ (random-choice (funa) (funb)))))

;; avg macro 
(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))
;; avg test 
(defun avg-test ()
  (avg 1 2 33 5))

;; avoid declaring a lot of gensyms 
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))
;; with-gensyms test
(defmacro ntimes-gen (n &rest body)
  (with-gensyms (h g)
    `(let ((,h ,n))
       (do ((,g 0 (1+ ,g)))
           ((> ,g ,h))
         ,@body))))

;; intentional variable capture. 
;; the next-method-p and call-next-method also use this
;; technique
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
;; aif test 
(defun aif-test ()
  (aif 11 2 3))


;;; Exercises
;; ex1  
;; a) `(,z ,x z)
;; b) `(x ,y ,@z)
;; c) `((,@z ,x) z)

;; ex2 Define if in terms of cond. 
(defmacro cif (test then &optional else)
  `(cond (,test ,then)
         (t ,else)))

;; ex3 Define a macro that takes a number n followed 
;; by one or more expressions, and returns the value 
;; of the nth expression
(defmacro n-expr (n &rest exprs)
  (if (and (integerp n) (<= n (length exprs)))
      (nth (1- n) exprs)
      `(case ,n
         ,@(let ((key 0))
                (mapcar #'(lambda (expr)
                            `(,(incf key) ,expr))
                        exprs)))))

;; ex4 Define ntimes to expand into a (local) recursive 
;; function instead of a do. 
(defmacro ntimes (n &rest body)
  (with-gensyms (h fun)
    `(let ((,h ,n))
       (labels ((,fun (i)
                  (when (< i ,h)
                    ,@body
                    (,fun (1+ i)))))
         (,fun 0)))))

;; ex5 Define a macro n-of that takes a number n and an 
;; expression, and returns a list of n successive values
;; returned by the expression
;; ex5-V1 n-of do 
(defmacro n-of (n start)
  (with-gensyms (begin stop i res)
    `(let ((,begin ,start) (,stop ,n))
       (and (integerp ,stop) 
            (> ,stop 0)
            (do ((,i 0 (1+ ,i))
                 (,res nil (cons (+ ,i ,begin) ,res)))
                ((= ,stop ,i) (nreverse ,res)))))))
;; ex5-V2 n-of recursive 
(defmacro n-of (n start)
  (with-gensyms (begin stop f)
    `(let ((,begin ,start) (,stop ,n))
       (and (integerp ,stop) 
            (> ,stop 0)
            (labels ((,f (n res)
                       (if (zerop n)
                           (nreverse res)
                           (,f (1- n) (cons (1+ (car res)) res)))))
              (,f (1- ,stop) (list ,begin)))))))

;; ex6 nice solution
(defmacro retain (params &rest body)
  `((lambda ,params ,@body)
    ,@params))
(defun retain-test ()
  (let ((a 1) (b 2) (c 3) (d 4))
    (retain (a b c)
            (setf a 10 b 20 c 30 d 40)
            (format t "a=~A b=~A c=~A d=~A~%" a b c d))
    (format t "a=~A b=~A c=~A d=~A" a b c d)))

;; ex7 wrong push test 
(defmacro push-w (obj lst)
  `(setf ,lst (cons ,obj ,lst)))
(defun push-w-test () ; => #((1) (42 3) (3))
  (let ((i 0)
        (arr #((1) (2) (3))))
    (push-w 42 (aref arr (incf i)))
    arr))
(defun push-test () ; => #((1) (42 2) (3))
  (let ((i 0)
        (arr #((1) (2) (3))))
    (push 42 (aref arr (incf i)))
    arr))

;; ex8 double its argument
(define-modify-macro double ()
    (lambda (val) (* val 2)))

