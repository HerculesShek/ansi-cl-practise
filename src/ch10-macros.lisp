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

;; avoid declare a lot of gensyms 
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
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
(defun aif-test ()
  (aif 11 2 3))

