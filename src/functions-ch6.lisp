;;;; for demos and practise in the chapter 6

;; symbol-function
(defun symbol-function-test ()
  (symbol-function 'car))

;; function name is (setf f)
;; > (paimo-test )
;; (42 B C)
(defun (setf primo) (val lst)
  (setf (car lst) val))
(defun paimo-test ()
  (let ((x (list 'a 'b 'c)))
    (setf (primo x) 42)
    x))

;; documentation --Standard Generic Function
(defun foo (x)
  "Implements an enhanced paradigm of diversity"
  x)
(defun foo-documentation-test ()
  (let ((foo-doc (documentation 'foo 'function)))
    (if foo-doc
        (format t "foo-doc is ~A~%" foo-doc)))
  (setf (documentation 'foo 'function) "new doc for foo")
  (format t "new foo-doc is ~A" (documentation 'foo 'function)))

;; labels test -- bad defined
(defun labels-test()
  (labels ((a (x) (b x))
           (b (x) (a x)))
    (a 3)))

;; transfer do to labels
(defun trans-do-to-labels ()
  (let ((res
         (do ((i 1 (incf i))
              (j 1 (incf j)))
             ((= i j 10) 'done)
           (format t "~A*~A=~A~%" i j (* i j)))))
    (format t "res is ~A~%~%" res))
  (labels ((rec (i j)
             (cond ((= i j 10) 
                    'done)
                   (t
                    (format t "~A*~A=~A~%" i j (* i j))
                    (rec (incf i) (incf j))))))
    (rec 1 1)))
                   
;; rest demo
(defun do-rest (&rest args)
  (type-of args))
(defun rest-demo(&rest args)
  args)
;; funcall implementation of apply
(defun our-funcall (fn &rest args)
  (apply fn args))

;; optional demo #1
(defun optional-1 (&optional arg)
  (list arg))
;; optional demo #2
(defun do-optional1(&optional (a 'fun)  b)
  (list a b))

;; rest and optional
(defun rest-optional-test (&optional (a "Will") b &rest c)
  (append (list a b) c))

;;; &key demo
;; default keyword parameter is nil
(defun key-test (a &key b c)
  (list a b c))
;; &key and &rest #1 OK &key can not be followed by &rest
(defun key-rest-test (a &key b (name "Will"))
  (list a b name))
;; &key and &rest #2 error! Can't compile the function!
(defun key-rest-test-err (a &key b (name "Will") &rest res)
  (list a b name res))
;; &key and &optional test error! 
;; &key can not be followed by &optional
(defun key-optional-test-err (a &key (name "Will") &optional c d)
  (list a name c d))
;; but &optional can be followed by &key
(defun key-optional-test (&optional c d &key (name "Will"))
  (list c d name))

;; define our adjoin 
(defun our-adjoin (obj lst &rest args)
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))


;;; Utilities
;; wether lst is a proper list and just has only ont element
(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

;; append obj to the end of lst
(defun append1 (lst obj)
  (append lst (list obj)))

;; get a list of results of calling the function on the integers [0, n-1]
(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))
;; map-int test #1
(defun map-int-test ()
  (map-int #'(lambda (x) (identity x)) 10))
;; map-int test #2
(defun map-int-test-2 ()
  (map-int #'(lambda (x) (random 100)) 10))

;; apply a function to every element of lst, return the non-nil values
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((var (funcall fn x)))
        (if var (push var acc)))) ; (if var (push x acc))))
    (nreverse acc)))

;; get the highest and the element according to the scoring function 
(defun most(fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))

;;; Closures
;; combine obj according to the type
(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))z
(defun combine (&rest args)
  (apply (combiner (car args))
         args))

;; generate a closure using a free variable num, and apply it to lst
(defun add-to-list (num lst)
  (mapcar #'(lambda (x)
              (+ x num))
          lst))

;; 2 closures sharing a same free varialbe
(let ((counter 0))
  (defun reset()
    (setf counter 0))
  (defun stamp()
    (incf counter))
  (list (reset) (stamp) (stamp) (reset) (stamp)))

;; generete a complement of function f
(defun our-complement (f)
  #'(lambda (&rest args)
      (not (apply f args))))

;;; Dylan programming language derives from Scheme and Common Lisp 
;;; Dylan methods implemented in Common Lisp, Awesome!
;;; functions returning functions

;; #1 compose functions to one function
;; (compose #'a #'b #'c) 
;; == #'(lambda (&rest args) (a (b (apply #'c args))))
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns) ;; from end
    #'(lambda (&rest args)
        (reduce #'(lambda (res f)
                    (funcall f res)) ;; here is funcall, the precedes functions only take 1 argument
                rest
                :initial-value (apply fn1 args))))) ;; here is apply, so the last function to compose can take any number of arguments
;; compose test 
(defun compose-test () ; => 4
  (funcall (compose #'(lambda (x) (* x x))
                    #'(lambda (x) (- x 42))
                    #'+) 21 2 21))

;; take some predicate functions and args should satisfy one of them
(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
        #'(lambda (&rest args)
            (or (apply fn args) (apply disj args)))))) ;; it's not effective

;; take some predicates functions and args should satisfy all of them
(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
        #'(lambda (&rest args)
            (and (apply fn args) (apply conj args))))))

;; 
(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

;; 
(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(defun always (x)
  #'(lambda (&rest args) ;; warnings for unused lexical variable args
      x))

;; dynamic scope
(defun ds-1()
  (let ((x 10))
    (defun foo()
      x))
  
  (let ((x 20))
    (foo)))

(defun ds-2()
  (let ((x 10))
    (defun foo()
      (declare (special x))
      x))
  
  (let ((x 20))
    (declare (special x))
    (foo)))

(let ((*print-base* 16))
  (princ 32))

;; using recursion
(defun fib (n)
  (if (<= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defun fib-do (n)
  (do ((i n (- i 1))
       (f1 1 (+ f1 f2))
       (f2 1 f1))
      ((<= i 1) f1)))

;; exercises
;; ex1
(defun tokens (str &key (test #'constituent) (start 0)) ;; 
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str :test test :start p2))))))) ;; 
(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\Space))))
;; ex2
(defun bin-search (obj vec &key (key #'identity) (test #'equal) (start 0) end) ;; [start end]
  (let* ((len (length vec))
         (e (if (and end (integerp end)) end (- len 1)))) 
    (and (not (zerop len))
         (<= start e)
         (finder obj vec start e key test))))

(defun finder (obj vec start end key test)
  (let ((range (- end start)))
    (if (zerop range)
        (if (funcall test obj (funcall key (svref vec start)))
            (svref vec start)
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (funcall key (svref vec mid))))
            (if (> obj obj2)
                (finder obj vec (+ mid 1) end key test)
                (if (< obj obj2)
                    (finder obj vec start (- mid 1) key test)
                    (svref vec mid))))))))
;; ex3
(defun n-params (&rest args)
  (length args))
;; ex4
(defun most-2 (fn lst)
  (cond 
    ((null lst) (values nil nil))
    ((null (cdr lst)) (values (car lst) nil))
    (t 
     (let* ((e1 (car lst))
            (e2 (cadr lst))
            (v1 (funcall fn e1))
            (v2 (funcall fn e2)))
       (when (< v1 v2)
         (rotatef e1 e2)
         (rotatef v1 v2))
       (dolist (elt (cddr lst))
         (let ((v (funcall fn elt)))
           (cond 
             ((< v1 v) (setf e2 e1 v2 v1 e1 elt v1 v))
             ((< v2 v) (setf e2 elt v2 v))
             (t nil))))
       (values e1 e2)))))
;; ex5
(defun remove-if-1 (fn lst)
  (filter-5 #'(lambda (x) (not (funcall fn x))) lst))
(defun filter-5 (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((var (funcall fn x)))
        (if var (push x acc)))) ;; modified
    (nreverse acc)))
(defun remove-if-2 (fn lst)
  (let ((acc nil))
    (dolist (obj lst)
      (if (not (funcall fn obj))
          (push obj acc)))
    (nreverse acc)))
;; ex6
(let ((max 0))
  (defun max-fo-far (n)
    (and (numberp n)
         (progn 
           (setf max (if (< max n) n max))
           max))))
;; ex7
(let ((pre))
  (defun pre (n)
    (if (not (numberp n))
        (error "You should pass a number!")
        (if pre
            (progn 
              (let ((temp pre))
                (setf pre n)
                (< temp pre)))
            (progn
              (setf pre n)
              nil)))))
;; ex7-prog1
(let ((pre))
  (defun pre-prog1 (n)
    (if (not (numberp n))
        (error "You should pass a number!")
        (prog1
            (and pre (< pre n))
          (setf pre n)))))
;; ex8
(defun expensive (n)
  (multiple-value-bind (res) (round (/ (* n (random 100)) 100))
    res))
(let ((respo (make-array 101 :initial-element nil)))
  (defun frugal (n)
    (or (svref respo n)
        (setf (svref respo n) (expensive n))))))
;; ex9 
(defun apply-octal (&rest args)
  (let ((*print-base* 8))
    (apply #'apply args)))

