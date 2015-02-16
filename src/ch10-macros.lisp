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
