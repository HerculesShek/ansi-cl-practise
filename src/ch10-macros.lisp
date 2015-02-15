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
