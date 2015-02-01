;;;; source file for chapter 8 of ANSI Common Lisp

;; test symbol-name
(defun test-symbol-name ()
  (symbol-name '||))

;; test special symbols
;; => (|Lisp 1.5| || |will| NAME)
(defun visit-special-symbols () 
  (list '|Lisp 1.5| '|| '|will| '|NAME|))

;; special symbols & let  
(defun special-symbols-test () ; => 9
  (let ((|| 2)
        (|#\Newline| 3)
        (|42| 4))
    (+ || |#\Newline| |42|)))

;; Error! lexical variable is not bound to a symbol!
(defun variable-symbol-test ()
  (let ((v 42))
    (symbol-value 'v))) ; error!
;; But this is ok, name is a special variable bound to a symbol 
(defun variable-symbol-test2 ()
  (setf name "will")
  (symbol-value 'name))

;;; The code of random test generater, go to ch8-symbols-random-text.lisp


;; Exercises
;; ex1 Yes if they're in different packages
;; ex2 "FOO" 3 bytes 
;; 'FOO name package variable function attribute-list 20+ bytes
;; ex3 a serious question, if defpackage with symbol, the symbol is interned in the package implicitly
;; ex4 reference to ch8-symbols-ex4.lisp
;; ex5 read-text a Henley's poem to generate the *words*, then judge a string from the first words as prev found from *words* 
;; the file ch8-symbols-ex5.lisp contains the solution



