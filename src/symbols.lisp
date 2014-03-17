(defpackage "MY-APPLICATION"
  (:use "COMMON-LISP")
  (:nicknames "APP")
  (:export "WIN" "LOSE" "DRAW"))

(in-package my-application)

(defun noise (animal)
  (case animal
	(:dog :woof)
	(:cat :meow)
	(:pig :oink)))


;; Random Text
(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
	(let ((buffer (make-string maxword))
		  (pos 0))
	  (do ((c (read-char s nil :eof)
			  (read-char s nil :eof)))
		  ((eql c :eof))
		(if (or (alpha-char-p c) (char= c #\'))
			(progn 
			  (setf (aref buffer pos) c)
			  (incf pos))
			(progn
			  (unless (zerop pos)
				(see (intern (string-downcase 
							  (subseq buffer 0 pos))))
				(setf pos 0))
			  (let ((p (punc c)))
				(if p (see p)))))))))

(defun punc (c)
  (case c
	(#\. '|.|) (#\, '|,|) (#\; '|;|)
	(#\! '|!|) (#\? '|?|) )) 
 
(let ((prev '|.|))
  (defun see (symb)
	(let ((pair (assoc symb (gethash prev *words*))))
	  (if (null pair)
		  (push (cons symb 1) (gethash prev *words*))
		  (incf (cdr pair))))
	(setf prev symb)))

;; generate text
(defun generate-text (n &optional (prev '|.|))
  (if (zerop n)
	  (terpri)
	  (let ((next (random-next prev)))
		(format t "~A " next)
		(generate-text (1- n ) next))))

(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
		 (i (random (reduce #'+ choices
							:key #'cdr))))
	(dolist (pair choices)
	  (if (minusp (decf i (cdr pair)))
		  (return (car pair))))))


(read-text "./text/lost.txt")
(generate-text 15)

;; Exercises
;; ex1 Yes if they're in different packages
;; ex2 "FOO" 3 bytes 'FOO name package variable function attribute-list 20+ bytes
;; ex3 a serious question TODO!
;; ex4 reference to symbols-ex4.lisp
;; ex5 


