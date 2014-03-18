;; in my opinion, this exercise inspect the single directed linked list
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







;; from takafumi@shido.info
(defconstant maxword 100)

(defun punc (c) ;; char -> symbol
  (case c
	(#\. '|.|) (#\, '|,|) (#\; '|;|)
	(#\! '|!|) (#\? '|?|) )) 

(defun terminal (sy)
  (or (eq sy '|.|) (eq sy '|!|) (eq sy '|?|) (eq sy '|:|)))

;; Is it written by Henley? The parameter is a file name.
(defun henleyp (fi)
  (let ((buffer (make-string maxword))
		(pos 0) (nwls nil) (nw 0))
    (with-open-file (s fi :direction :input)
      (do ((c (read-char s nil :eof)
			  (read-char s nil :eof)))
		  ((eql c :eof))
		(if (or (alpha-char-p c) (char= c #\'))
			(progn
			  (setf (aref buffer pos) c)
			  (incf pos))
			(progn
			  (unless (zerop pos)
				(incf nw)
				(setf pos 0))
			  (let ((p (punc c)))
				(when p
				  (if (terminal p)
					  (progn
						(push nw nwls)
						(setf nw 0))
					  (incf nw))))))))
    (anal-cwlist nwls)))

(defun hispos (x r mn n)
  (let ((p (truncate (- x mn) r)))
    (if (= p n) (- p 1) p)))

(defun nstar (n)
  (make-string n :initial-element #\*))

(defun anal-cwlist (cwls)
  (let ((mx (apply #'max cwls))
        (mn (apply #'min cwls))
        (a (make-array 5 :initial-element 0)))
    (if (< 60 mx)
        (progn
          (format t "more than 60 words in one sentence.~%")
          t)
		(let ((r (/ (- mx mn) 5)))
		  (dolist (x cwls)
			(incf (aref a (hispos x r mn 5))))
		  (let* ((j mn)
				 (hmax (max (aref a 0) (aref a 1) (aref a 2) (aref a 3) (aref a 4)))
				 (n* (/ hmax 20.0)))
			(format t "* = ~A sentences~%" (if (< n* 1.0) 1.0 n*) )
			(dotimes (i 5)
			  (format t "~2D-~2D:~A~%"
					  (truncate j)
					  (+ (truncate (incf j r)) (if (= i 4) 1 0))
					  (nstar (if (< n* 1.0) (aref a i) (truncate (/  (aref a i) n*)))))))
		  (if (< (aref a 3) (aref a 4))
			  t
			  nil)))))

