;;;; this source file contains the solution for the ex5 of chapter 8

;;; In my opinion, this exercise inspects the single directed linked list
;;; To verify whether or not a quote was produced by Henley
;;; We're supposed that the origin article is stored in a text "some.txt"
;;; first (read-txt "/path/to/some.txt")
;;; second (henleyp-sentence "Wast present, and with mighty wings outspread" #'my-char)
;;; of course, you can replace string stream with file stream
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

;; simple direction linked list 
(defun henleyp-sentence (sen &optional (test #'my-char))
  (multiple-value-bind (first index) (next-w sen test 0)
    (if first
        (let ((first-sym (intern (string-downcase first))))   ;; string-downcase can turn a char into a string  #\A => "a"
          (if (gethash first-sym *words*)
              (henleyp-process sen first-sym test index)
              nil)))))

(defun tokens (sen test &optional (start 0)) ;; test next-w
  (multiple-value-bind (word index) (next-w sen test start)
    (when word
      (format t "~A index is ~A   " word index)
      (format t "length is ~A ~%" (if (characterp  word) 1 (length word)))
      (if index
          (tokens sen test index)))))

(defun henleyp-process (sen pre-sym test start)
  (let ((choices (gethash pre-sym *words*)))
    (multiple-value-bind (next index) (next-w sen test start)
      (let ((next-sym (intern (string-downcase next))))    ;; string-downcase can turn a char into a string  #\A => "a"
        (if (and next (member next-sym choices :key #'car))
            (if (null index)
                t
                (henleyp-process sen next-sym test index)))))))

(defun next-w (sen test start)
  (let ((p1 (position-if test sen :start start)))
    (if p1
        (let* ((p2 (position-if #'(lambda (x)
                                    (not (funcall test x)))
                                sen :start p1))
               (prefix (position-if #'punc sen :start start :end p1)))
          (if prefix
              (values (char sen prefix) (1+ prefix))
              (values (subseq sen p1 p2) p2)))
        (values nil nil))))

(defun my-char (c)
  (or (alpha-char-p c)
      (char= c #\')
      (char= c #\-)))




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

