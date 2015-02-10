;;;; this source file is part of symbols-ch8.lisp

;; define a package 
(defpackage "MY-APPLICATION"
  (:use "COMMON-LISP") ; the pacakge my-utilities is removed 
  (:nicknames "APP")
  (:export "WIN" "LOSE" "DRAW" "NOISE" "*WORDS*" 
           "MAXWORD" "READ-TEXT" "PUNC" "SEE"
           "GENERATE-TEXT" "RANDOM-NEXT" "RTG-TEST"))
;; make my-application to be the current package 
(in-package app)
(defparameter win 1)
(defparameter draw 0)
(defparameter lose -1)

;; this function has no symbols belonging to my-application, 
;; it can be used everywhere, because the keywords are special symbols 
(defun noise (animal)
  (case animal
    (:dog :woof)
    (:cat :meow)
    (:pig :oink)))


;;; Random Text
;;; PART I read from text 
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

;; PART II 
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

;; the poem is here 
;; http://www.gutenberg.org/cache/epub/26/pg26.txt
;; or https://www.dartmouth.edu/~milton/reading_room/pl/book_1/text.shtml
(DEFUN RTG-TEST()
  (read-text "./text/lost.txt")
  (generate-text 15))

