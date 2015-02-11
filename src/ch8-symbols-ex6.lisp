;;;; This source file contains the solution for the ex6 of chapter 8

(defpackage "EX6"
  (:use "COMMON-LISP")
  (:export "GENERATE-SEN-TEST"))
(in-package ex6)

;;; Random Text  
;;; take a word and generate a sentence with that word in the middle of it
;;; PART I read from text 
(defparameter *fwords* (make-hash-table :size 10000))
(defparameter *bwords* (make-hash-table :size 10000))
(defconstant *wcount* 4)
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
    (let ((fpair (assoc symb (gethash prev *fwords*)))
          (bpair (assoc prev (gethash symb *bwords*))))
      (if (null fpair)
          (push (cons symb 1) (gethash prev *fwords*))
          (incf (cdr fpair)))
      (if (null bpair)
          (push (cons prev 1) (gethash symb *bwords*))
          (incf (cdr bpair))))
    (setf prev symb)))

;; PART II
;; generate a sentence: take a word and generate a sentence 
;; with that word in the middle of it. 
(defun generate-sen (w)
  (let ((sym (intern (string-downcase w))))
    (do* ((f-next (random-next sym) (random-next f-next))
          (b-next (random-next sym :from-end t) (random-next b-next :from-end t))
          (res (list b-next sym f-next) (append (cons b-next res) (list f-next)))
          (i 1 (1+ i)))
         ((= i *wcount*) res))))

;; get a word according to the preceding word 
(defun random-next (prev &key (from-end nil))
  (let* ((choices (gethash prev (if from-end *bwords* *fwords*)))
         (i (random (reduce #'+ choices
                            :key #'cdr))))
    (if choices
        (dolist (pair choices)
          (if (minusp (decf i (cdr pair)))
              (return (car pair)))))))

(defun generate-sen-test ()
  (read-text "./text/lost.txt")
  (generate-sen "you"))


