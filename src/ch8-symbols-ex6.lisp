;;;; This source file contains the solution for the ex6 of chapter 8

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
    (let ((pair (assoc symb (gethash prev *fwords*)))
          (pair2 (assoc prev (gethash symb *bwords*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *fwords*))
          (incf (cdr pair)))
      (if (null pair2)
          (push (cons prev 1) (gethash symb *bwords*))
          (incf (cdr pair2))))
    (setf prev symb)))

;; PART II
;; generate a sentence: take a word and generate a sentence 
;; with that word in the middle of it. 
(defun generate-sen (w)
  )

;; get a word according to the pre word 
(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices
                            :key #'cdr))))
    (if choices
        (dolist (pair choices)
          (if (minusp (decf i (cdr pair)))
              (return (car pair)))))))



