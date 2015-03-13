

;; string permutation
(defun string-permutation (str)
  )

(defun u (c rest)
  (let lst
    (if (= 1 (length rest))
        (cons (concatenate 'string c rest) lst)
        (dolist (e rest)
          




(defun make-str (str i)
  (concatenate 'string (subseq str 0 i) (subseq str (1+ i))))
