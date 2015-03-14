;; string permutation
(defun string-permutation (str)
  (u "" str))

(defun u (c rest)
  (let (lst)
    (if (= 1 (length rest))
        (push (format nil "~A~A" c rest) lst)
        (do ((i 0 (1+ i)))
            ((= i (length rest)))
          (dolist (s (u (char rest i) (make-str rest i)))
            (push (format nil "~A~A" c s) lst))))
    lst))

(defun make-str (str i)
  (concatenate 'string (subseq str 0 i) (subseq str (1+ i))))
