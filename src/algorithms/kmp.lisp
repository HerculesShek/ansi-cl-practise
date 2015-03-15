;; just match the first occurence of p in tar using KMP
(defun kmp-matcher (tar p)
  (let ((n (length tar)) 
        (m (length p))
        (r (compute-prefix p))
        (p-idx 0)
        (t-idx 0))
    (do ()
        ((or (>= p-idx m) (>= t-idx n)))
      (cond ((char= (char tar t-idx) (char p p-idx))
             (incf p-idx)
             (incf t-idx))
            ((= 0 p-idx)
             (incf t-idx))
            (t
             (setf p-idx (1+ (svref r (1- p-idx)))))))
    (if (= p-idx m)
        (- t-idx p-idx)
        -1)))

;; compute prefix 
(defun compute-prefix (str)
  (let* ((m (length str))
         (r (make-array m))
         (k -1))
    (setf (svref r 0) -1)
    (do ((q 1 (1+ q)))
        ((= q m))
      (do ()
          ((or (< k 0) (char= (char str (1+ k)) (char str q))))
        (setf k (svref r k)))
      (if (char= (char str (1+ k)) (char str q))
          (incf k))
      (setf (svref r q) k))
    r))
          
    
