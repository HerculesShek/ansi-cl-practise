

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
          
    
