;; bubble sort 
(defun bubble-sort (v)
  (do ((i 1 (1+ i)))
      ((= i (length v)))
    (do ((j (1- (length v)) (1- j)))
        ((< j i))
      (if (< (svref v j) (svref v (1- j)))
          (rotatef (svref v j) (svref v (1- j)))))))
