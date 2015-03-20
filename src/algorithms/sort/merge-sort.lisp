;; merge sort 
(defun merge-sort (arr)
  (msort arr 0 (1- (length arr))))

(defun msort (arr from to)
  (when (< from to)
    (let ((mid (+ from (floor (/ (- to from) 2)))))
      (msort arr from mid)
      (msort arr (1+ mid) to)
      (my-merge arr from mid to))))

(defun my-merge (arr from mid to)
  (let ((l-arr (make-array (1+ (- mid from))))
        (r-arr (make-array (- to mid)))
        (l-idx 0)
        (r-idx 0))
    (do ((i 0 (1+ i)))
        ((= i (length l-arr)))
      (setf (svref l-arr i) (svref arr (+ from i))))
    (do ((i 0 (1+ i)))
        ((= i (length r-arr)))
      (setf (svref r-arr i) (svref arr (+ mid i 1))))
    
    (do ((idx from (1+ idx)))
        ((> idx to))
      (if (< (svref l-arr l-idx) (svref r-arr r-idx))
          (progn 
            (setf (svref arr idx) (svref l-arr l-idx))
            (incf l-idx)
            (when (= l-idx (length l-arr))
              (copy-rest arr (1+ idx) r-arr r-idx)
              (return-from my-merge)))
          (progn 
            (setf (svref arr idx) (svref r-arr r-idx))
            (incf r-idx)
            (when (= r-idx (length r-arr))
              (copy-rest arr (1+ idx) l-arr l-idx)
              (return-from my-merge)))))))

(defun copy-rest (arr idx arr2 idx2)
  (do ()
      ((= idx2 (length arr2)))
    (setf (svref arr idx) (svref arr2 idx2))
    (incf idx)
    (incf idx2)))

    
    
  
