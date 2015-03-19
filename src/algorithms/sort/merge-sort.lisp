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
        (r-arr (make-array (- to mid))))
    (do ((i 0 (1+ i)))
        ((= i (length l-arr)))
      (setf (svref l-arr i) (svref arr (+ from i))))
    (do ((i 0 (1+ i)))
        ((= i (length r-arr)))
      (setf (svref r-arr i) (svref arr (+ mid i 1))))
    
    
  
