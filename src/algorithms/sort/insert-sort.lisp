
;; classic insertion sort 
;; array is a simple vector 
(defun insertion-sort (array)
  (do ((j 1 (1+ j)))
      ((= (length array) j))
    (let ((key (svref array j)))
      (do ((i (1- j) (1- i)))
          ((or (< i 0) (< (svref array i) key))
           (setf (svref array (1+ i)) key))
        (rotatef (svref array (1+ i)) (svref array i))))))

