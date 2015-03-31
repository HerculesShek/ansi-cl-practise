;; heap sort by array
(defparameter heap-size 0)

(defun heap-sort (arr)
  (setf heap-size (length arr))
  (build-max-heap arr)
  (do ((i (length arr) (1- i)))
      ((< i 2))
    (rotatef (svref arr 0) (svref arr (1- i)))
    (decf heap-size)
    (max-heapify arr 0)))

(defun build-max-heap (a)
  (let ((from (floor (/ heap-size 2))))
    (do ((i from (1- i)))
        ((< i 1))
      (max-heapify a (1- i)))))


(defun max-heapify (a i)
  (let ((l (1- (* 2 (1+ i))))
        (r (* 2 (1+ i)))
        (largest i))
    (if (and (< l heap-size) (> (svref a l) (svref a largest)))
        (setf largest l))
    (if (and (< r heap-size) (> (svref a r) (svref a largest)))
        (setf largest r))
    (when (/= largest i)
      (rotatef (svref a i) (svref a largest))
      (max-heapify a largest))))
      
        
  
