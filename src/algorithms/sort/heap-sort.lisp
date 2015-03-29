;; heap sort by array
(defun heap-sort (arr)
  )

(defparameter heap-size 0)

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
      
        
  
