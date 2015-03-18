;; quick sort 
(defun quick-sort (arr)
  (qsort arr 0 (1- (length arr))))

(defun qsort (arr p r)
  (if (< p r)
      (let ((q (partition arr p r)))
        (qsort arr p (1- q))
        (qsort arr (1+ q) r))))

(defun partition (arr p r)
  (let ((x (svref arr r))
        (i (1- p)))
    (do ((j p (1+ j)))
        ((> j (1- r)))
      (when (<= (svref arr j) x)
        (incf i)
        (rotatef (svref arr i) (svref arr j))))
    (rotatef (svref arr (1+ i)) (svref arr r))
    (1+ i)))

;; test quick sort 
(defun quick-sort-test ()
  (let ((arr #(3 7 4 2 42 74 23 16 3 2 1 75)))
    (quick-sort arr)
    arr))


