;;;; this file contains demos and exercise of chapter 13

;; If a major bottleneck occurred in the inner loop of some function, add a declaration
(defun bottleneck (&rest xx)
  (do ()
      ()
    (do ()
        ()
      (declare (optimize (speed 3) (safety 0)))
      )))

;; To ask globally for the fastest possible code, regardless of the consequences,
(declaim (optimize (speed 3)
                   (compilation-speed 0)
                   (safety 0)
                   (debug 0)))

;; not tail call
(defun length/r (lst)
  (if (null lst)
      0
      (1+ (length/r (cdr lst)))))
;; tail call 
(defun length/rt (lst)
  (labels ((len (lst acc)
             (if (null lst)
                 acc
                 (len (cdr lst) (1+ acc)))))
    (len lst 0)))


