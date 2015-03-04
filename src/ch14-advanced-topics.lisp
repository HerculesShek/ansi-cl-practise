;;;; this file contains demos and exercise of chapter 14

(defun cdr-circular? (lst &optional pres)
  (if (null lst)
      nil
      (if (member lst pres)
          t
          (cdr-circularp (cdr lst) (cons lst pres)))))

(deftype proseq ()
  '(or vector (and list (not (satisfies cdr-circular?)))))

;; TODO here is something wrong 
(deftype multiple-of (n)
  `(and integer (satisfies (lambda (x)
                             (zerop (mod x ,n))))))


;; copy files, not only binary  
(defun my-copy-file (from to)
  (with-open-file (in from :direction :input
                      :element-type 'unsigned-byte)
    (with-open-file (out to :direction :output
                         :element-type 'unsigned-byte)
      (do ((i (read-byte in nil -1)
              (read-byte in nil -1)))
          ((minusp i))
        (declare (fixnum i))
        (write-byte i out)))))

;; quote read macro
(set-macro-character #\'
                     #'(lambda (stream char)
                         (list (quote quote) (read stream t nil t))))


(set-dispatch-macro-character #\# #\?
                              #'(lambda (stream char1 char2)
                                  (list 'quote
                                        (let ((lst nil))
                                          (dotimes (i (+ (read stream t nil t) 1))
                                            (push i lst))
                                          (nreverse lst)))))


(set-macro-character #\} (get-macro-character #\)))

(set-dispatch-macro-character #\# #\{
                              #'(lambda (stream char1 char2)
                                  (let ((accum nil)
                                        (pair (read-delimited-list #\} stream t)))
                                    (do ((i (car pair) (+ i 1)))
                                        ((> i (cadr pair))
                                         (list 'quote (nreverse accum)))
                                      (push i accum)))))


;; giant loop!
(defun even/odd (ns)
  (loop for n in ns
     if (evenp n)
     collect n into evens
     else collect n into odds
     finally (return (values evens odds))))

(defun sum (n)
  (loop for x from 1 to n
     sum x))

;; get the most value and the corresponding element 
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max  score))))
        (values wins max))))

(defun num-year (n)
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d))))
      (do* ((y yzero (+ y 1))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev))))))


(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (loop with wins = (car lst)
         with max = (funcall fn wins)
         for obj in (cdr lst)
         for score = (funcall fn obj)
         when (> score max)
           (do (setf wins obj
                     max score)
               finally (return (values wins max))))))

(defun num-year (n)
  (if (< n 0)
      (loop for y downfrom (- yzero 1)
         until (<= d n)
         sum (- (year-days y)) into d
         finally (return (values (+ y 1) (- n d))))
      (loop with prev = 0
         for y from yzero
         until (> d n)
         do (setf prev d)
         sum (year-days y) into d
         finally (return (values (- y 1)
                                 (- n prev))))))


(loop for y = 0 then z
   for x from 1 to 5
   sum 1 into z
   finally (return y z))

(loop for x from 1 to 5
   for y = 0 then z
   sum 1 into z
   finally (return y z))


