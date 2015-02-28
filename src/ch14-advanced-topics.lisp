;;;; this file contains demos and exercise of chapter 14


(defun cdr-circular? (lst &optional pres)
  (if (null lst)
      nil
      (if (member lst pres)
          t
          (cdr-circularp (cdr lst) (cons lst pres)))))

(deftype proseq ()
  '(or vector (and list (not (satisfies cdr-circular?)))))
