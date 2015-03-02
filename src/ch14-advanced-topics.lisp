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


;; copy binary files 
(defun copy-file (from to)
  (with-open-file (in from :direction :input
                      :element-type 'unsigned-byte)
    (with-open-file (out to :direction :output
                         :element-type 'unsigned-byte)
      (do ((i (read-byte in nil -1)
              (read-byte in nil -1)))
          ((minusp i))
        (declare (fixnum i))
        (write-byte i out)))))


(set-macro-character #\'
                     #'(lambda (stream char)
                         (list (quote quote) (read stream t nil t))))
