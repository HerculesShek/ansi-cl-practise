;;;; this file contains demos and exercise of chapter 12 

;; our tailp
(defun our-tailp (obj lst)
  (or (eql obj lst)
      (and (consp lst)
           (our-tailp obj (cdr lst)))))

;; our copy-list
(defun our-copy-list (lst)
  (if (or (atom lst) (null lst))
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))

;; our copy-tree
(defun our-copy-tree (tree)
  (if (or (atom tree) (null tree))
      tree
      (cons (our-copy-tree (car tree))
            (our-copy-tree (cdr tree)))))

;;; Queues
(defun make-queue () (cons nil nil))
;; put a obj into q
(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))
;; get and remove the front element 
(defun dequeue (q)
  (pop (car q)))      


