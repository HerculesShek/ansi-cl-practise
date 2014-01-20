(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (equal elt next)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n) (compr next 1 (cdr lst)))))))
      
(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))


(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
	    (rest (uncompress (cdr lst))))
	    (if (consp elt)
		(append (apply #'list-of elt) rest)
		(cons elt rest)))))}

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))


;; copy a tree
(defun my-copy-tree (root)
  (if (atom root)
	  root
	  (cons (my-copy-tree (car root))
			(my-copy-tree (cdr root)))))

;; 自定义subst
(defun my-subst (new old tree)
  (if (eql old tree)
	  new
	  (if (atom tree)
		  tree
		  (cons (my-subst new old (car tree))
				(my-subst new old (cdr tree))))))
	  



