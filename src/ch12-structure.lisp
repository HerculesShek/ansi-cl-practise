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

;; our mapcan
(defun our-mapcan (fn &rest lsts)
  (apply #'nconc (apply #'mapcar fn lsts)))

;; suppose children function get the list of children of a node
;; this function gets the grand children of a node
(defun grandchildren (x)
  (mapcan #'(lambda (c)
               (copy-list (children c)))
           (children x)))

;; nondestructive variant of mapcan 
(defun mappend (fn &rest lsts )
  (apply #'append (apply #'mapcar fn lsts)))

;; definition using mappend
(defun grandchildren (x)
  (mappend #'children (children x)))


;;; destructive and usefull BST in practise 
;;; 因为是破坏性的原因，本次的BST结构都有一个虚拟根节点
;; node structure 
(defstruct (node (:print-function
                  (lambda (node s d) ; 打印函数必须接受三个参数
                    (format s "#<~A>" (node-elt node)))))
  elt
  (l nil)
  (r nil))
(defun root-insert! (obj v-root <)
  (bst-insert! obj (node-elt v-root) < v-root :e))
;; insert 非平衡的BST插入
;; obj---要查入到树中的元素的值
;; bst---二叉搜索树
;; <---表示的是比较函数 不是小于运算符
(defun bst-insert! (obj bst < par dir)
  (if (null bst)
      (case dir
        (:l (setf (node-l par) (make-node :elt obj)))
        (:r (setf (node-r par) (make-node :elt obj)))
        (:e (setf (node-elt par) (make-node :elt obj))))
      (let ((elt (node-elt bst)))
        (if (eql elt obj) ; 已经存在此元素则不再插入 base case
            bst
            (if (funcall < obj elt)
                (bst-insert! obj (node-l bst) < bst :l)
                (bst-insert! obj (node-r bst) < bst :r))))))

(defun root-find (obj v-root <)
  (bst-find obj (node-elt v-root) <))
;; search
(defun bst-find (obj bst <)
  (if bst
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

;;; virtual root 
(defun root-min (v-root)
  (bst-min (node-elt v-root)))
;; min
(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))
;;; virtual root 
(defun root-max (v-root)
  (bst-max (node-elt v-root)))
;; max 
(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

;; replace-node 
(defun rp-node (old new)
  (setf (node-elt old) (node-elt new)
        (node-l old) (node-l new)
        (node-r old) (node-r new)))

;; delete min
(defun bst-delete-min (v-root)
  (bst-del-min (node-elt v-root) v-root :e))
;;; 删除掉bst中的最小的元素，破坏性的
;;; par为bst的父节点，dir表明bst是par的左分支还是右分支
;;; bst不是nil，dir为 :l 或 :r
(defun bst-del-min (bst par dir)
  (if (node-l bst)
      (bst-del-min (node-l bst) bst :l)
      (if (node-r bst)
          (rp-node bst (node-r bst))
          (case dir
              (:l (setf (node-l par) nil))
              (:r (setf (node-r par) nil))
              (:e (setf (node-elt par) nil))))))

;; delete max 
(defun bst-delete-max (v-root)
  (bst-del-max (node-elt v-root) v-root :e))
;;; 删除掉bst中的最大的元素，破坏性的
;;; par为bst的父节点，dir表明bst是par的左分支还是右分支
;;; bst不是nil，dir为 :l 或 :r
(defun bst-del-max (bst par dir)
  (if (node-r bst)
      (bst-del-max (node-r bst) bst :r)
      (if (node-l bst)
          (rp-node bst (node-l bst))
          (case dir
            (:l (setf (node-l par) nil))
            (:r (setf (node-r par) nil))
            (:e (setf (node-elt par) nil))))))

;; delete obj from bst
(defun bst-del (obj v-root <)
  (bst-delete obj (node-elt v-root) v-root :e))
;;; 从bst中删除一个元素
;;; par是bst的父节点，dir有3个值 l左孩子，r有孩子，e说明par是虚拟根节点
(defun bst-delete (obj bst < par dir)
  (if bst
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            (percolate bst par dir)
            (if (funcall < obj elt)
                (bst-delete obj (node-l bst) < bst :l)
                (bst-delete obj (node-r bst) < bst :r))))))
;;; 渗透bst
(defun percolate (bst par dir)
  (let ((l (node-l bst)) (r (node-r bst)))
    (cond ((null l) (if (null r)
                        (case dir
                          (:l (setf (node-l par) nil))
                          (:r (setf (node-r par) nil))
                          (:e (setf (node-elt par) nil)))
                        (rp-node bst r)))
          ((null r) (rp-node bst l))
          (t (if (zerop (random 2)) ;; 这里随机选择前驱或者是后继
                 (progn
                   (setf (node-elt bst) (node-elt (bst-max (node-l bst))))
                   (bst-del-max (node-l bst) bst :l))
                 (progn 
                   (setf (node-elt bst) (node-elt (bst-min (node-r bst))))
                   (bst-del-min (node-r bst) bst :r)))))))

;; print 前序
(defun print-root (v-root)
  (print-bst (node-elt v-root)))
(defun print-bst (bst)
  (when bst
    (format t "~A <--~A--> ~A~%" (node-l bst) bst (node-r bst))
    (print-bst (node-l bst))
    (print-bst (node-r bst))))

;; 中序输出 排序
(defun root-traverse (fn v-root)
  (bst-traverse fn (node-elt v-root)))
(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

;;; 为了递归更好的工作，定义一个根
(defun make-a-bst ()
  (make-node))

;; BST的测试函数
(defun bst-test()
  (let ((nums (make-a-bst)))
    (progn
      (format t "insert 5 8 4 2 1 9 6 7 3 ...~%")
      (dolist (x '(5 8 4 2 1 9 6 7 3)) ; insert nums
        (root-insert! x nums #'<))

      (format t "if 12 exists? ~A" 
              (if (root-find 12 nums #'<) "yes" "no"))
      (format t "~%if 9 exists? ~A" 
              (if (root-find 9 nums #'<) "yes" "no"))
      (format t "~%min:~t~A" (root-min nums))
      (format t "~%max:~t~A" (root-max nums))
      (format t "~%before remove:~%print:~%")
      (print-root nums)
      (format t "bst-traverse: ")
      (root-traverse #'(lambda (x) (format t "~A, " x)) nums)
      (bst-del 2 nums #'<)
      (format t "~%after remove 2:~%print:~%")
      (print-root nums)
      (format t "bst-traverse:~%~T")
      (root-traverse #'princ nums))))


