;;;; this source file contains the solution for ex4 of chapter 8 and test ok 

;; String Substitution -- an execllent example!
(defpackage "RING"
  (:use "COMMON-LISP")
  (:export "BUF" "BREF" "NEW-BUF" "BUF-INSERT" 
           "BUF-POP" "BUF-NEXT" "BUF-RESET" 
           "BUF-CLEAR" "BUF-FLUSH"))
(in-package ring)

(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len :initial-element nil)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1 
      (bref b (incf (buf-start b)))
    (buf-reset b)))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new b) (buf-end b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used b) -1
        (buf-new b) -1 (buf-end b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))

(defpackage "FILE"
  (:use "COMMON-LISP" "RING")
  (:export "FILE-SUBST" "FILE-SUBST-TEST"))
(in-package file)

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                         :if-exists :supersede)
      (stream-subst old new in out))))

(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))  ; setf返回最后一个参数求值结果
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)            ; 3 字符相等 并且 现在是全部相等
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         ; 2 字符相等又不是从buf中获取的。插入buf中，buf中的标位除了end都不动
                    (buf-insert c buf))))
            ((zerop pos)                   ; 1 字符至今没有匹配上
             (princ c out)
             (when from-buf                
               (buf-pop buf)))
            (t                             ; 4 之前有相等的，但是现在不相等了
             (unless from-buf              
               (buf-insert c buf))         
             (princ (buf-pop buf) out)     
             (setf pos 0))))
    (buf-flush buf out)))

(defun file-subst-test ()
  (file-subst "baro" "baric" "a.txt" "b.txt"))
