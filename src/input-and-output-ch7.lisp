;;;; source file for chapter 7 of ANSI Common Lisp

;; print the contents of the file to toplevel
(defun pseudo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))

;; get the type of the express parsed by read function
(defun read-1()
  (type-of (read)))


;;; ---------------------------------------------------
;;; String Substitution -- an execllent example!
;;; We should understand the conception of ring buffer 
;;; ---------------------------------------------------
;; structure buf 
;; It has 5 fields: 
;; vec -- a vector contaioning the objects stored in the buffer 
;; start -- pointing the first value in the buffer, will be incremented when pop a value 
;; end -- pointing the last value in the buffer, will be incremented when insert a value
;; used and new are some information about the buffer, are like start and end for the "current match"
;; start <= used <= new <= end 
(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

;; take a buf and a index, return the element stored at that index
(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))
;; setf of bref 
(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

;; get a new buffer able to hold up to len elements 
(defun new-buf (len)
  (make-buf :vec (make-array len :initial-element nil)))

;; insert a new value x into buffer b, increments the end and put the value at that location
(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

;; return the first value in the buffer, and increments start 
(defun buf-pop (b)
  (prog1
      (bref b (incf (buf-start b)))
    (buf-reset b)))

;; get the next value but don't remove it from the buffer b
(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

;; reset the buffer b, make used equal to start and new equal to end 
(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new b) (buf-end b)))

;; clear data form the buffer b 
(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used b) -1
        (buf-new b) -1 (buf-end b) -1))

;; flush a buffer by writing all the live elements to a stream given as the second argument
(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))

;; read from file1 and substitute all old string to new string and write to file2
(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                         :if-exists :supersede)
      (stream-subst old new in out))))

;; the core working function, called by file-subst
(defun stream-subst (old new in out)
  (let* ((pos 0) ; 在old上此次循环要比较的字符的索引
         (len (length old))
         (buf (new-buf len)) ; the length of the buffer is decided by the length of the old string
         (from-buf nil)) ; storing a character from the buf
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))  ;; setf 会返回最后一个参数求值结果
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((char= c (char old pos))      ; 5 从buf中取得的字符，并且是部分相等，这是buffer真正有用的地方
             (incf pos)
             (cond ((= pos len)            ; 3 字符相等 并且 现在是全部相等 这个是最简单的情况
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         ; 2 字符相等又不是从buf中获取的。插入buf中，buf中的标位除了end都不动
                    (buf-insert c buf))))
            ((zerop pos)                   ; 1 字符至今没有匹配上
             (princ c out)
             (when from-buf                ; 如果是从buf中获取的字符，需要pop出并重置
               (buf-pop buf)))
            (t                             ; 4 之前有相等的，但是现在这个字符不相等了
             (unless from-buf              ; 如果不是从buf中拿来的字符 需要加入到buf中，修改end
               (buf-insert c buf))         ; 这里就是为了保留火种！很巧妙，现在暂时不相等以后未必不相等，这个字符说不定和old的第一个字符相等
             (princ (buf-pop buf) out)     ; 在这种情况下，需要将buf中的第一个字符取出来输出并从buffer中清理掉它 这个时候buf中一定是有数据的！并且避免了死循环！这句话是算法的一个核心，实际上，在buffer中如果之前匹配，但是碰上了一个不匹配的字符的时候，就必须去掉一个字符！如果有n个不匹配就去掉n个了，很好理解！
             (setf pos 0)))) ; 既然碰到了不匹配的字符，就必须从old的开始索引0重新开始新一轮的匹配
    (buf-flush buf out)))
;; > (file-subst "baro" "baric" "a.txt" "b.txt")

;; test file-subst
(defun file-subst-test ()
  (let ((origin-filename (make-pathname :name "origin.txt"))
        (output-filename (make-pathname :name "output.txt")))
    (with-open-file (origin origin-filename :direction :output
                            :if-exists :supersede)
      (dotimes (i 100000)
        (princ "barbarous" origin)
        (fresh-line origin)))
    (file-subst "baro" "baric" origin-filename output-filename)))
    

;; Exercises
;; ex1
(defun file-to-lststr(file)
  (let ((acc nil))
    (with-open-file (str file :direction :input)
      (do ((line (read-line str nil 'eof)
                 (read-line str nil 'eof)))
          ((eql line 'eof))
        (push line acc)))
    (reverse acc)))
;; ex2
(defun file-to-lstexp(file)
  (let ((acc nil))
    (with-open-file (str file :direction :input)
      (do ((exp (read str nil 'eof)
                (read str nil 'eof)))
          ((eql exp 'eof))
        (push exp acc)))
    (reverse acc)))
;; ex3
(defun appendwithoutnotation(file1 file2)
  (with-open-file (in file2 :direction :input)
    (with-open-file (out file1 :direction :output
                         :if-exists :append)
      (do ((line (read-line in nil :eof)
                 (read-line in nil :eof)))
          ((eql line :eof))
        (terpri out)
        (princ (subseq line 0 (position #\% line)) out)))))
;; ex4
(defun print-float-array (farr)
  (let ((dimens (array-dimensions farr)))
    (dotimes (i (first dimens))
      (dotimes (j (second dimens))
        (format t "~10,2,0,,' F" (aref farr i j)))
      (terpri))))
;; ex5 wildcards "+"   reference the source file input-and-ouput-ex5.lisp
;; ex6 reference the source file input-and-output-ex6.lisp

