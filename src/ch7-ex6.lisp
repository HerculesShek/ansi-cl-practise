;;;; source for the exercise 6 in chapter 7

;; String Substitution -- an execllent example!
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

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                         :if-exists :supersede)
      (stream-subst old new in out))))
;; here are 3 wild cards :
;;    %a -- all char
;;    %d -- 0-9
;;    %w -- a-zA-Z and 0-9
;;    %% -- #\%
(defun parse-pattern (patt)
  (labels ((rec (i len ctrl acc)
             (if (< i len)
                 (let* ((c (char patt i))
                        (ctrl-next (and (not ctrl) (char= c #\%))))
                   (rec (1+ i)
                        len
                        ctrl-next
                        (if ctrl-next
                            acc
                            (cons
                             (if ctrl
                                 (case c
                                   (#\a 'all)
                                   (#\w 'word)
                                   (#\d 'digit)
                                   (#\% #\%))
                                 c)
                             acc))))
                 (concatenate 'vector (nreverse acc)))))
    (rec 0 (length patt) nil nil)))

(defun stream-subst (patt new in out)
  (let* ((pos 0)
         (old (parse-pattern patt))
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))  
                (read-char in nil :eof))))
        ((eql c :eof))
      (let ((c0 (svref old pos)))
        (cond ((or 
                (eql c0 'all)
                (and (eql c0 'word) (or (alpha-char-p c) (digit-char-p c)))
                (and (eql c0 'digit) (digit-char-p c))
                (char= c c0))
               (incf pos)
               (cond ((= pos len)            ; 3 
                      (princ new out)
                      (setf pos 0)
                      (buf-clear buf))
                     ((not from-buf)         ; 2 
                      (buf-insert c buf))))
              ((zerop pos)                   ; 1 
               (princ c out)
               (when from-buf               
                 (buf-pop buf)))
              (t                             ; 4 
               (unless from-buf             
                 (buf-insert c buf))        
               (princ (buf-pop buf) out)    
               (setf pos 0)))))
    (buf-flush buf out)))

