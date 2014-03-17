 (defun one-of (choices &optional (prompt "Choice"))
   (let ((n (length choices)) (i))
     (do ((c choices (cdr c)) (i 1 (+ i 1)))
         ((null c))
       (format t "~&[~D] ~A~%" i (car c)))
     (do () ((typep i `(integer 1 ,n)))
       (format t "~&~A: " prompt)
       (setq i (read))
       (fresh-line))
     (nth (- i 1) choices)))

 (defun my-debugger (condition me-or-my-encapsulation)
   (format t "~&Fooey: ~A" condition)
   (let ((restart (one-of (compute-restarts))))
     (if (not restart) (error "My debugger got an error."))
     (let ((*debugger-hook* me-or-my-encapsulation))
       (invoke-restart-interactively restart))))
 
 (let ((*debugger-hook* #'my-debugger))
   (+ 3 'a))