(load (merge-pathnames "utils.lisp" *load-truename*))

(defvar cycle 1)
(defvar x 1)
(defvar signal-strength 0)

(defun reset-cpu ()
  (setf cycle 1 x 1 signal-strength 0))

(defun next-cycle ()
  (when (member cycle '(20 60 100 140 180 220))
    (format t "cycle ~a x ~a~%" cycle x)
    (incf signal-strength (* cycle x)))
  (incf cycle))

(defun eval-op (op)
  (ecase (first op)
    (:addx
     (next-cycle)
     (next-cycle)
     (incf x (second op)))
    (:noop
     (next-cycle))))

(defun decode-op (line)
  (let* ((ss (split-sep line #\space))
	 (code (kw (pop ss))))
    (ecase code
      (:noop '(:noop))
      (:addx `(:addx ,(parse-integer (pop ss)))))))

(defun eval-file (path)
  (reset-cpu)
  (dolist (op (mapcar #'decode-op (read-lines path)))
    (eval-op op))
  signal-strength)
   
