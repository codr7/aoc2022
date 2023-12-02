(load (merge-pathnames "utils.lisp" *load-truename*))

(define-symbol-macro cols 40)

(defvar cycle 1)
(defvar x 1)
(defvar signal-strength 0)

(defun reset-cpu ()
  (setf cycle 1 x 1 signal-strength 0))

(defun sprite-visible? ()
  (member (mod (1- cycle) cols) (list x (1- x) (1+ x))))

(defun next-cycle ()
  (princ (if (sprite-visible?) "#" "."))
  (incf cycle)
  (when (zerop (mod (1- cycle) cols))
    (terpri)))

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
    (eval-op op)))
