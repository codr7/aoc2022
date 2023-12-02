(load (merge-pathnames "utils.lisp" *load-truename*))

(defun encode-char (in)
  (ecase in
    (0 "0")
    (1 "1")
    (2 "2")
    (-1 "-")
    (-2 "=")))
  
(defun encode (in)
  (let ((max-w 1) (out (make-array 0 :fill-pointer 0)))
    (do-while (< max-w (* in 5))
      (setf max-w (* max-w 5)))
    
    (let ((w max-w))
      (tagbody
       loop
	 (let ((v (floor in w)))
	   (vector-push-extend v out)
	   (decf in (* v w)))
	 (when (> w 1)
	   (setf w (/ w 5))
	   (go loop))))

    (let ((r 0))
      ;; Increment
      (let ((w 1))
	(dotimes (i (length out))
	  (let ((j (- (length out) i 1)))
	    (do-while (> (aref out j) 2)
	      (incf (aref out (1- j)))
	      (incf r (* w 5))
	      (decf (aref out j))
	      (decf r w))
	    (setf w (* w 5)))))

      ;; Decrement
      (let ((w max-w))
	(dotimes (i (length out))
	  (multiple-value-bind (n nr) (floor r w)
	    (when (not (zerop n))
	      (decf (aref out i) n)
	      (setf r nr)))
	  (setf w (/ w 5))))

      (assert (zerop r)))
      
    (with-output-to-string (s)
      (let ((i 0))
	(do-while (zerop (aref out i))
	  (incf i))
	(dotimes (j (- (length out) i))
	  (write-string (encode-char (aref out (+ i j))) s))))))

(defun decode-char (in)
  (ecase in
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\- -1)
    (#\= -2)))

(defun decode (in)
  (let ((in (reverse in))
	(out 0)
	(w 1))
    (dotimes (i (length in))
      (incf out (* (decode-char (char in i)) w))
      (setf w (* w 5)))
    out))

(defun tests ()
  (assert (= 314159265 (decode "1121-1110-1=0")))
  (assert (string= "1121-1110-1=0" (encode 314159265))))

(defun calculate-fuel (path)
  (encode (reduce #'+ (mapcar #'decode (read-lines path)))))
