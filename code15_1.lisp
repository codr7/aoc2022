(load (merge-pathnames "utils.lisp" *load-truename*))

(defvar min-x nil)
(defvar max-x nil)

(defun distance (p1 p2)
  (+ (abs (- (first p1) (first p2)))
     (abs (- (rest p1) (rest p2)))))

(defun update-min (x)
  (when (or (null min-x) (< x min-x))
    (setf min-x x)))

(defun update-max (x)
  (when (or (null max-x) (> x max-x))
    (setf max-x x)))

(defun extract-pos (in)
  (let ((x (parse-integer in :start (+ (search "x=" in) 2) :junk-allowed t))
	(y (parse-integer in :start (+ (search "y=" in) 2) :junk-allowed t)))
    (cons x y)))

(defun decode-line (in)
  (let* ((parts (split-sep in #\:))
	 (sensor (extract-pos (first parts)))
	 (beacon (extract-pos (second parts)))
	 (d (distance sensor beacon)))
    (update-min (- (first sensor) d))
    (update-max (+ (first sensor) d))
    (list sensor beacon d)))

(defun equal-pos? (l r)
  (and (= (first l) (first r))
       (= (rest l) (rest r))))

(defun impossible? (sensors p)
  (dolist (s sensors)
    (when (equal-pos? p (second s))
      (return-from impossible?)))
  
  (dolist (s sensors)
    (when (<= (distance (first s) p) (third s))
      (return-from impossible? t)))
  
  nil)

(defun count-positions (path)
  (let ((sensors (mapcar #'decode-line (read-lines path)))
	(result 0))
    (dotimes (dx (1+ (abs (- max-x min-x))))
      (when (impossible? sensors (cons (+ min-x dx) 2000000))
	(incf result)))
    result))
