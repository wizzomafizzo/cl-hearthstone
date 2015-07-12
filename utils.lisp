;;;; utils.lisp

(defun str (&rest strings)
  (apply #'concatenate 'string strings))

(defun slurp (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defmacro alias (new-name prev-name)
  `(defmacro ,new-name (&rest args)
     `(,',prev-name ,@args)))

(defun now ()
  (get-universal-time))

(defun lk (item alist)
  (cdr (assoc item alist)))

(defun round-to (number precision &optional (what #'round))
    (let ((div (expt 10 precision)))
         (/ (funcall what (* number div)) div)))

(defun start-of-today ()
  (let ((date (subseq (multiple-value-list (get-decoded-time)) 3 6)))
	(apply #'encode-universal-time
		   (concatenate 'list '(0 0 0) date))))

(defun a-week-ago ()
  (- (now) (* 60 60 24 7)))

(defun start-of-month ()
  (let ((date (subseq (multiple-value-list (get-decoded-time)) 0 6)))
	(setf (nth 3 date) 1)
	(apply #'encode-universal-time date)))

(defun human-date (x)
  (let ((date (multiple-value-list (decode-universal-time x))))
	(format nil "~d/~d" (nth 3 date) (nth 4 date))))

(defun human-time (x)
  (let ((date (multiple-value-list (decode-universal-time x))))
	(format nil "~2,'0d:~2,'0d:~2,'0d"
			(nth 2 date) (nth 1 date) (nth 0 date))))

(defun winrate (wins total)
  (if (not (and (= wins 0) (= total 0)))
	  (float (round-to (/ (* wins 100) total) 1))
	  0))
