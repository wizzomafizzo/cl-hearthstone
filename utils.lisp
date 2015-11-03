;;;; utils.lisp

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

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

(defun flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

(defun round-to (number precision &optional (what #'round))
    (let ((div (expt 10 precision)))
         (/ (funcall what (* number div)) div)))

(defun start-of-today ()
  (let ((date (subseq (multiple-value-list (get-decoded-time)) 3 6)))
	(apply #'encode-universal-time
		   (concatenate 'list '(0 0 0) date))))

(defun end-of-today ()
  (let ((date (subseq (multiple-value-list (get-decoded-time)) 3 6)))
	(apply #'encode-universal-time
		   (concatenate 'list '(59 59 23) date))))

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

(defun my-copy-file (from-file to-file)
  (with-open-file (input-stream from-file
								:direction :input
								:element-type '(unsigned-byte 8))
    (with-open-file (output-stream to-file
								   :direction :output
								   :if-exists :supersede
								   :if-does-not-exist :create
								   :element-type '(unsigned-byte 8))
      (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
		(loop for pos = (read-sequence buf input-stream)
		   while (plusp pos)
		   do (write-sequence buf output-stream :end pos))))))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun trim (s)
  (string-trim " " s))
