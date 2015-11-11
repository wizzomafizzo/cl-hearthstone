;;;; database.lisp

(defmacro db-non-query (&rest body)
  `(sqlite:execute-non-query *db* ,@body))

(defmacro db-single (&rest body)
  `(sqlite:execute-single *db* ,@body))

(defmacro db-to-list (&rest body)
  `(sqlite:execute-to-list *db* ,@body))

(defun create-db ()
  (db-non-query (str "create table if not exists matches "
					 "(date integer, type text, "
					 "deck text, against text, "
					 "notes text, outcome integer)")))

(defun export-matches ()
  (db-to-list "select rowid,* from matches"))

(defun add-match (mode deck against notes outcome)
  (let ((q "insert into matches values (?, ?, ?, ?, ?, ?)")
		(outcome (if outcome 1 0)))
	(db-single q (now) mode deck against notes outcome)
	(sqlite:last-insert-rowid *db*)))

(defun remove-match (id)
  (let ((q "delete from matches where rowid = ?"))
	(db-non-query q id)))

(defun read-match (result)
  (let ((outcome (if (= (nth 6 result) 1) t nil)))
	(setf (nth 6 result) outcome)
	(list (cons 'id (nth 0 result))
		  (cons 'date (nth 1 result))
		  (cons 'type (nth 2 result))
		  (cons 'deck (nth 3 result))
		  (cons 'against (nth 4 result))
		  (cons 'notes (nth 5 result))
		  (cons 'outcome (nth 6 result)))))

(defun get-match (id)
  (let ((q "select rowid,* from matches where rowid = ?"))
	(read-match (car (db-to-list q id)))))

(defun all-matches (&optional deck limit)
  (let* ((limit (if limit limit -1))
		 (results (if deck
					  (db-to-list (str "select rowid,* from matches where"
									   " deck = ? order by date desc limit ?")
								  deck limit)
					  (db-to-list (str "select rowid,* from matches"
									   " order by date desc limit ?")
								  limit))))
	(map 'list #'read-match results)))

(defun count-filter-matches (from to)
  (let ((from-time (if (or (not from) (equal from "")) 0
					   (- (net.telent.date:parse-time from) 1)))
		(to-time (if (or (not to) (equal to "")) (end-of-today)
					 (+ +one-day+ (net.telent.date:parse-time to) 1))))
	(db-single "select count(*) from matches where date between ? and ?"
			   from-time to-time)))

(defun filter-matches (from to type deck against notes outcome)
  (let ((from-time (if (or (not from) (equal from "")) 0
					   (- (net.telent.date:parse-time from) 1)))
		(to-time (if (or (not to) (equal to "")) (end-of-today)
					 (+ +one-day+ (net.telent.date:parse-time to) 1)))
		(type-q (cond
				  ((or (not type) (equal type "") (equal type "All")) "%")
				  ((equal type "Ranked") "Rank %")
				  (t type)))
		(deck-q (if (or (not deck) (equal deck ""))
					"%" (str "%" deck "%")))
		(against-q (if (or (not against) (equal against "")
						   (equal against "All"))
					   "%" (str "%" against "%")))
		(notes-q (if (or (not notes) (equal notes ""))
					 "%" (str "%" notes "%")))
		(outcome-q (cond
					 ((equal outcome "win") "outcome = 1")
					 ((equal outcome "lose") "outcome = 0")
					 (t "(outcome = 0 or outcome = 1)"))))
	(map 'list #'read-match
		 (db-to-list (str "select rowid,* from matches where "
						  "date between ? and ? and "
						  "type like ? and deck like ? and "
						  "against like ? and notes like ? and "
						  outcome-q " order by date desc limit ?")
					 from-time to-time type-q deck-q
					 against-q notes-q (lk 'filter-limit *config*)))))

(defun matches-this-season ()
  (map 'list
	   #'read-match
	   (db-to-list (str "select rowid,* from matches"
						" where date between ? and ?"
						" and type != 'Casual'")
				   (start-of-month) (now))))

(defun stats-this-season ()
  (let ((all (length (db-to-list (str "select rowid,* from matches"
									  " where date between ? and ?"
									  " and type != 'Casual'")
								 (start-of-month) (now))))
		(wins (length (db-to-list (str "select rowid,* from matches"
									   " where date between ? and ?"
									   " and type != 'Casual'"
									   " and outcome = 1")
								  (start-of-month) (now)))))
	(list (cons 'total all)
		  (cons 'wins wins)
		  (cons 'losses (- all wins))
		  (cons 'winrate (winrate wins all)))))

(defun deck-names (&optional days)
  (let ((start (if days
				   (- (now) (days-to-secs days))
				   (- (now) (days-to-secs (lk 'deck-limit *config*))))))
	(map 'list #'(lambda (x) (car x))
		 (db-to-list (str "select distinct deck from matches"
						  " where date between ? and ?"
						  " order by deck desc")
					 start (now)))))

(defun all-decks ()
  (let* ((q "select * from matches where deck = ? and outcome = ? ")
		 (read-fn (lambda (x)
					(let* ((wins (length (db-to-list q x 1)))
						   (losses (length (db-to-list q x 0)))
						   (total (+ wins losses))
						   (winrate (winrate wins total)))
					  (cons x (list (cons 'wins wins)
									(cons 'losses losses)
									(cons 'total total)
									(cons 'winrate winrate)))))))
	(sort (map 'list read-fn (deck-names))
		  #'string-lessp :key #'car)))

(defun rename-deck (from to)
  (db-non-query "update matches set deck = ? where deck = ?"
				to from))

(defun remove-deck (name)
  (db-non-query "delete from matches where deck = ?" name))

(defun count-deck (name)
  (db-single "select count(*) from matches where deck = ?" name))

(defun match-stats-range (from to &optional deck)
  (let ((qs (if deck
				(list (str "select * from matches where"
						   " date between ? and ? and outcome = 1"
						   " and deck = ?")
					  (str "select * from matches where"
						   " date between ? and ? and outcome = 0"
						   " and deck = ?"))
				(list (str "select * from matches where"
						   " date between ? and ? and outcome = 1")
					  (str "select * from matches where"
						   " date between ? and ? and outcome = 0")))))
	(let ((wins (length (if deck
							(db-to-list (car qs) from to deck)
							(db-to-list (car qs) from to))))
		  (losses (length (if deck
							  (db-to-list (cadr qs) from to deck)
							  (db-to-list (cadr qs) from to)))))
	  (list (cons 'wins wins)
			(cons 'losses losses)
			(cons 'total (+ wins losses))
			(cons 'winrate (winrate wins (+ wins losses)))))))

(defun all-match-stats (&optional deck)
  (let ((args (if deck (list (now) deck) (list (now)))))
	(list (cons 'today (apply #'match-stats-range (start-of-today) args))
		  (cons 'week (apply #'match-stats-range (a-week-ago) args))
		  (cons 'month (apply #'match-stats-range (a-month-ago) args))
		  (cons 'overall (apply #'match-stats-range 0 args)))))

(defun daily-match-stats (days-back &optional deck)
  (let ((from (start-of-today))
		(to (end-of-today))
		(day (* 60 60 24))
		(qs (if deck
				(list (str "select * from matches where"
						   " date between ? and ? and outcome = 1"
						   " and deck = ?")
					  (str "select * from matches where"
						   " date between ? and ? and outcome = 0"
						   " and deck = ?"))
				(list (str "select * from matches where"
						   " date between ? and ? and outcome = 1")
					  (str "select * from matches where"
						   " date between ? and ? and outcome = 0")))))
	(loop for x from 1 to days-back
	   collect (let* ((from (- from (* day (- x 1))))
					  (to (- to (* day (- x 1))))
					  (wins (length (if deck
										(db-to-list (car qs) from to deck)
										(db-to-list (car qs) from to))))
					  (losses (length (if deck
										  (db-to-list (cadr qs) from to deck)
										  (db-to-list (cadr qs) from to)))))
				 (list (cons 'date to)
					   (cons 'wins wins)
					   (cons 'losses losses)
					   (cons 'winrate (winrate wins (+ wins losses))))))))

(defun daily-match-stats-week (&optional deck)
  (reverse (daily-match-stats 7 deck)))

(defun daily-match-stats-fortnight (&optional deck)
  (reverse (daily-match-stats 14 deck)))

(defun against-stats-range (from to &optional deck)
  (let ((heroes (lk 'heroes *config*))
		(qs (if deck
				(list (str "select * from matches where"
						   " date between ? and ? and outcome = 1"
						   " and against = ? and deck = ?")
					  (str "select * from matches where"
						   " date between ? and ? and outcome = 0"
						   " and against = ? and deck = ?"))
				(list (str "select * from matches where"
						   " date between ? and ? and outcome = 1"
						   " and against = ?")
					  (str "select * from matches where"
						   " date between ? and ? and outcome = 0"
						   " and against = ?")))))
	(loop for x in heroes collect
		 (let ((wins (length (if deck
								 (db-to-list (car qs) from to x deck)
								 (db-to-list (car qs) from to x))))
			   (losses (length (if deck
								   (db-to-list (cadr qs) from to x deck)
								   (db-to-list (cadr qs) from to x)))))
		   (cons x (list (cons 'wins wins)
						 (cons 'losses losses)
						 (cons 'winrate (winrate wins (+ wins
														 losses)))))))))

(defun worst-against-range (from to &optional deck)
  (let ((stats (against-stats-range from to deck)))
	(sort (map 'list
			   #'(lambda (x)
				   (list (car x)
						 (cdr (assoc 'losses (cdr x)))
						 (cdr (assoc 'winrate (cdr x)))))
			   stats)
		  #'< :key #'caddr)))

(defun seen-against-range (from to &optional deck)
  (let ((stats (against-stats-range from to deck)))
	(sort (map 'list
			   #'(lambda (x)
				   (cons (car x)
						 (+ (cdr (assoc 'wins (cdr x)))
							(cdr (assoc 'losses (cdr x))))))
			   stats)
		  #'> :key #'cdr)))

(defun worst-against-week (&optional deck)
  (worst-against-range (a-week-ago) (now) deck))

(defun worst-against-month (&optional deck)
  (worst-against-range (a-month-ago) (now) deck))

(defun seen-against-week (&optional deck)
  (seen-against-range (a-week-ago) (now) deck))

(defun seen-against-month (&optional deck)
  (seen-against-range (a-month-ago) (now) deck))

(defun against-stats-week (&optional deck)
  (against-stats-range (a-week-ago) (now) deck))

(defun against-stats-month (&optional deck)
  (against-stats-range (a-month-ago) (now) deck))

(defun against-stats-season (&optional deck)
  (against-stats-range (start-of-month) (now) deck))

(defun current-rank ()
  (let ((rank (first (loop for y in (map 'list
										 #'(lambda (x) (cdr (assoc 'type x)))
										 (reverse (matches-this-season)))
						if (not (equal y "Casual"))
						collect y))))
	(if rank rank "Rank 25")))

(defun average-match-length (matches)
  (let* ((match-limit (* 60 15))
		 (matches (reverse matches))
		 (last-date (cdr (assoc 'date (first matches))))
		 (current-session nil)
		 (sessions nil)
		 (match-times nil))
	(loop for x in matches
	   do (let ((delta (- (cdr (assoc 'date x)) last-date)))
			(if (<= delta match-limit)
				(progn
				  (if (> delta 0)
					  (setf match-times (cons delta match-times)))
				  (setf current-session (cons x current-session))
				  (setf last-date (cdr (assoc 'date x))))
				(progn
				  (setf sessions (cons current-session sessions))
				  (setf current-session x)
				  (setf last-date (cdr (assoc 'date x)))))))
	(if match-times
		(/ (apply #'+ match-times)
		   (length match-times))
		0)))

(defun backup-db ()
  (let ((filename (str (lk 'db *config*) "-"
					   (princ-to-string (get-unix-time)))))
	(my-copy-file (lk 'db *config*) filename)))

(defun import-json (filename)
  (let* ((json (slurp filename))
		 (data (jsown:parse json))
		 (q "insert into matches values (?, ?, ?, ?, ?, ?)"))
	(loop for x in data
	   do (db-non-query q (nth 1 x) (nth 2 x) (nth 3 x)
						(nth 4 x) (nth 5 x) (nth 6 x)))))
