;;;; database.lisp

(defmacro db-non-query (&rest body)
  `(sqlite:execute-non-query *db* ,@body))

(defmacro db-single (&rest body)
  `(sqlite:execute-single *db* ,@body))

(defmacro db-to-list (&rest body)
  `(sqlite:execute-to-list *db* ,@body))

(defun create-db ()
  (db-non-query (str "create table matches "
					 "(date integer, hero text, "
					 "deck text, against text, "
					 "notes text, outcome integer)")))

(defun add-match (hero deck against notes outcome)
  (let ((q "insert into matches values (?, ?, ?, ?, ?, ?)")
		(outcome (if outcome 1 0)))
	(db-single q (now) hero deck against notes outcome)
	(sqlite:last-insert-rowid *db*)))

(defun remove-match (id)
  (let ((q "delete from matches where rowid = ?"))
	(db-non-query q id)))

(defun read-match (result)
  (let ((outcome (if (= (nth 6 result) 1) t nil)))
	(setf (nth 6 result) outcome)
	(list (cons 'id (nth 0 result))
		  (cons 'date (nth 1 result))
		  (cons 'hero (nth 2 result))
		  (cons 'deck (nth 3 result))
		  (cons 'against (nth 4 result))
		  (cons 'notes (nth 5 result))
		  (cons 'outcome (nth 6 result)))))

(defun all-matches (&optional deck)
  (let ((results (if deck
					 (db-to-list (str "select rowid,* from matches where"
									  " deck = ? order by date desc limit ?")
								 deck (lk 'match-limit *config*))
					 (db-to-list (str "select rowid,* from matches"
									  " order by date desc limit ?")
								 (lk 'match-limit *config*)))))
	(map 'list #'read-match results)))

(defun deck-names ()
  (map 'list #'(lambda (x) (car x))
	   (db-to-list (str "select distinct deck from"
						" matches order by deck desc"))))

(defun all-decks ()
  (map 'list
	   #'(lambda (x)
		   (let ((wins (length (db-to-list (str "select * from matches where"
												" deck = ? and outcome = 1")
										   x)))
				 (losses (length (db-to-list (str "select * from matches where"
												  " deck = ? and outcome = 0")
									  x))))
			 (cons x (list (cons 'wins wins)
						   (cons 'losses losses)
						   (cons 'total (+ wins losses))
						   (cons 'winrate (winrate wins (+ wins losses)))))))
	   (deck-names)))

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
		  (cons 'season (apply #'match-stats-range (start-of-month) args))
		  (cons 'overall (apply #'match-stats-range 0 args)))))

(defun daily-match-stats (days-back &optional deck)
  (let ((date (now))
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
	   collect (let* ((to (- date (* day (- x 1))))
					  (from (- date (* day x)))
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

(defun against-stats-week (&optional deck)
  (against-stats-range (- (now) (* 60 60 24 7)) (now) deck))
