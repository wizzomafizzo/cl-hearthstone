;;;; hearthstone.lisp

(load "~/quicklisp/setup.lisp")
(ql:quickload "sqlite")
(ql:quickload "hunchentoot")
(ql:quickload "html-template")
(ql:quickload "jsown")

(defpackage :hearthstone
  (:use :cl))

(in-package :hearthstone)

(defvar *config*
  '((db . "hearthstone.db")
	(username . "user")
	(password . "password")
	(port . 8888)
	(match-limit . 100)
	(winrate-tiers . (50 60))
	(heroes . ("Druid"
			   "Hunter"
			   "Mage"
			   "Paladin"
			   "Priest"
			   "Rogue"
               "Shaman"
               "Warlock"
			   "Warrior"))))

(defun str (&rest strings)
  (apply #'concatenate 'string strings))

(defun slurp (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

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

(defun winrate (wins total)
  (if (not (and (= wins 0) (= total 0)))
	  (float (round-to (/ (* wins 100) total) 1))
	  0))

;;; database

(defvar *db* (sqlite:connect (lk 'db *config*)))

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
	(db-single q (now) hero deck against notes outcome)))

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

;;; web

(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password) (hunchentoot:authorization)
     (cond ((and (string= username (lk 'username *config*))
				 (string= password (lk 'password *config*)))
            ,@body)
           (t (hunchentoot:require-authorization "Hearthstone")))))

(defun winrate->template (winrate)
  (cond ((< winrate (car (lk 'winrate-tiers *config*))) "label-danger")
		((< winrate (cadr (lk 'winrate-tiers *config*))) "label-warning")
		(t "label-success")))

(defun all-decks->template ()
  (let ((decks (all-decks)))
	(map 'list
		 #'(lambda (x)
			 (list :name (car x)
				   :wins (lk 'wins (cdr x))
				   :losses (lk 'losses (cdr x))
				   :total (lk 'total (cdr x))
				   :class (winrate->template (lk 'winrate (cdr x)))
				   :winrate (lk 'winrate (cdr x))))
		 decks)))

(defun all-matches->template (&optional deck)
  (let ((matches (all-matches deck)))
	(map 'list
		 #'(lambda (x)
			 (list :date (human-date (lk 'date x))
				   :hero (lk 'hero x)
				   :deck (lk 'deck x)
				   :against (lk 'against x)
				   :notes (lk 'notes x)
				   :are-notes (not (equal (lk 'notes x) ""))
				   :outcome (lk 'outcome x)))
		 matches)))

(defun all-match-stats->template (&optional deck)
  (let ((stats (all-match-stats deck)))
	(map 'list
		 #'(lambda (x)
			 (list :name (cond ((eq (car x) 'today) "Today")
							   ((eq (car x) 'week) "Week")
							   ((eq (car x) 'season) "Season")
							   ((eq (car x) 'overall) "Overall"))
				   :wins (lk 'wins (cdr x))
				   :losses (lk 'losses (cdr x))
				   :total (lk 'total (cdr x))
				   :class (winrate->template (lk 'winrate (cdr x)))
				   :winrate (lk 'winrate (cdr x))))
		 stats)))

(defun daily-match-stats->template (&optional deck)
  (let ((stats (daily-match-stats-week deck)))
	(list (cons 'labels
				(format nil "~{~A~^,~}"
						(map 'list #'(lambda (x) (human-date (lk 'date x)))
							 stats)))
		  (cons 'wins
				(jsown:to-json (map 'list #'(lambda (x) (lk 'wins x))
									stats)))
		  (cons 'losses
				(jsown:to-json (map 'list #'(lambda (x) (lk 'losses x))
									stats)))
		  (cons 'winrate
				(jsown:to-json (map 'list #'(lambda (x) (lk 'winrate x))
									stats))))))

(defun against-stats->template (&optional deck)
  (let ((stats (against-stats-week deck)))
	(list (cons 'labels
				(format nil "~{~A~^,~}"
						(map 'list #'(lambda (x) (car x))
							 stats)))
		  (cons 'wins
				(jsown:to-json (map 'list #'(lambda (x) (lk 'wins (cdr x)))
									stats)))
		  (cons 'losses
				(jsown:to-json (map 'list #'(lambda (x) (lk 'losses (cdr x)))
									stats)))
		  (cons 'winrate
				(jsown:to-json (map 'list #'(lambda (x) (lk 'winrate (cdr x)))
									stats))))))

(defun hero-select (selected)
  (map 'list #'(lambda (x)
				 (list :hero x
					   :selected (if (equal selected x) "selected")))
	   (lk 'heroes *config*)))

(defun index-page ()
  (hunchentoot:no-cache)
  (with-http-authentication
	  (if (and (eq (hunchentoot:request-method hunchentoot:*request*) :POST)
			   (hunchentoot:post-parameter "hero")
			   (not (equal "" (hunchentoot:post-parameter "deck")))
			   (hunchentoot:post-parameter "against")
			   (hunchentoot:post-parameter "outcome"))
		  (add-match (hunchentoot:post-parameter "hero")
					 (hunchentoot:post-parameter "deck")
					 (hunchentoot:post-parameter "against")
					 (hunchentoot:post-parameter "notes")
					 (if (equal (hunchentoot:post-parameter "outcome")
								"win")
						 t)))
	(let* ((get-deck (hunchentoot:get-parameter "deck"))
		   (post-deck (hunchentoot:post-parameter "deck"))
		   (hero (hunchentoot:post-parameter "hero"))
		   (daily-graph (daily-match-stats->template get-deck))
		   (against-graph (against-stats->template get-deck))
		   (vals (list :heroes (hero-select hero)
					   :last-hero hero
					   :last-deck (or post-deck get-deck)
					   :filter-deck get-deck
					   :all-match-stats (all-match-stats->template get-deck)
					   :all-decks (all-decks->template)
					   :all-matches (all-matches->template get-deck)
					   :daily-graph-labels (lk 'labels daily-graph)
					   :daily-graph-wins (lk 'wins daily-graph)
					   :daily-graph-losses (lk 'losses daily-graph)
					   :daily-graph-winrate (lk 'winrate daily-graph)
					   :against-graph-labels (lk 'labels against-graph)
					   :against-graph-wins (lk 'wins against-graph)
					   :against-graph-losses (lk 'losses against-graph)
					   :against-graph-winrate (lk 'winrate against-graph))))
	  (with-output-to-string (html-template:*default-template-output*)
		(html-template:fill-and-print-template #p"./templates/hearthstone.html"
											   vals)))))

(defun start-server ()
  (html-template:clear-template-cache)
  (setq hunchentoot:*dispatch-table*
		(list (hunchentoot:create-folder-dispatcher-and-handler
			   "/static/" #p"./static/")
			  (hunchentoot:create-regex-dispatcher
			   "^/$" 'index-page)))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
									:port (lk 'port *config*))))

(defvar *web* (start-server))
