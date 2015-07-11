;;;; web.lisp

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
				   :time (human-time (lk 'date x))
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
