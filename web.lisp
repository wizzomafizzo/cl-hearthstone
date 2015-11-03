;;;; web.lisp

(alias gp hunchentoot:get-parameter)
(alias pp hunchentoot:post-parameter)

(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password)
	   (hunchentoot:authorization)
     (cond ((and (string= username (car *auth*))
				 (string= password (cdr *auth*)))
            ,@body)
           (t (hunchentoot:require-authorization "Hearthlisp")))))

(defun winrate->template (winrate)
  (cond ((< winrate (car (lk 'winrate-tiers *config*))) "label-danger")
		((< winrate (cadr (lk 'winrate-tiers *config*))) "label-warning")
		(t "label-success")))

(defun all-decks->template ()
  (let ((decks (all-decks)))
	(map 'list
		 #'(lambda (x)
			 (list :name (car x)
				   :encoded-name (do-urlencode:urlencode (car x))
				   :escaped-name (html-template:escape-string (car x))
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
				   :type (lk 'type x)
				   :deck (lk 'deck x)
				   :encoded-deck (do-urlencode:urlencode (lk 'deck x))
				   :escaped-deck (html-template:escape-string (lk 'deck x))
				   :against (lk 'against x)
				   :notes (lk 'notes x)
				   :are-notes (not (equal (lk 'notes x) ""))
				   :outcome (lk 'outcome x)
				   :id (lk 'id x)))
		 matches)))

(defun all-match-stats->template (&optional deck)
  (let ((stats (all-match-stats deck)))
	(map 'list
		 #'(lambda (x)
			 (list :name (cond ((eq (car x) 'today) "Today")
							   ((eq (car x) 'week) "Week")
							   ((eq (car x) 'season) "Month")
							   ((eq (car x) 'overall) "Overall"))
				   :wins (lk 'wins (cdr x))
				   :losses (lk 'losses (cdr x))
				   :total (lk 'total (cdr x))
				   :class (winrate->template (lk 'winrate (cdr x)))
				   :winrate (lk 'winrate (cdr x))))
		 stats)))

(defun daily-match-stats->template (&optional deck)
  (let ((stats (daily-match-stats-week deck)))
	(list (cons 'labels (format nil "~{~A~^,~}"
								(map 'list
									 #'(lambda (x) (human-date (lk 'date x)))
									 stats)))
		  (cons 'wins (jsown:to-json (map 'list
										  #'(lambda (x) (lk 'wins x))
										  stats)))
		  (cons 'losses (jsown:to-json (map 'list
											#'(lambda (x) (lk 'losses x))
											stats)))
		  (cons 'winrate (jsown:to-json (map 'list
											 #'(lambda (x) (lk 'winrate x))
											 stats))))))

(defun against-stats->template (&optional deck)
  (let ((stats (against-stats-season deck)))
	(list (cons 'labels
				(format nil "~{~A~^,~}"
						(map 'list #'(lambda (x) (car x))stats)))
		  (cons 'wins
				(jsown:to-json
				 (map 'list #'(lambda (x) (lk 'wins (cdr x))) stats)))
		  (cons 'losses
				(jsown:to-json
				 (map 'list #'(lambda (x) (lk 'losses (cdr x))) stats)))
		  (cons 'winrate
				(jsown:to-json
				 (map 'list #'(lambda (x) (lk 'winrate (cdr x))) stats))))))

(defun worst-against->template (&optional deck)
  (let ((stats (worst-against-week deck)))
	(map 'list
		 #'(lambda (x)
			 (list :hero (car x)
				   :losses (cdr x)))
		 (subseq stats 0 5))))

(defun seen-against->template (&optional deck)
  (let ((stats (seen-against-week deck)))
	(map 'list
		 #'(lambda (x)
			 (list :hero (car x)
				   :total (cdr x)))
		 (subseq stats 0 5))))

(defun type-select (selected)
  (map 'list
	   #'(lambda (x)
		   (list :type x
				 :selected (if (equal selected x)
							   "selected")))
	   *game-types*))

(defun hero-select (selected)
  (map 'list
	   #'(lambda (x)
		   (list :hero x
				 :selected (if (equal selected x)
							   "selected")))
	   (lk 'heroes *config*)))

(defun games-until-legend ()
  (let* ((stats (stats-this-season)))
	(- (cdar (last (loop for x in (reverse (lk 'winrate-legend *config*))
					  while (>= (lk 'winrate stats) (car x))
					  collect x)))
	   (lk 'total stats))))

(defun undo-submit ()
  (with-http-authentication
	  (let* ((match (get-match *last-added*))
			 (deck (lk 'deck match))
			 (url-name (do-urlencode:urlencode deck))
			 (vals (list :match-id *last-added*
						 :match-deck deck
						 :url-name url-name)))
		(remove-match *last-added*)
		(with-output-to-string (html-template:*default-template-output*)
		  (html-template:fill-and-print-template #p"templates/undo.html"
												 vals)))))

(defun download-matches ()
  (with-http-authentication (hunchentoot:no-cache)
	(setf (hunchentoot:content-type*) "application/json")
	(jsown:to-json (export-matches))))

(defun average-length ()
  (let ((len (average-match-length (reverse (matches-this-season)))))
	(float (round-to (/ len 60) 1))))

(defun time-to-legend ()
  (float (round-to (/ (* (games-until-legend)
						 (average-length))
					  60)
				   1)))

(defun index-page ()
  (with-http-authentication (hunchentoot:no-cache)
	(setq *last-added* nil)
	(if (and (eq (hunchentoot:request-method hunchentoot:*request*) :POST)
			 (pp "type")
			 (not (equal "" (pp "deck")))
			 (pp "against")
			 (pp "outcome"))
		(progn
		  (setq *last-type* (pp "type"))
		  (setq *last-added*
				(add-match (pp "type")
						   (trim (pp "deck"))
						   (pp "against")
						   (trim (pp "notes"))
						   (if (equal (pp "outcome") "win") t)))))
	(let* ((get-deck (if (gp "deck") (trim (gp "deck"))))
		   (post-deck (if (pp "deck") (trim (pp "deck"))))
		   (active-deck (or post-deck get-deck))
		   (type *last-type*)
		   (daily-graph (daily-match-stats->template active-deck))
		   (against-graph (against-stats->template active-deck))
		   (season (stats-this-season))
		   (vals (list :heroes (hero-select nil)
					   :last-deck active-deck
					   :last-added *last-added*
					   :filter-deck (html-template:escape-string active-deck)
					   :encoded-deck (do-urlencode:urlencode active-deck)
					   :all-match-stats (all-match-stats->template active-deck)
					   :all-decks (all-decks->template)
					   :all-matches (all-matches->template active-deck)
					   :daily-graph-labels (lk 'labels daily-graph)
					   :daily-graph-wins (lk 'wins daily-graph)
					   :daily-graph-losses (lk 'losses daily-graph)
					   :daily-graph-winrate (lk 'winrate daily-graph)
					   :against-graph-labels (lk 'labels against-graph)
					   :against-graph-wins (lk 'wins against-graph)
					   :against-graph-losses (lk 'losses against-graph)
					   :against-graph-winrate (lk 'winrate against-graph)
					   :seen-against (seen-against->template active-deck)
					   :worst-against (worst-against->template active-deck)
					   :game-types (type-select type)
					   :current-rank (current-rank)
					   :average-length (average-length)
					   :season-wins (lk 'wins season)
					   :season-losses (lk 'losses season)
					   :season-total (lk 'total season)
					   :season-winrate (lk 'winrate season)
					   :season-class (winrate->template (lk 'winrate season))
					   :games-until-legend (games-until-legend)
					   :time-to-legend (time-to-legend)
					   :not-in-legend (not (equal (current-rank) "Legend")))))
	  (with-output-to-string (html-template:*default-template-output*)
		(html-template:fill-and-print-template #p"templates/hearthstone.html"
											   vals)))))

(defun start-server ()
  (html-template:clear-template-cache)
  (setq hunchentoot:*dispatch-table*
		(list (hunchentoot:create-folder-dispatcher-and-handler
			   (str (lk 'url-prefix *config*) "/static/") #p"static/")
			  (hunchentoot:create-regex-dispatcher
			   (str "^" (lk 'url-prefix *config*) "/export.json$")
			   'download-matches)
			  (hunchentoot:create-regex-dispatcher
			   (str "^" (lk 'url-prefix *config*) "/undo$")
			   'undo-submit)
			  (hunchentoot:create-regex-dispatcher
			   (str "^" (lk 'url-prefix *config*) "/$")
			   'index-page)))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
									:port (lk 'port *config*))))
