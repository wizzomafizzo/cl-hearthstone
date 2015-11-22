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
				   :wins (lk 'wins (cdr x))
				   :losses (lk 'losses (cdr x))
				   :total (lk 'total (cdr x))
				   :class (winrate->template (lk 'winrate (cdr x)))
				   :winrate (lk 'winrate (cdr x))))
		 decks)))

(defun all-matches->template (&optional deck limit)
  (let ((matches (all-matches deck limit)))
	(map 'list
		 #'(lambda (x)
			 (list :date (human-date (lk 'date x))
				   :time (human-time (lk 'date x))
				   :type (lk 'type x)
				   :deck (lk 'deck x)
				   :encoded-deck (do-urlencode:urlencode (lk 'deck x))
				   :against (lk 'against x)
				   :notes (lk 'notes x)
				   :are-notes (not (equal (lk 'notes x) ""))
				   :outcome (lk 'outcome x)
				   :id (lk 'id x)))
		 matches)))

(defun filter-matches->template (from to type deck against notes outcome)
  (let ((matches (filter-matches from to type deck against notes outcome)))
	(map 'list
		 #'(lambda (x)
			 (list :date (human-date (lk 'date x))
				   :time (human-time (lk 'date x))
				   :type (lk 'type x)
				   :deck (lk 'deck x)
				   :against (lk 'against x)
				   :notes (lk 'notes x)
				   :outcome (lk 'outcome x)
				   :id (lk 'id x)))
		 matches)))

(defun all-match-stats->template (&optional deck)
  (let ((stats (all-match-stats deck)))
	(map 'list
		 #'(lambda (x)
			 (list :name (cond ((eq (car x) 'today) "Today")
							   ((eq (car x) 'week) "Week")
							   ((eq (car x) 'month) "Month")
							   ((eq (car x) 'overall) "Overall"))
				   :wins (lk 'wins (cdr x))
				   :losses (lk 'losses (cdr x))
				   :total (lk 'total (cdr x))
				   :class (winrate->template (lk 'winrate (cdr x)))
				   :winrate (lk 'winrate (cdr x))))
		 stats)))

(defun daily-match-stats->template (&optional deck)
  (let ((stats (daily-match-stats-fortnight deck)))
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
  (let ((stats (against-stats-month deck)))
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
  (let ((stats (worst-against-month deck)))
	(map 'list
		 #'(lambda (x)
			 (list :hero (car x)
				   :losses (cadr x)
				   :winrate (caddr x)))
		 (subseq stats 0 5))))

(defun seen-against->template (&optional deck)
  (let ((stats (seen-against-month deck)))
	(map 'list
		 #'(lambda (x)
			 (list :hero (car x)
				   :total (cdr x)))
		 (subseq stats 0 5))))

(defun seen-against-chart->template (&optional deck)
  (let ((stats (seen-against-month deck)))
	(map 'list
		 #'(lambda (x)
			 (list :hero (car x)
				   :total (cdr x)
				   :color (cond
							((equal (car x) "Druid") "#AD8C3D")
							((equal (car x) "Hunter") "#34A349")
							((equal (car x) "Mage") "#74C7E3")
							((equal (car x) "Paladin") "#E3D052")
							((equal (car x) "Priest") "#E3E3BF")
							((equal (car x) "Rogue") "#8C8C8C")
							((equal (car x) "Shaman") "#0508B5")
							((equal (car x) "Warlock") "#9A3DBF")
							((equal (car x) "Warrior") "#E31B29"))))
		 stats)))

(defun type-select (selected &optional filter)
  (let* ((default-types (copy-list *game-types*))
		 (types (if filter
				   (insert-after default-types 0 "Ranked")
				   default-types)))
	(map 'list
		 #'(lambda (x)
			 (list :type x
				   :selected (if (equal selected x)
								 "selected")))
		 types)))

(defun hero-select (selected)
  (map 'list
	   #'(lambda (x)
		   (list :hero x
				 :selected (if (equal selected x)
							   "selected")))
	   (lk 'heroes *config*)))

(defun outcome-select (selected)
  (let ((default (if (or (not selected) (equal selected ""))
					 "all" selected)))
	(map 'list
		 #'(lambda (x)
			 (list :name (car x)
				   :val (cadr x)
				   :selected (if (equal default (cadr x))
								 "checked")))
		 '(("All" "all") ("Wins" "win") ("Losses" "lose")))))

(defun games-until-legend ()
  (let* ((stats (stats-this-season)))
	(- (cdar (last (loop for x in (reverse (lk 'winrate-legend *config*))
					  while (>= (lk 'winrate stats) (car x))
					  collect x)))
	   (lk 'total stats))))

(defun average-length ()
  (let ((len (average-match-length (reverse (matches-this-season)))))
	(float (round-to (/ len 60) 1))))

(defun time-to-legend ()
  (float (round-to (/ (* (games-until-legend)
						 (average-length))
					  60)
				   1)))

(defun undo-submit ()
  (with-http-authentication
	  (let* ((match-id (car *last-added*))
			 (match (if match-id (get-match match-id)))
			 (deck (lk 'deck match))
			 (against (lk 'against match))
			 (notes (lk 'notes match))
			 (url-deck (do-urlencode:urlencode deck))
			 (url-against (do-urlencode:urlencode against))
			 (url-notes (do-urlencode:urlencode notes))
			 (vals (list :match-id match-id
						 :match-deck deck
						 :url-against (if against url-against "")
						 :url-notes (if notes url-notes "")
						 :url-deck (if deck url-deck ""))))
		(if match-id (remove-match match-id))
		(with-output-to-string (html-template:*default-template-output*)
		  (html-template:fill-and-print-template #p"templates/undo.html"
												 vals)))))

(defun download-matches ()
  (with-http-authentication (hunchentoot:no-cache)
	(setf (hunchentoot:content-type*) "application/json")
	(jsown:to-json (export-matches))))

(defun matches-page ()
  (with-http-authentication
	  (let* ((template #p"templates/filtermatches.html")
			 (from (gp "from"))
			 (to (gp "to"))
			 (type (gp "type"))
			 (deck (gp "deck"))
			 (against (gp "against"))
			 (notes (gp "notes"))
			 (outcome (gp "outcome"))
			 (total-in-range (count-filter-matches from to))
			 (matches (filter-matches->template from to type deck
												against notes outcome))
			 (vals (list :matches matches
						 :heroes (hero-select against)
						 :types (type-select type t)
						 :total-results (length matches)
						 :no-results (< (length matches) 1)
						 :percent-results (winrate (length matches)
												   total-in-range)
						 :total-in-range total-in-range
						 :not-hundred-percent (not (= total-in-range
													  (length matches)))
						 :from from
						 :to to
						 :deck deck
						 :notes notes
						 :outcomes (outcome-select outcome))))
		(with-output-to-string (html-template:*default-template-output*)
		  (html-template:fill-and-print-template template vals)))))

(defun stats-page ()
  (with-http-authentication
	  (let* ((template #p"templates/detailedstats.html")
			 (vals (list :matches nil)))
		(with-output-to-string (html-template:*default-template-output*)
		  (html-template:fill-and-print-template template vals)))))

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
				(cons (add-match (pp "type")
								 (trim (pp "deck"))
								 (pp "against")
								 (trim (pp "notes"))
								 (if (equal (pp "outcome") "win") t))
					  (count-deck (trim (pp "deck")))))))
	(let* ((get-deck (if (gp "deck") (trim (gp "deck"))))
		   (post-deck (if (pp "deck") (trim (pp "deck"))))
		   (get-against (gp "against"))
		   (get-notes (gp "notes"))
		   (active-deck (let ((deck (or post-deck get-deck)))
						  (if (and deck (not (equal deck ""))) deck)))
		   (type *last-type*)
		   (daily-graph (daily-match-stats->template active-deck))
		   (against-graph (against-stats->template active-deck))
		   (season (stats-this-season))
		   (last-matches (all-matches->template active-deck
												(lk 'match-limit *config*)))
		   (vals (list :heroes (hero-select get-against)
					   :notes get-notes
					   :last-deck active-deck
					   :last-added (car *last-added*)
					   :no-results (< (length last-matches) 1)
					   :new-deck (if *last-added* (< (cdr *last-added*) 2))
					   :filter-deck active-deck
					   :encoded-deck (do-urlencode:urlencode active-deck)
					   :all-match-stats (all-match-stats->template active-deck)
					   :all-decks (all-decks->template)
					   :all-matches last-matches
					   :daily-graph-labels (lk 'labels daily-graph)
					   :daily-graph-wins (lk 'wins daily-graph)
					   :daily-graph-losses (lk 'losses daily-graph)
					   :daily-graph-winrate (lk 'winrate daily-graph)
					   :against-graph-labels (lk 'labels against-graph)
					   :against-graph-wins (lk 'wins against-graph)
					   :against-graph-losses (lk 'losses against-graph)
					   :against-graph-winrate (lk 'winrate against-graph)
					   :seen-against-chart (seen-against-chart->template active-deck)
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
			   (str "^" (lk 'url-prefix *config*) "/matches$")
			   'matches-page)
			  (hunchentoot:create-regex-dispatcher
			   (str "^" (lk 'url-prefix *config*) "/$")
			   'index-page)))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
									:port (lk 'port *config*))))
