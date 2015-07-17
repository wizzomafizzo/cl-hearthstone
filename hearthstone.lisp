;;;; hearthstone.lisp

(load "packages.lisp")
(in-package :hearthstone)

(load "utils.lisp")

(defvar *config*
  '((db . "hearthstone.db")
	(port . 8888)
	(match-limit . 100)
	(winrate-tiers . (50 60))
	(heroes . ("Druid" "Hunter" "Mage"
			   "Paladin" "Priest" "Rogue"
               "Shaman" "Warlock" "Warrior"))
	(winrate-legend . ((90 . 70) (80 . 83)
					   (70 . 103) (60 . 140)
					   (55 . 181) (51 . 243)
					   (50 . 267) (0 . 1000)))))

(defvar *auth* (with-open-file (in "auth.conf") (read in)))
(defvar *last-added* nil)
(defvar *last-type* nil)

(defvar *game-types*
  (flatten (list "Casual"
				 (reverse (loop for i from 1 to 25 collect
							   (str "Rank " (write-to-string i))))
				 "Legend")))

(defvar *db* (sqlite:connect (lk 'db *config*)))

(load "database.lisp")
(load "web.lisp")

(create-db)
(defvar *web* (start-server))
