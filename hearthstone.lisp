;;;; hearthstone.lisp

(load "packages.lisp")
(in-package :hearthstone)

(defvar *config*
  '((db . "hearthstone.db")
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

(defvar *auth* (with-open-file (in "auth.conf") (read in)))
(defvar *last-added* nil)

(load "utils.lisp")

(defvar *db* (sqlite:connect (lk 'db *config*)))

(load "database.lisp")
(load "web.lisp")

(create-db)
(defvar *web* (start-server))
