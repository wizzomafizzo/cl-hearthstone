(load "~/quicklisp/setup.lisp")
(ql:quickload "sqlite")
(ql:quickload "hunchentoot")
(ql:quickload "html-template")
(ql:quickload "jsown")
(ql:quickload "do-urlencode")
(ql:quickload "net-telent-date")

(defpackage :hearthstone
  (:use :cl))
