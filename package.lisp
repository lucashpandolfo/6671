;;;; package.lisp

(defpackage #:sistemas-graficos
  (:nicknames #:sg)
  (:use #:cl)
  (:export :main
	   :framework-window
	   :draw
	   :add-key))
