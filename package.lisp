;;;; package.lisp

(defpackage #:sistemas-graficos
  (:nicknames #:sg)
  (:use #:cl)
  (:export :main
	   :framework-window
	   :draw
	   :mouse-button
	   :mouse-motion
	   :mouse-passive-motion
	   :add-key
	   :cuadrado
	   :circulo
	   :cilindro))
