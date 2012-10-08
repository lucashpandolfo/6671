;;;; sistemas-graficos.asd

(asdf:defsystem #:sistemas-graficos
  :serial t
  :depends-on (#:cl-opengl
               #:cl-glu
               #:cl-glut)
  :components ((:file "package")
               (:file "framework")
               (:file "gridded-window")
	       (:file "primitivas")
               (:file "arbol/arbol")))

