;;;; sistemas-graficos.asd

(asdf:defsystem #:sistemas-graficos
  :serial t
  :depends-on (#:cl-opengl
               #:cl-glu
               #:cl-glut)
  :components ((:file "package")
               (:file "framework"  :depends-on ("package"))
	       (:file "primitivas" :depends-on ("package"))))

