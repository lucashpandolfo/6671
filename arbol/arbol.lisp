(require 'sistemas-graficos)

(defun rama (ancho largo)
  (gl:color 0.6 0.3 0)
  (glu::cylinder (glu:new-quadric) ancho ancho largo 20 1))

(defun hoja (largo)
  (gl:color 0 0.7 0.2)
  (gl:with-primitive :quads
    (gl:vertex 0 (/ (- largo) 2) 0)
    (gl:vertex 0 0 (- largo))
    (gl:vertex 0 (/ largo 2) 0)
    (gl:vertex 0 0 largo)))

(defun arbol (n)
  (if (eq n 0)
      (hoja 4)
      (progn
	(rama 1 6)
	(gl:translate 0 0 6)
	(dotimes (i 5)
	  (gl:with-pushed-matrix
	    (gl:rotate (* (/ 360 5) i) 0 0 1)
	    (gl:rotate 50 0 1 0)
	    (gl:scale 0.7 0.7 0.7)
	    (arbol (- n 1)))))))
      
(defmethod sg:draw ((window sg:framework-window))
  (gl:color 0.1 0.3 0.7)
  (arbol 5))

(sg:main (make-instance 'sg:framework-window))
