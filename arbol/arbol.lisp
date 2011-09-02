(require 'sistemas-graficos)

(defclass ventana-arbol (sg:framework-window)
  ((edad :initform 4 :accessor edad)))

(defmethod aumentar-edad ((ventana ventana-arbol))
  (when (< (edad ventana) 6)
    (incf (edad ventana))))

(defmethod disminuir-edad ((ventana ventana-arbol))
  (when (> (edad ventana) 0)
    (decf (edad ventana))))

(defmethod initialize-instance :after ((ventana ventana-arbol) &key)
  (sg:add-key ventana #\m #'aumentar-edad)
  (sg:add-key ventana #\M #'disminuir-edad))

(defun rama (ancho largo)
  (gl:color 0.5 0.2 0)
  (gl:with-pushed-matrix
    (gl:translate 0 0 (/ largo 2))
    (glu::cylinder (glu:new-quadric) ancho (* 0.7 ancho) largo 20 1)))

(defun hoja (largo)
  (gl:color 0 0.5 0.1)
  (gl:with-primitive :quads
    (gl:vertex 0 (/ (- largo) 2) 0)
    (gl:vertex 0 0 (- largo))
    (gl:vertex 0 (/ largo 2) 0)
    (gl:vertex 0 0 largo)))

(defun arbol (n &optional (nivel 0))
  (let ((ancho-rama (* 1 (expt 0.5 nivel)))
	(largo-rama (* 6 (expt 0.6 nivel)))
	(largo-hoja (* 4 (expt 0.7 nivel)))
	(angulos '(50 60 70 65 45)))
    (if (eq n 0)
	(hoja largo-hoja)
	(progn
	  (gl:with-pushed-matrix
	    (gl:translate 0 0 (- (/ largo-rama 2)))
	    (rama ancho-rama  largo-rama))
	(dotimes (i 5)
	  (gl:with-pushed-matrix
	    (gl:translate 0 0 (if (eq n 1)
				  largo-rama
				  (* 0.9 largo-rama)))

	    (gl:rotate (* (/ 360 5) i) 0 0 1)
	    (when (eq n 1)
	      (gl:translate (/ largo-hoja 2) 0 0))
	    (gl:rotate (nth (mod (+ i nivel) 5) angulos) 0 1 0)
	    (arbol (- n 1) (+ nivel 1))))))))

(defmethod sg:draw ((window ventana-arbol))
  (arbol (edad window)))

(sg:main (make-instance 'ventana-arbol))

