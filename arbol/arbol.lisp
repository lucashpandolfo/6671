(defclass ventana-arbol (sg:gridded-window)
  ((edad :initform 4 :accessor edad)
   (lista :initform nil :accessor lista)
   (usar-lista :initform nil :accessor usar-lista)
   (porcentaje :initform 0 :accessor porcentaje)
   (capturar-mouse :initform nil :accessor capturar-mouse)
   (posicion-anterior :initform (make-array 3) :accessor posicion-anterior)))

(defun rama (ancho largo)
  (gl:color 0.74 0.2 0.1)
  (gl:with-pushed-matrix
    (gl:translate 0 0 (/ largo 2.0))
    (glu::cylinder (glu:new-quadric) ancho (* 0.7 ancho) largo 20 1)))

(defun hoja (largo)
  (gl:color 0 0.5 0.1 0.5)
  (gl:with-primitive :quads
    (gl:normal -1 0 0)
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

(defun arbol-animado (n porcentaje &optional (nivel 0))
  (gl:with-pushed-matrix
    (gl:scale porcentaje porcentaje porcentaje)
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
		(arbol-animado (- n 1) porcentaje (+ nivel 1)))))))))  

(defmethod toggle-lista ((ventana ventana-arbol))
  (setf (usar-lista ventana) (not (usar-lista ventana)))
  (recompilar-lista ventana)
  (format t "Usar lista compilada: ~a~%" (usar-lista ventana)))

(defmethod toggle-capturar-mouse ((ventana ventana-arbol))
  (if (capturar-mouse ventana)
      (progn
	(glut:set-cursor :cursor-inherit)
	(setf (capturar-mouse ventana) nil))
      (progn 
	(glut:set-cursor :cursor-none)
	(with-accessors ((width glut:width) (height glut:height)) ventana
	  (glut:warp-pointer (floor (/ width 2))
			     (floor (/ height 2))))
	(setf (posicion-anterior ventana) #((floor (/ width 2))
					    (floor (/ height 2)) 0))
	(setf (capturar-mouse ventana) t)))
  (format t "Capturar mouse: ~a~%" (capturar-mouse ventana)))

(defmethod recompilar-lista ((ventana ventana-arbol))
  (gl:with-new-list ((lista ventana) :compile)
    (arbol (edad ventana))))

(defmethod aumentar-edad ((ventana ventana-arbol))
  (when (< (edad ventana) 6)
    (incf (edad ventana))
    (recompilar-lista ventana)))

(defmethod disminuir-edad ((ventana ventana-arbol))
  (when (> (edad ventana) 0)
    (decf (edad ventana))
    (recompilar-lista ventana)))

(defmethod initialize-instance :after ((ventana ventana-arbol) &key)
  (sg:add-key ventana #\r (lambda (ventana) 
			    (setf (porcentaje ventana) 0)))
  (sg:add-key ventana #\c #'toggle-capturar-mouse)
  (sg:add-key ventana #\m #'aumentar-edad)
  (sg:add-key ventana #\M #'disminuir-edad)
  (sg:add-key ventana #\l #'toggle-lista))

(defmethod rotar-mundo ((ventana ventana-arbol) x y z)
  (let ((anterior (sg::world-rotation ventana)))
    (setf (sg::world-rotation ventana) 
	  (coerce 
	   (mapcar #'+ (coerce anterior 'list) (list x y z))
	   'vector))))

(defmethod sg:draw ((ventana ventana-arbol))
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  (unless (lista ventana)
    (setf (lista ventana) (gl:gen-lists 1)))

  (when (< (porcentaje ventana) 1)
    (glut:schedule-timer 50 (lambda () 
			       (setf (porcentaje ventana) 
				     (+ (porcentaje ventana) 0.01))
			       (glut:post-redisplay))))

  (arbol-animado (edad ventana) (porcentaje ventana)))

(defmethod sg::mouse-passive-motion ((ventana ventana-arbol) x y)
  (when (capturar-mouse ventana)
    (with-accessors ((width glut:width) (height glut:height)) ventana
      (if (or
	   (> (abs (- (/ width 2) x)) (/ width 4))
	   (> (abs (- (/ height 2) y)) (/ height 4)))
	  (glut:warp-pointer (floor (/ width 2))
			     (floor (/ height 2)))))))
    

(defun main ()
  (sg:main (make-instance 'ventana-arbol)))
