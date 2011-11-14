(defun cuadrado ()
  "Dibuja un cuadrado de lado 1 centrado."
  (gl:with-primitive :triangle-fan
    (gl:normal 0 0 -1)
    (gl:tex-coord 0 0)
    (gl:vertex -0.5 -0.5)
    (gl:tex-coord 1 0)
    (gl:vertex -0.5 0.5)
    (gl:tex-coord 1 1)
    (gl:vertex 0.5 0.5)
    (gl:tex-coord 0 1)
    (gl:vertex 0.5 -0.5)))

(defun circulo (segmentos)
  "Dibuja un circulo centrado de radio 1 conformado por la cantidad de
segmentos dados."
  (gl:with-primitive :triangle-fan
    (gl:normal 0 0 1)
    (gl:vertex 0 0 0)
    (loop for i = 0 then (+ i (/ (* 2 pi) segmentos))
	 while (<= i (* 2 pi))
	 do
	 (gl:vertex (cos i) (sin i) 0))))

(defun cilindro (lados &optional (radio-superior 1))
  "Dibuja un cilindro de radio 1 y altura 1."
  (gl:with-primitive :triangle-strip
    (loop for i = 0 then (+ i (/ (* 2 pi) lados))
       while (<= i (* 2 pi))
       do
	 (let ((cos (cos i))
	       (sin (sin i)))
	 (gl:normal cos sin 0)
	 (gl:vertex cos sin -0.5)
	 (gl:vertex (* radio-superior cos) 
		    (* radio-superior sin) 0.5))))
  ;;Dibujo las tapas
  (gl:with-pushed-matrix
    (gl:translate 0 0 0.5)
    (gl:scale radio-superior radio-superior 1)
    (circulo lados))
  (gl:with-pushed-matrix
    (gl:translate 0 0 -0.5)
    (gl:rotate 180 1 0 0)
    (circulo lados)))
