(in-package #:sg.curves)

(defclass ventana-bezier (sg:framework-window)())

(defparameter *control-points* '((0   0  0.8)
                                 (1.3 0  0.8)
                                 (2.5 0 -0.5)
                                 (3   0  0.3)
                                 (6   0    3)
                                 (0.5 0    2)
                                 (4   0   11)
                                 (4.8 0   20)
                                 (1.5 0   18)
                                 (1   0   24)
                                 (1.5 0   24.5)
                                 (1.3 0   25)
                                 (1.3 0   26.5)))

(defparameter *bezier* (make-instance 'bezier :control-points *control-points*))

(defun vertexlist (vertex)
  (gl:vertex (car vertex) (cadr vertex) (caddr vertex)))

(defun dibujar-curva (subdivisiones &optional &key (control-points-p nil) (control-lines-p nil))
  (gl:line-width 3)
  (gl:color 1 1 1)
  (gl:with-primitive :line-strip
    (dotimes (i subdivisiones)
      (let ((vertex (get-point *bezier* (/ i subdivisiones) subdivisiones)))
        (gl:vertex (car vertex) (cadr vertex) (caddr vertex)))))
  (gl:line-width 1)

  (with-accessors ((control-points control-points)) *bezier*
    (when control-points-p

      (gl:point-size 4)
      (gl:color 0 0 1)
      (gl:with-primitive :points
        (dolist (i (control-points *bezier*))
          (gl:vertex (car i) (cadr i) (caddr i)))))

    (when control-lines-p

      (gl:color 1 0 0)
      (gl:with-primitive :lines
        (loop for i from 0 by 3
           while (not (null (nth (+ i 1) control-points)))
           do
             (vertexlist (nth i control-points))
             (vertexlist (nth (+ i 1) control-points))
             
             (vertexlist (nth (+ i 2) control-points))
             (vertexlist (nth (+ i 3) control-points)))))))

(defmethod sg:draw ((ventana ventana-bezier))
  (gl:disable :lighting)
  (gl:disable :depth-test)
  (gl:hint :line-smooth-hint :nicest)
  (gl:enable :line-smooth)
  (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at 5 -15 13
               5 0 13
               1 0 0)
  (dibujar-curva 150 :control-lines-p t :control-points-p t))

;;(sg:main (make-instance 'ventana-bezier))
