(in-package #:sistemas-graficos)

(defgeneric draw (window)
  (:documentation "Método donde se debe incluír el código de usuario
  para dibujar."))

(defgeneric mouse-button (window button state x y)
  (:documentation "Método donde se debe incluír el manejo de los
  botones del mouse."))

(defgeneric mouse-motion (window x y)
  (:documentation "Método donde se debe incluír el manejo del
  movimiento del mouse."))

(defgeneric mouse-passive-motion (window x y)
  (:documentation "Método donde se debe incluír el manejo del
  movimiento del mouse cuando no hay ningún botón presionado."))

(defgeneric set-3d-env (window)
  (:documentation ""))

(defgeneric get-key (window key)
  (:documentation ""))

(defgeneric add-key (window key function)
  (:documentation ""))

(defclass framework-window (glut:window)
  ((key-map :initform (make-hash-table :test 'equal) :accessor key-map))
  (:default-initargs :pos-x 0 :pos-y 0 :width 1024 :height 768
                     :mode '(:double :rgb :depth) :title "Framework"))

(defmethod draw ((window framework-window)))

(defmethod handle-mouse ((window framework-window) button state x y))

(defmethod mouse-motion ((window framework-window) x y))

(defmethod mouse-passive-motion ((window framework-window) x y))
  
(defmethod set-3d-env ((w framework-window))
  (with-accessors ((width glut:width) (height glut:height)) w
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 60 (/ width height) 0.1 100)))
    
(defmethod init (window)
  (:documentation ""))

(defmethod get-key ((w framework-window) key)
  (gethash key (key-map w) #'identity))

(defmethod add-key ((w framework-window) key function)
  (setf (gethash key (key-map w)) function))

(defmethod init ((w framework-window)))

(defmethod glut:display-window :before ((w framework-window))
  (init w))

(defmethod glut:display ((w framework-window))
  (set-3d-env w)
  (draw w)
  (glut:swap-buffers))

(defmethod glut:reshape ((w framework-window) width height)
  (with-accessors ((w-width glut:width) (w-height glut:height)) w
    (setf w-width width)
    (setf w-height height)))

(defmethod glut:mouse ((w framework-window) button state x y)
  (handle-mouse w button state x y))

(defmethod glut:motion ((w framework-window) x y)
  (mouse-motion w x y))

(defmethod glut:passive-motion ((w framework-window) x y)
  (mouse-passive-motion w x y))

(defmethod glut:keyboard ((w framework-window) key x y)
  (declare (ignore x y))
  (funcall (get-key w key) w)
  (glut:post-redisplay))

(defun main (window)
  (glut:display-window window))

