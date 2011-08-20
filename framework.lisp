(in-package #:sistemas-graficos)

(defgeneric draw (window)
  (:documentation "Método donde se debe incluír el código de usuario para dibujar."))

(defclass framework-window (glut:window)
  ((eye :initform #(15 15 5) :accessor eye)
   (at :initform #(0 0 0) :accessor at)
   (up :initform #(0 0 1) :accessor up)
   (light-color :initform #(1 1 1 1) :accessor light-color)
   (light-position :initform #(10 10 10 0) :accessor light-position)
   (light-ambient :initform #(0.05 0.05 0.05 1) :accessor light-ambient)
   (view-grid :initform t :accessor view-grid)
   (view-axis :initform t :accessor view-axis)
   (edit-panel :initform nil :accessor edit-panel)
   (world-rotation :initform #(0 0 0) :accessor world-rotation))
  (:default-initargs :pos-x 0 :pos-y 0 :width 1024 :height 768
                     :mode '(:double :rgb :depth) :title "Framework"))

(defmethod toggle-grid ((w framework-window))
  (setf (view-grid w) (not (view-grid w))))

(defmethod toggle-axis ((w framework-window))
  (setf (view-axis w) (not (view-axis w))))

(defmethod toggle-edit-panel ((w framework-window))
  (setf (edit-panel w) (not (edit-panel w))))

(defun draw-axis ()
  (gl:disable :lighting)
  (gl:with-primitive :lines
    ;;; X
    (gl:color 1 0 0)
    (gl:vertex 0 0 0)
    (gl:color 0 0 0)
    (gl:vertex 15 0 0)
    ;;; Y
    (gl:color 0 1 0)
    (gl:vertex 0 0 0)
    (gl:color 0 0 0)
    (gl:vertex 0 15 0)
    ;;; Z
    (gl:color 0 0 1)
    (gl:vertex 0 0 0)
    (gl:color 0 0 0)
    (gl:vertex 0 0 15))
  (gl:enable :lighting))

(defun draw-axis-2d-top-view ()
  (gl:disable :lighting)
  (gl:with-primitive :line-loop
    (gl:color 0 0.5 1)
    (gl:vertex 0 0 0)
    (gl:vertex 1 0 0)
    (gl:vertex 1 1 0)
    (gl:vertex 0 1 0))
  (gl:with-primitive :quads
    (gl:color 0.1 0.1 0.1)
    (gl:vertex 0 0 0)
    (gl:vertex 1 0 0)
    (gl:vertex 1 1 0)
    (gl:vertex 0 1 0))
  (gl:enable :lighting))

(defun draw-xy-grid ()
  (gl:disable :lighting)
  (gl:color 0.25 0.2 0.2)
  (gl:with-primitive :lines
    (do ((i -20 (1+ i)))
	((eq i 21) nil)
      (gl:vertex i -20 0)
      (gl:vertex i 20 0)
      (gl:vertex -20 i 0)
      (gl:vertex 20 i 0)))
  (gl:enable :lighting))

(defmethod set-3d-env ((w framework-window))
  (with-accessors ((width glut:width) (height glut:height)) w
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 60 (/ width height) 0.1 100)))

(defmethod set-panel-top-env ((w framework-window))
  (with-accessors ((width glut:width) (height glut:height)) w
    (gl:viewport (* 0.6 width)
		 (* 0.6 height)
		 (* 0.4 width)
		 (* 0.4 height)))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d -0.1 1.05 -0.1 1.05))

(defmethod init ((w framework-window))
  (setf (world-rotation w) (make-array 3 ))
  (gl:clear-color 0.02 0.02 0.04 0)
  (gl:shade-model :smooth)
  (gl:enable :depth-test)
  (gl:light :light0 :diffuse (light-color w))
  (gl:light :light0 :ambient (light-ambient w))
  (gl:light :light0 :position (light-position w))
  (gl:enable :light0)
  (gl:enable :lighting))

(defmethod glut:display-window :before ((w framework-window))
  (init w))

(defmethod glut:display ((w framework-window))
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (set-3d-env w)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (with-slots (eye at up world-rotation) w
    (glu:look-at (aref eye 0) (aref eye 1) (aref eye 2)
		 (aref at 0) (aref at 1) (aref at 2)
		 (aref up 0) (aref up 1) (aref up 2))
    (gl:rotate (aref world-rotation 0) 1 0 0)
    (gl:rotate (aref world-rotation 1) 0 1 0)
    (gl:rotate (aref world-rotation 2) 0 0 1))

  (when (view-axis w)
    (draw-axis))
  
  (when (view-grid w)
    (draw-xy-grid))
  (handler-case
      (draw w)
    (simple-error () (error "Se debe definir el método (draw ((w framework-window))).~%")))

  (when (edit-panel w)
    (set-panel-top-env w)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (glu:look-at 0 0 0.5 0 0 0 0 1 0)
    (draw-axis-2d-top-view))
  (glut:swap-buffers))

(defmethod glut:reshape ((w framework-window) width height)
  (with-accessors ((w-width glut:width) (w-height glut:height)) w
    (setf w-width width)
    (setf w-height height)))


(defmethod glut:keyboard ((w framework-window) key x y)
  (declare (ignore x y))
  (case key
    (#\g (toggle-grid w))
    (#\a (toggle-axis w))
    (#\e (toggle-edit-panel w))
    (#\2 (setf (eye w) #(0 0 15))
	 (setf (at w) #(0 0 0))
	 (setf (up w) #(0 1 0)))
    (#\3 (setf (eye w) #(15 15 5))
	 (setf (at w) #(0 0 0))
	 (setf (up w) #(0 0 1)))
    (#\i (init w))
    (#\x (incf (aref (world-rotation w) 0) 3))
    (#\X (decf (aref (world-rotation w) 0) 3))
    (#\y (incf (aref (world-rotation w) 1) 3))
    (#\Y (decf (aref (world-rotation w) 1) 3))
    (#\z (incf (aref (world-rotation w) 2) 3))
    (#\Z (decf (aref (world-rotation w) 2) 3)))
  (glut:post-redisplay))

(defun main (window)
  (glut:display-window window))
