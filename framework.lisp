(require 'cl-opengl)
(require 'cl-glu)
(require 'cl-glut)

(defparameter *eye* #(15 15 5))
(defparameter *at*  #(0 0 0))
(defparameter *up*  #(0 0 1))

(defparameter *light-color* #(1 1 1 1))
(defparameter *light-position* #(10 10 10 0))
(defparameter *light-ambient* #(0.05 0.05 0.05 1))

(defparameter *world-rotation* #(0 0 0))

(defparameter *view-grid* t)
(defparameter *view-axis* t)
(defparameter *edit-panel* nil)

(defparameter *w-width* 0)
(defparameter *w-height* 0)

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

(defun set-3d-env ()
  (gl:viewport 0 0 *w-width* *w-height*)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60 (/ *w-width* *w-height*) 0.1 100))

(defun set-panel-top-env ()
  (gl:viewport (* 0.6 *w-width*)
	       (* 0.6 *w-height*)
	       (* 0.4 *w-width*)
	       (* 0.4 *w-height*))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d -0.1 1.05 -0.1 1.05))


(defun init ()
  (setf *world-rotation* (make-array 3 ))
  (gl:clear-color 0.02 0.02 0.04 0)
  (gl:shade-model :smooth)
  (gl:enable :depth-test)
  (gl:light :light0 :diffuse *light-color*)
  (gl:light :light0 :ambient *light-ambient*)
  (gl:light :light0 :position *light-position*)
  (gl:enable :light0)
  (gl:enable :lighting))

(defun display ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  (set-3d-env)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at (aref *eye* 0) (aref *eye* 1) (aref *eye* 2)
	       (aref *at* 0) (aref *at* 1) (aref *at* 2)
	       (aref *up* 0) (aref *up* 1) (aref *up* 2))

  (gl:rotate (aref *world-rotation* 0) 1 0 0)
  (gl:rotate (aref *world-rotation* 1) 0 1 0)
  (gl:rotate (aref *world-rotation* 2) 0 0 1)


  (when *view-axis*
    (draw-axis))
  
  (when *view-grid*
    (draw-xy-grid))

  ;;;;;;;; DRAW HERE



  ;;;;;;;;;;;;;;;;;;

  (when *edit-panel*
    (set-panel-top-env)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (glu:look-at 0 0 0.5 0 0 0 0 1 0)
    (draw-axis-2d-top-view))

  (glut:swap-buffers))


(defun reshape (width height)
  (setf *w-width* width)
  (setf *w-height* height))

(defun keyboard (key x y)
  (declare (ignore x y))
  (case key
    (#\g (setf *view-grid* (not *view-grid*)))
    (#\a (setf *view-axis* (not *view-axis*)))
    (#\e (setf *edit-panel* (not *edit-panel*)))
    (#\2 (setf *eye* #(0 0 15))
	 (setf *at* #(0 0 0))
	 (setf *up* #(0 1 0)))
    (#\3 (setf *eye* #(15 15 5))
	 (setf *at* #(0 0 0))
	 (setf *up* #(0 0 1)))
    (#\i (init))
    (#\x (incf (aref *world-rotation* 0) 3))
    (#\X (decf (aref *world-rotation* 0) 3))
    (#\y (incf (aref *world-rotation* 1) 3))
    (#\Y (decf (aref *world-rotation* 1) 3))
    (#\z (incf (aref *world-rotation* 2) 3))
    (#\Z (decf (aref *world-rotation* 2) 3)))
  (glut:post-redisplay))

(defclass framework-window (glut:window)
  ()
  (:default-initargs :pos-x 0 :pos-y 0 :width 1024 :height 768
                     :mode '(:double :rgb :depth) :title "Framework"))

(defmethod glut:display-window :before ((w framework-window))
  (init))

(defmethod glut:display ((w framework-window))
  (display))

(defmethod glut:reshape ((w framework-window) width height)
  (reshape width height))

(defmethod glut:keyboard ((w framework-window) key x y)
  (keyboard key x y))

(defun start ()
  (glut:display-window (make-instance 'framework-window)))
