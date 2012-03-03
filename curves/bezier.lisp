(in-package #:sistemas-graficos.curves)

(defclass bezier () 
  ((control-points :initform nil :initarg :control-points :accessor control-points)
   (degree :initform 3 :initarg degree :reader degree)))

(defmethod add-control-point ((bezier bezier) point)
  (setf (control-points bezier)
        (append (control-points bezier) (list point))))

(defun vector-add (&rest elements)
  (reduce (lambda (v1 v2) (loop 
                             for i in v1
                             for j in v2
                             collect (+ i j)))
          elements))

(defun vector-mult (vector &rest floats)
  (let ((constant (reduce #'* floats)))
    (loop for i in vector
         collect (* i constant))))

(defmethod get-point ((bezier bezier) u total-points)
  (with-accessors ((points control-points)) bezier
    (let ((total-segments (1+ (floor (/ 
                                      (-(length (control-points bezier)) 4) 
                                      3)))))
      (multiple-value-bind (segment offset) 
          (floor (* u total-segments))
        (let ((first-point (* segment 3))
              (p offset)
              (one-minus-p (- 1 offset)))
          (vector-add 
           (vector-mult (nth first-point points) one-minus-p one-minus-p one-minus-p)
           (vector-mult (nth (+ first-point 1) points) 3 p one-minus-p one-minus-p)
           (vector-mult (nth (+ first-point 2) points) 3 p p one-minus-p)
           (vector-mult (nth (+ first-point 3) points) p p p)))))))

(defmethod discretize ((bezier bezier) points)
  (loop for i from 0 to points
       collect (get-point bezier (/ i points) points)))
