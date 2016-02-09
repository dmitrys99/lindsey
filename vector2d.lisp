(in-package #:lindsey)

(defclass Vector2D ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defun new-vector2d (x y)
  (make-instance 'Vector2D :x x :y y))

(defmethod print-object ((v Vector2D) stream)
  (format stream "Vector2D <~A, ~A>" (x v) (y v)))

(defmethod len ((v Vector2D))
  (sqrt (+ (* (x v) (x v))
	   (* (y v) (y v)))))

(defmethod dot ((v Vector2D) (a Vector2D))
  (+ (* (x v) (x a))
     (* (y v) (y a))))

(defmethod cross ((v Vector2D) (a Vector2D))
  (- (* (x v) (y a))
     (* (y v) (x a))))

(defmethod unit ((v Vector2D))
  (div v (len v)))

(defgeneric div (v a))
(defmethod div ((v Vector2D) scalar)
  (make-instance 'Vector2D
		 :x (/ (x v) scalar)
		 :y (/ (y v) scalar)))

(defmethod div= ((v Vector2D) scalar)
  (setf (x v) (/ (x v) scalar)
	(y v) (/ (y v) scalar))
  v)

(defgeneric mul (v a))
(defmethod mul ((v Vector2D) scalar)
  (make-instance 'Vector2D
		 :x (* (x v) scalar)
		 :y (* (y v) scalar)))
(defmethod mul= ((v Vector2D) scalar)
  (setf (x v) (* (x v) scalar)
	(y v) (* (y v) scalar))
  v)

(defgeneric add (v a))
(defmethod add ((v Vector2D) (a Vector2D))
  (make-instance 'Vector2D
		 :x (+ (x v) (x a))
		 :y (+ (y v) (y a))))

(defmethod add= ((v Vector2D) (a Vector2D))
  (setf (x v) (+ (x v) (x a))
	(y v) (+ (y v) (y a)))
  v)

(defgeneric sub (v a))
(defmethod sub ((v Vector2D) (a Vector2D))
  (make-instance 'Vector2D
		 :x (- (x v) (x a))
		 :y (- (y v) (y a))))
(defmethod sub= ((v Vector2D) (a Vector2D))
  (setf (x v) (- (x v) (x a))
	(y v) (- (y v) (y a)))
  v)

(defmethod perp ((v Vector2D))
  (make-instance 'Vector2D
		 :x (- (y v))
		 :y (x v)))

(defmethod perpendicular ((v Vector2D) (a Vector2D))
  (- v (project v a)))

(defmethod project ((v Vector2D) (a Vector2D))
  (let ((percent (/ (dot v a) (dot a a))))
    (mul a percent)))

(defmethod fromPoints ((v Vector2D) (p1 Point2D) (p2 Point2D))
  (make-instance 'Vector2D
		 :x (- (x p2) (x p1))
		 :y (- (y p2) (y p1))))

(defun vector-xy (v)
  (cons (x v)
	(y v)))
