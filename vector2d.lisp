(in-package #:lindsey)

(defclass Vector2D ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y))
  (:documentation "Vector2D is an object used to encapsulate a 2-dimensional vector."))

(defun new-vector2d (x y)
  "Returns Vector2D object for given 2-dimensional vector."
  (make-instance 'Vector2D :x x :y y))

(defmethod print-object ((v Vector2D) stream)
  "Print method for Vector2D object."
  (format stream "Vector2D <~A, ~A>" (x v) (y v)))

(defmethod len ((v Vector2D))
  "Returns the length, or magnitude, of the vector."
  (sqrt (+ (* (x v) (x v))
	   (* (y v) (y v)))))

(defmethod dot ((v Vector2D) (a Vector2D))
  "Returns the dot product of this object and another Vector2D object."
  (+ (* (x v) (x a))
     (* (y v) (y a))))

(defmethod cross ((v Vector2D) (a Vector2D))
  "Returns the cross product of this object and another Vector2D object."
  (- (* (x v) (y a))
     (* (y v) (x a))))

(defmethod unit ((v Vector2D))
  "Returns the unit vector as new Vector2D"
  (div v (len v)))

(defmethod unit= ((v Vector2D))
  "Converts the current vector to a unit vector. The object is returned to allow multiple vector operations to be chained together."
  (div= v (len v)))

(defgeneric div (v a))
(defmethod div ((v Vector2D) scalar)
  "Returns a new Vector2D equal to this object divided by the specified scalar amount."
  (make-instance 'Vector2D
		 :x (/ (x v) scalar)
		 :y (/ (y v) scalar)))

(defmethod div= ((v Vector2D) scalar)
  "Divides this vector's components by the specified amount. The object is returned to allow multiple vector operations to be chained together."
  (setf (x v) (/ (x v) scalar)
	(y v) (/ (y v) scalar))
  v)

(defgeneric mul (v a))
(defmethod mul ((v Vector2D) scalar)
  "Returns a new Vector2D equal to this object multiplied by the specified scalar amount."
  (make-instance 'Vector2D
		 :x (* (x v) scalar)
		 :y (* (y v) scalar)))
(defmethod mul= ((v Vector2D) scalar)
  "Multiplies this vector's components by the specified amount. The object is returned to allow multiple vector operations to be chained together."
  (setf (x v) (* (x v) scalar)
	(y v) (* (y v) scalar))
  v)

(defgeneric add (v a))
(defmethod add ((v Vector2D) (a Vector2D))
  "Returns a new Vector2D that is the addition of this object and another Vector2D."
  (make-instance 'Vector2D
		 :x (+ (x v) (x a))
		 :y (+ (y v) (y a))))

(defmethod add= ((v Vector2D) (a Vector2D))
  "Adds another Vector2D to this object. The object is returned to allow multiple vector operations to be chained together."
  (setf (x v) (+ (x v) (x a))
	(y v) (+ (y v) (y a)))
  v)

(defgeneric sub (v a))
(defmethod sub ((v Vector2D) (a Vector2D))
  "Returns a new Vector2D that is the subtraction of another Vector2D from this object."
  (make-instance 'Vector2D
		 :x (- (x v) (x a))
		 :y (- (y v) (y a))))
(defmethod sub= ((v Vector2D) (a Vector2D))
  "Subtracts another Vector2D from this object. The object is returned to allow multiple vector operations to be chained together."
  (setf (x v) (- (x v) (x a))
	(y v) (- (y v) (y a)))
  v)

(defmethod perp ((v Vector2D))
  "Returns perpendicular vector to given."
  (make-instance 'Vector2D
		 :x (- (y v))
		 :y (x v)))

(defmethod perpendicular ((v Vector2D) (a Vector2D))
  "???"
  (- v (project v a)))

(defmethod project ((v Vector2D) (a Vector2D))
  "Returns vector projection of a vector v on a nonzero vector a."
  (let ((percent (/ (dot v a) (dot a a))))
    (mul a percent)))

(defmethod fromPoints ((v Vector2D) (p1 Point2D) (p2 Point2D))
  "Returns vector based on two given points."
  (make-instance 'Vector2D
		 :x (- (x p2) (x p1))
		 :y (- (y p2) (y p1))))

(defun vector-xy (v)
  "Returns vector's dimensions as a cons."
  (cons (x v)
	(y v)))
