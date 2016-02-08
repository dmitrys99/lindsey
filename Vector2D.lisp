(in-package #:lindsey)

(defclass Vector2D ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defmethod print-object ((v Vector2D) stream)
  (format stream "Vector2D <~A, ~A>" (x v) (y v)))

(defmethod length ((v Vector2D))
  (sqrt (cl:+
	 (cl:* (x v) (x v))
	 (cl:* (y v) (y v)))))

(defmethod dot ((v Vector2D) (a Vector2D))
  (cl:+
   (cl:* (x v) (x a))
   (cl:* (y v) (y a))))

(defmethod cross ((v Vector2D) (a Vector2D))
  (cl:-
   (cl:* (x v) (y a))
   (cl:* (y v) (x a))))

(defmethod unit ((v Vector2D))
  (/ v (length v)))

(defgeneric / (v a))
(defmethod / ((v Vector2D) scalar)
  (make-instance 'Vector2D
		 :x (cl:/ (x v) scalar)
		 :y (cl:/ (y v) scalar)))
(defmethod /= ((v Vector2D) scalar)
  (setf (x v) (cl:/ (x v) scalar)
	(y v) (cl:/ (y v) scalar))
  v)

(defgeneric * (v a))
(defmethod * ((v Vector2D) scalar)
  (make-instance 'Vector2D
		 :x (cl:* (x v) scalar)
		 :y (cl:* (y v) scalar)))
(defmethod *= ((v Vector2D) scalar)
  (setf (x v) (cl:* (x v) scalar)
	(y v) (cl:* (y v) scalar))
  v)

(defgeneric + (v a))
(defmethod + ((v Vector2D) (a Vector2D))
  (make-instance 'Vector2D
		 :x (cl:+ (x v) (x a))
		 :y (cl:+ (y v) (y a))))

(defmethod += ((v Vector2D) (a Vector2D))
  (setf (x v) (cl:+ (x v) (x a))
	(y v) (cl:+ (y v) (y a)))
  v)

(defgeneric - (v a))
(defmethod - ((v Vector2D) (a Vector2D))
  (make-instance 'Vector2D
		 :x (cl:- (x v) (x a))
		 :y (cl:- (y v) (y a))))
(defmethod -= ((v Vector2D) (a Vector2D))
  (setf (x v) (cl:- (x v) (x a))
	(y v) (cl:- (y v) (y a)))
  v)

(defmethod perp ((v Vector2D))
  (make-instance 'Vector2D
		 :x (cl:- (y v))
		 :y (x v)))

(defmethod perpendicular ((v Vector2D) (a Vector2D))
  (- v (project v a)))

(defmethod project ((v Vector2D) (a Vector2D))
  (let ((percent (cl:/ (dot v a) (dot a a))))
    (* a percent)))

(defmethod fromPoints ((v Vector2D) (p1 Point2D) (p2 Point2D))
  (make-instance 'Vector2D
		 :x (cl:- (x p2) (x p1))
		 :y (cl:- (y p2) (y p1))))
