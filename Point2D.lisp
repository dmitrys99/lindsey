(in-package #:lindsey)

(defclass Point2D ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

;; TODO DRY for similar methods

(defmethod print-object ((p Point2D) stream)
  (format stream "Point2D <~A, ~A>" (x p) (y p)))

(defmethod clone ((p Point2D))
  (make-instance 'Point2D :x (x p) :y (y p)))

(defmethod rmoveto ((p Point2D) dx dy)
  (setf (x p) (+ (x p) dx)
	(y p) (+ (y p) dy)))

(defgeneric + (p a))
(defmethod + ((p Point2D) (a Point2D))
  (make-instance 'Point2D
		 :x (cl:+ (x p) (x a))
		 :y (cl:+ (y p) (y a))))

(defmethod += ((p Point2D) (a Point2D))
  (setf (x p) (+ (x p) (x a))
	(y p) (+ (y p) (y a)))
  p)

(defmethod + ((p Point2D) scalar)
  (make-instance 'Point2D
		 :x (cl:+ (x p) scalar)
		 :y (cl:+ (y p) scalar)))

(defmethod += ((p Point2D) scalar)
  (setf (x p) (cl:+ (x p) scalar)
	(y p) (cl:+ (y p) scalar))
  p)

(defgeneric - (p a))
(defmethod - ((p Point2D) (a Point2D))
  (make-instance 'Point2D
		 :x (cl:- (x p) (x a))
		 :y (cl:- (y p) (y a))))

(defgeneric -= (p a))
(defmethod -= ((p Point2D) (a Point2D))
  (setf (x p) (cl:- (x p) (x a))
	(y p) (cl:- (y p) (y a)))
  p)

(defmethod - ((p Point2D) scalar)
  (make-instance 'Point2D
		 :x (cl:- (x p) scalar)
		 :y (cl:- (y p) scalar)))

(defmethod -= ((p Point2D) scalar)
  (setf (x p) (cl:- (x p) scalar)
	(y p) (cl:- (y p) scalar))
  p)

(defgeneric * (p a))
(defmethod * ((p Point2D) scalar)
  (make-instance 'Point2D
		 :x (cl:* (x p) scalar)
		 :y (cl:* (y p) scalar)))

(defmethod *= ((p Point2D) scalar)
  (setf (x p) (cl:* (x p) scalar)
	(y p) (cl:* (y p) scalar))
  p)

(defgeneric / (p a))
(defmethod / ((p Point2D) scalar)
  (make-instance 'Point2D
		 :x (cl:/ (x p) scalar)
		 :y (cl:/ (y p) scalar)))

(defgeneric /= (p a))
(defmethod /= ((p Point2D) scalar)
  (setf (x p) (cl:/ (x p) scalar)
	(y p) (cl:/ (y p) scalar))
  p)

(defgeneric < (p a))
(defmethod < ((p Point2D) (a Point2D))
  (and (cl:< (x p) (x a))
       (cl:< (y p) (y a))))

(defgeneric > (p a))
(defmethod > ((p Point2D) (a Point2D))
  (and (cl:> (x p) (x a))
       (cl:> (y p) (y a))))

(defgeneric <= (p a))
(defmethod <= ((p Point2D) (a Point2D))
  (and (cl:<= (x p) (x a))
       (cl:<= (y p) (y a))))

(defgeneric >= (p a))
(defmethod >= ((p Point2D) (a Point2D))
  (and (cl:>= (x p) (x a))
       (cl:>= (y p) (y a))))

(defmethod lerp ((p Point2D) (a Point2D) tt)
  (make-instance 'Point2D
		 :x (cl:+ (x p) (cl:* tt (cl:- (x a) (x p))))
		 :y (cl:+ (y p) (cl:* tt (cl:- (y a) (y p))))))

(defmethod distance ((p Point2D) (a Point2D))
  (let ((dx (cl:- (x p) (x a)))
	(dy (cl:- (y p) (y a))))
    (sqrt (cl:+ (cl:* dx dx) (cl:* dy dy)))))

(defgeneric min (p a))
(defmethod min ((p Point2D) (a Point2D))
  (make-instance 'Point2D
		 :x (min (x p) (x a))
		 :y (min (y p) (y a))))

(defgeneric max (p a))
(defmethod max ((p Point2D) (a Point2D))
  (make-instance 'Point2D
		 :x (max (x p) (x a))
		 :y (max (y p) (y a))))

(defmethod setXY ((p Point2D) x y)
  (setf (x p) x
	(y p) y))

(defmethod setFromPoint ((p Point2D) (a Point2D))
  (setf (x p) (x a)
	(y p) (y a)))
