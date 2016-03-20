(in-package #:lindsey)

(defclass Point2D ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y))
  (:documentation "Point2D is an object used to encapsulate a 2-dimensional coordinate."))

(defun new-point2d (x y)
  "Returns Point2D object for given 2-dimensional coordinates."
  (make-instance 'Point2D :x x :y y))

;; TODO DRY for similar methods

(defmethod print-object ((p Point2D) stream)
  "Print method for Point2D object."
  (format stream "Point2D <~A, ~A>" (x p) (y p)))

(defmethod clone ((p Point2D))
  "Returns exact copy for given object."
  (make-instance 'Point2D :x (x p) :y (y p)))

(defmethod rmoveto ((p Point2D) dx dy)
  "Move given Point2D for offsets dx and dy."
  (setf (x p) (+ (x p) dx)
	(y p) (+ (y p) dy)))

(defgeneric add (p a))
(defmethod add ((p Point2D) (a Point2D))
  "Returns a new Point2D that is the addition of this object and another Point2D."
  (make-instance 'Point2D
		 :x (+ (x p) (x a))
		 :y (+ (y p) (y a))))

(defmethod add ((p Point2D) scalar)
  "Returns a new Point2D that is equal to the addition of the specified scalar to both coordinates in the object."
  (make-instance 'Point2D
		 :x (+ (x p) scalar)
		 :y (+ (y p) scalar)))

(defgeneric add= (p a))
(defmethod add= ((p Point2D) (a Point2D))
  "Adds another Point2D to this object. The object is returned to allow multiple point operations to be chained together."
  (setf (x p) (+ (x p) (x a))
	(y p) (+ (y p) (y a)))
  p)

(defmethod add= ((p Point2D) scalar)
  "Adds the specified scalar to both coordinates of the object. The object is returned to allow multiple point operations to be chained together."
  (setf (x p) (+ (x p) scalar)
	(y p) (+ (y p) scalar))
  p)

(defgeneric sub (p a))
(defmethod sub ((p Point2D) (a Point2D))
  "Returns a new Point2D that is the subtraction another Point2D from this object."
  (make-instance 'Point2D
		 :x (- (x p) (x a))
		 :y (- (y p) (y a))))

(defmethod sub ((p Point2D) scalar)
  "Returns a new Point2D that is equal to the subtraction of the specified scalar from both coordinates in the object."
  (make-instance 'Point2D
		 :x (- (x p) scalar)
		 :y (- (y p) scalar)))

(defgeneric sub= (p a))
(defmethod sub= ((p Point2D) (a Point2D))
  "Subtracts another Point2D from this object. The object is returned to allow multiple point operations to be chained together."
  (setf (x p) (- (x p) (x a))
	(y p) (- (y p) (y a)))
  p)

(defmethod sub= ((p Point2D) scalar)
  "Returns a new Point2D that is equal to the subtraction of the specified scalar from both coordinates in the object."
  (setf (x p) (- (x p) scalar)
	(y p) (- (y p) scalar))
  p)

(defgeneric mul (p a))
(defmethod mul ((p Point2D) scalar)
  "Returns a new Point2D that equals this object whose coordinates have been multiplied by the specified scalar amount."
  (make-instance 'Point2D
		 :x (* (x p) scalar)
		 :y (* (y p) scalar)))

(defmethod mul= ((p Point2D) scalar)
  "Multiplies this object's coordinates by the specified amount. The object is returned to allow multiple point operations to be chained together."
  (setf (x p) (* (x p) scalar)
	(y p) (* (y p) scalar))
  p)

(defgeneric div (p a))
(defmethod div ((p Point2D) scalar)
  "Returns a new Point2D that equals this object whose coordinates have been divided by the specified scalar amount."
  (make-instance 'Point2D
		 :x (/ (x p) scalar)
		 :y (/ (y p) scalar)))

(defgeneric div= (p a))
(defmethod div= ((p Point2D) scalar)
  "Divides this object's coordinates by the specified amount. The object is returned to allow multiple point operations to be chained together."
  (setf (x p) (/ (x p) scalar)
	(y p) (/ (y p) scalar))
  p)

(defmethod equals ((p Point2D) (a Point2D))
  "Determines if this object's coordinates match the specified Point2D coordinates."
  (and (= (x p) (x a))
       (= (y p) (y a))))

(defgeneric lt (p a))
(defmethod lt ((p Point2D) (a Point2D))
  "Determines if any of this object's coordinates are less than the specified Point2D coordinates."
  (and (< (x p) (x a))
       (< (y p) (y a))))

(defgeneric gt (p a))
(defmethod gt ((p Point2D) (a Point2D))
  "Determines if any of this object's coordinates are greater than the specified Point2D coordinates."
  (and (> (x p) (x a))
       (> (y p) (y a))))

(defgeneric le (p a))
(defmethod le ((p Point2D) (a Point2D))
  "Determines if any of this object's coordinates are less than the specified Point2D coordinates or if all coordinates match."
  (and (<= (x p) (x a))
       (<= (y p) (y a))))

(defgeneric ge (p a))
(defmethod ge ((p Point2D) (a Point2D))
  "Determines if any of this object's coordinates are greater than the specified Point2D coordinates or if all coordinates match."
  (and (>= (x p) (x a))
       (>= (y p) (y a))))

(defmethod lerp ((p Point2D) (a Point2D) percent)
  "Performs a linear interpolation between the current Point2D and another Point2D based on the percentage passed to this method."
  (make-instance 'Point2D
		 :x (+ (x p) (* percent (- (x a) (x p))))
		 :y (+ (y p) (* percent (- (y a) (y p))))))

(defmethod distance ((p Point2D) (a Point2D))
  "Returns the shortest distance between this object and another Point2D."
  (let ((dx (- (x p) (x a)))
	(dy (- (y p) (y a))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defgeneric minimum (p a))
(defmethod minimum ((p Point2D) (a Point2D))
  "Returns a new Point2D whose coordinates are the minimum of each coordinate in this object and the specified Point2D."
  (make-instance 'Point2D
		 :x (min (x p) (x a))
		 :y (min (y p) (y a))))

(defgeneric maximum (p a))
(defmethod maximum ((p Point2D) (a Point2D))
  "Returns a new Point2D whose coordinates are the maximum of each coordinate in this object and the specified Point2D."
  (make-instance 'Point2D
		 :x (max (x p) (x a))
		 :y (max (y p) (y a))))

(defmethod setXY ((p Point2D) x y)
  "Sets the current object's coordinates to the specified values."
  (setf (x p) x
	(y p) y))

(defmethod setFromPoint ((p Point2D) (a Point2D))
  "Copies the specified point's coordinates to this object."
  (setf (x p) (x a)
	(y p) (y a)))

(defun point-xy (p)
  "Returns point's coordinates as cons."
  (cons (x p)
	(y p)))
