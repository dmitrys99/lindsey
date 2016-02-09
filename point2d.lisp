(in-package #:lindsey)

(defclass Point2D ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defun new-point2d (x y)
  (make-instance 'Point2D :x x :y y))

;; TODO DRY for similar methods

(defmethod print-object ((p Point2D) stream)
  (format stream "Point2D <~A, ~A>" (x p) (y p)))

(defmethod clone ((p Point2D))
  (make-instance 'Point2D :x (x p) :y (y p)))

(defmethod rmoveto ((p Point2D) dx dy)
  (setf (x p) (+ (x p) dx)
	(y p) (+ (y p) dy)))

(defgeneric add (p a))
(defmethod add ((p Point2D) (a Point2D))
  (make-instance 'Point2D
		 :x (+ (x p) (x a))
		 :y (+ (y p) (y a))))

(defmethod add ((p Point2D) scalar)
  (make-instance 'Point2D
		 :x (+ (x p) scalar)
		 :y (+ (y p) scalar)))

(defgeneric add= (p a))
(defmethod add= ((p Point2D) (a Point2D))
  (setf (x p) (+ (x p) (x a))
	(y p) (+ (y p) (y a)))
  p)

(defmethod add= ((p Point2D) scalar)
  (setf (x p) (+ (x p) scalar)
	(y p) (+ (y p) scalar))
  p)

(defgeneric sub (p a))
(defmethod sub ((p Point2D) (a Point2D))
  (make-instance 'Point2D
		 :x (- (x p) (x a))
		 :y (- (y p) (y a))))

(defmethod sub ((p Point2D) scalar)
  (make-instance 'Point2D
		 :x (- (x p) scalar)
		 :y (- (y p) scalar)))

(defgeneric sub= (p a))
(defmethod sub= ((p Point2D) (a Point2D))
  (setf (x p) (- (x p) (x a))
	(y p) (- (y p) (y a)))
  p)

(defmethod sub= ((p Point2D) scalar)
  (setf (x p) (- (x p) scalar)
	(y p) (- (y p) scalar))
  p)

(defgeneric mul (p a))
(defmethod mul ((p Point2D) scalar)
  (make-instance 'Point2D
		 :x (* (x p) scalar)
		 :y (* (y p) scalar)))

(defmethod mul= ((p Point2D) scalar)
  (setf (x p) (* (x p) scalar)
	(y p) (* (y p) scalar))
  p)

(defgeneric div (p a))
(defmethod div ((p Point2D) scalar)
  (make-instance 'Point2D
		 :x (/ (x p) scalar)
		 :y (/ (y p) scalar)))

(defgeneric div= (p a))
(defmethod div= ((p Point2D) scalar)
  (setf (x p) (/ (x p) scalar)
	(y p) (/ (y p) scalar))
  p)

(defgeneric lt (p a))
(defmethod lt ((p Point2D) (a Point2D))
  (and (< (x p) (x a))
       (< (y p) (y a))))

(defgeneric gt (p a))
(defmethod gt ((p Point2D) (a Point2D))
  (and (> (x p) (x a))
       (> (y p) (y a))))

(defgeneric le (p a))
(defmethod le ((p Point2D) (a Point2D))
  (and (<= (x p) (x a))
       (<= (y p) (y a))))

(defgeneric ge (p a))
(defmethod ge ((p Point2D) (a Point2D))
  (and (>= (x p) (x a))
       (>= (y p) (y a))))

(defmethod lerp ((p Point2D) (a Point2D) tt)
  (make-instance 'Point2D
		 :x (+ (x p) (* tt (- (x a) (x p))))
		 :y (+ (y p) (* tt (- (y a) (y p))))))

(defmethod distance ((p Point2D) (a Point2D))
  (let ((dx (- (x p) (x a)))
	(dy (- (y p) (y a))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defgeneric minimum (p a))
(defmethod minimum ((p Point2D) (a Point2D))
  (make-instance 'Point2D
		 :x (min (x p) (x a))
		 :y (min (y p) (y a))))

(defgeneric maximum (p a))
(defmethod maximum ((p Point2D) (a Point2D))
  (make-instance 'Point2D
		 :x (max (x p) (x a))
		 :y (max (y p) (y a))))

(defmethod setXY ((p Point2D) x y)
  (setf (x p) x
	(y p) y))

(defmethod setFromPoint ((p Point2D) (a Point2D))
  (setf (x p) (x a)
	(y p) (y a)))

(defun point-xy (p)
  (cons (x p)
	(y p)))
