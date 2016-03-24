(in-package #:lindsey)

(defconstant TOLERANCE 1e-6 "TOLERANCE is a number used to determine if a number is close enough to zero to be considered zero.")
(defconstant ACCURACY 6 "ACCURACY is an integer that is used to determine the minimum number of digits of accuracy to be returned by a result.")

(defclass Polynomial ()
  ((coefs :initarg :coefs
	  :accessor coefs
	  :documentation "Each number corresponds to a monomial in the polynomial. The position of the coefficient in the array corresponds to the degree of the monomial to which it belongs.")
   (var   :initarg :var
	  :accessor var
	  :initform "t")
   (s     :initarg :s
          :accessor s))
  (:documentation "Polynomial is an object used to manipulate polynomials.
This object was created to encapsulate root finding functions needed by the intersection methods in the Intersection object.

getCubicRoots\(\) and getQuarticRoots\(\) was derived from David Eberly's excellent game engine source which can be found at his site. If you are at all interested in 3D gaming algorithms, then I highly recommend his \"3D Game Engine Design\" book \(ISBN 1-55860-593-2\)."))

(defmethod init ((p Polynomial) cfs)
  "Initializes all properties for this object.

One or more coefficients initialize the Polynomial.
The coefficients are in order by highest degree monomial first.
For example, the following example initializes a Polynomial object for:

3x^4 + 2x^2 + 5:

\(init p 3 0 2 0 5\)

All coefficients from highest degree to degree 0 must be provided.
A zero is used for monomials that are not present in the polynomial.

NOTE: The polynomial coefficients are stored in an array in the reverse order
to how they were specified. This has the benefit that the coefficient's position
in the array corresponds to the degree of the monomial to which it belongs."
  (setf (coefs p) (reverse (copy-seq cfs))))

(defmethod initialize-instance :after ((p Polynomial) &key)
  "Constructor method"
  (init p (coefs p)))

(defmethod evaluate ((p Polynomial) x)
  "Evalutes the polynomial at the specified x value.
x is a number that is \"plugged into\" the polynomial to evaluate it."
  (assert (numberp x) nil "Polynomial evaluate: parameter must be a number.")
  (let ((result 0))
    (loop for i across (coefs p)
       do (setf result (+ (* result x) i)))
    result))

(defmethod degree ((p Polynomial))
  "Returns the degree of the polynomial represented by the Polynomial object."
  (1- (length (coefs p))))

(defmethod getCubicRoots ((p Polynomial))
  "Returns the roots of a cubic polynomial (degree equals three)."
  (let ((results (make-array 3 :element-type 'float :fill-pointer 0)))
    (when (= 3 (degree p))
      (let* ((c3 (elt (coefs p) 3))
	     (c2 (/ (elt (coefs p) 2) c3))
	     (c1 (/ (elt (coefs p) 1) c3))
	     (c0 (/ (elt (coefs p) 0) c3))
	     (a  (/ (- (* c1 3) (* c2 c2)) 3))
	     (b  (/ (+ (* 2 c2 c2 c2)
			     (- (* 9 c1 c2))
			     (* 27 c0)) 27))
	     (offset  (/ c2 3))
	     (discrim (+ (/ (* b b) 4)
			    (/ (* a a a) 27)))
	     (halfB (/ b 2)))

	;; (break "c0: ~G~%c1: ~G~%c2: ~G~%c3: ~G~%a: ~G~%b: ~G~%offset: ~G~%discrim: ~G~%"  c0 c1 c2 c3 a b offset discrim)
	(when (<= (abs discrim) TOLERANCE)
	  (setf discrim 0))
	(cond
	  ((> discrim 0)
	   (let* ((e (sqrt discrim))
		  (tmp (+ (- halfB) e))
		  (root (if (>= tmp 0)
			    (expt tmp 1/3)
			    (- (expt (- tmp) 1/3)))))
	     (setf tmp (+ (- halfB) (- e)))
	     (if (>= tmp 0)
		 (incf root (expt tmp 1/3))
		 (decf root (expt (- tmp) 1/3)))
	     (vector-push (- root offset) results)))
	  ((< discrim 0)
	   (let* ((distance (sqrt (/ (- a) 3)))
		  (angle (/
			  (atan (sqrt (- discrim))
				(- halfB))
			  3))
		  (cos (cos angle))
		  (sin (sin angle))
		  (sqrt3 (sqrt 3)))
	     ;;(break "distance: ~F~%angle: ~F~% cos: ~F~% sin: ~F~%sqrt3: ~F" distance angle cos sin sqrt3)
	     (vector-push (- (* 2 distance cos) offset) results)
	     (vector-push (- (* (- distance)
				(+ cos (* sqrt3 sin))) offset) results)
	     (vector-push (- (* (- distance)
				(- cos (* sqrt3 sin))) offset) results)))
	  (t (let ((tmp (if (>= halfB 0)
			    (- (expt halfB  1/3))
			    (expt (- halfB) 1/3))))
	       (vector-push (- (* 2 tmp) offset) results)
	       (vector-push (- (- tmp) offset) results))))
	results))))

(defmethod getLinearRoot ((p Polynomial))
  "Returns the root of a linear polynomial (degree equals one)."
  (let ((results (make-array 3 :element-type 'float :fill-pointer 0))
	(a (elt (coefs p) 1)))
    (when (/= 0 a)
      (vector-push (/ (- (elt (coefs p) 0)) a) results))
    results))

(defmethod getQuadraticRoots ((p Polynomial))
  "Returns the roots of a quadratic polynomial (degree equals two)."
  (let ((results (make-array 3 :element-type 'float :fill-pointer 0)))
    (when (= (degree p) 2)
      (let* ((a (elt (coefs p) 2))
	     (b (/ (elt (coefs p) 1) a))
	     (c (/ (elt (coefs p) 0) a))
	     (d (- (* b b) (* 4 c))))
	(cond
	  ((> d 0)
	   (let ((e (sqrt d)))
	     (vector-push (* 0.5 (+ (- b) e)) results)
	     (vector-push (* 0.5 (- (- b) e)) results)))
	  ((= d 0)
	   (vector-push (* 0.5 (- b)) results)))))
    results))

(defmethod simplify ((p Polynomial))
  "Simplify reduces the degree of the polynomial.
Each monomial, highest degree first, is visited.
If the absolute value of the monomial's coefficient is below a certain value,
then the monomial is removed from the polynomial.
This method is called by the getRoots() method."
  (loop for i from (degree p) downto 0
     do (if (<= (abs (elt (coefs p) i)) TOLERANCE)
	    (vector-pop (coefs p))
	    (return))))

(defmethod getRoots ((p Polynomial))
  "getRoots attempts to find the roots of the current polynomial.
This method will attempt to decrease the degree of the polynomial using the simplify method.
Once the degree is determined, getRoots() dispatches the appropriate root-finding
method for the degree of the polynomial.

NOTE: At this time, polynomials above the 4'th degree are not supported."
  (simplify p)
  (case (degree p)
    (0 #())
    (1 (getLinearRoot p))
    (2 (getQuadraticRoots p))
    (3 (getCubicRoots p))
    (otherwise #())))
