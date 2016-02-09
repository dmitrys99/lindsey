(in-package #:lindsey)

(defclass Intersection ()
  ((status :accessor status :initarg :status
	   :initform "")
   (points :accessor points :initarg :points
	   :initform (make-array 4
				 :element-type 'Point2D
				 :fill-pointer 0))))

(defmethod appendPoint ((i Intersection) p)
  (vector-push p (points i)))

(defmethod intersect-bezier3-line ((i Intersection)
				   (p1 Point2D)
				   (p2 Point2D)
				   (p3 Point2D)
				   (p4 Point2D)
				   (a1 Point2D)
				   (a2 Point2D))
  (let* ((result (make-instance 'Intersection
				:status "No intersection"))
	 (minimum (minimum a1 a2))
	 (maximum (maximum a1 a2))
	 (a (mul p1 -1))
	 (b (mul p2 3))
	 (c (mul p3 -3))
	 (d (add a (add b (add c p4))))
	 (c3) (c2) (c1) (c0)
	 (cl)
	 (n)
	 (roots)
	 (cfs)
	 (poly))
    (setf c3 (new-vector2d (x d) (y d)))
    ;; (break "a:~A b:~A c:~A d:~A" a b c d)
    ;; (break "~A" c3)
    (setf a (mul p1 3)
	  b (mul p2 -6)
	  c (mul p3 3)
	  d (add a (add b c))
	  c2 (new-vector2d (x d) (y d)))

    (setf a (mul p1 -3)
	  b (mul p2 3)
	  c (add a b)
	  c1 (new-vector2d (x c) (y c))

	  c0 (new-vector2d (x p1) (y p1))

	  n (new-vector2d (- (y a1) (y a2))
			  (- (x a2) (x a1)))
	  cl (- (* (x a1) (y a2))
		(* (x a2) (y a1))))
    (setf cfs (make-array 4
			  :initial-contents
			  (list (dot n c3)
				(dot n c2)
				(dot n c1)
				(+ (dot n c0) cl)))
	  poly (make-instance 'Polynomial
			      :coefs cfs)
	  roots (getRoots poly))
    ;; (break "c0: ~A~%c1: ~A~%c2: ~A~%c3: ~A~%n: ~A~%cl: ~A~%" c0 c1 c2 c3 n cl)
    ;; (break "~A" cfs)
    ;; (break "~A" roots)
    (loop for tt across roots
       do (let* ((p5 (lerp p1 p2 tt))
		 (p6 (lerp p2 p3 tt))
		 (p7 (lerp p3 p4 tt))

		 (p8 (lerp p5 p6 tt))
		 (p9 (lerp p6 p7 tt))

		 (p0 (lerp p8 p9 tt)))
	    (cond
	      
	      ((= (x a1) (x a2))
	       (when (and (<= (y minimum)
			      (y p0))
			  (<= (y p0)
			      (y maximum)))
		 (setf (status result) "Intersection")
		 (appendPoint result p0)))

	      ((= (y a1) (y a2))
	       (when (and (<= (x minimum)
			      (x p0))
			  (<= (x p0)
			      (x maximum)))
		 (setf (status result) "Intersection")
		 (appendPoint result p0)))

	      (t
	       (when (and (ge p0 minimum)
			  (le p0 maximum))
		 (setf (status result) "Intersection")
		 (appendPoint result p0))))))
    result))

(defun bezier3-line-intersection (p1 p2 p3 p4 a1 a2)
  (let* ((i (make-instance 'Intersection))
	 (j (intersect-bezier3-line i p1 p2 p3 p4 a1 a2)))
    (when(string/= (status j) "No intersection")
      (points j))))
