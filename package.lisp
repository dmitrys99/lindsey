(defpackage #:lindsey
  (:use #:cl)

  (:export

   #:Point2D
   #:new-point2d
   #:point-xy
   #:x
   #:y
   
   #:Vector2D
   #:new-vector2d
   #:vector-xy

   #:Polynomial

   #:Intersection
   #:bezier3-line-intersection
   #:line-line-intersection))
