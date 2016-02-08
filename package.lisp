(defpackage #:lindsey
  (:use #:cl)

  (:shadow
   #:+
   #:-
   #:/
   #:*
   #:/=

   #:<
   #:>
   #:<=
   #:>=

   #:min
   #:max)

  (:export
   #:+
   #:-
   #:/
   #:*
   #:/=

   #:<
   #:>
   #:<=
   #:>=
   
   #:min
   #:max
   #:lerp
   #:clone
   #:rmoveto
   #:distance
   ))
