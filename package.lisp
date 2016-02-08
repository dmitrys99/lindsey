(defpackage #:lindsey
  (:use #:cl)

  (:shadow
   #:+ #:- #:/ #:* #:/= #:< #:> #:<= #:>=

   #:min #:max #:length)

  (:export
   #:+ #:- #:/ #:* #:/= #:< #:> #:<= #:>=
   
   #:min #:max #:lerp #:clone #:rmoveto #:distance
   #:length))
