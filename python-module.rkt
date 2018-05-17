(module python-module racket
  (provide
   
   (all-from-out "./python-extras.rkt")
   (all-from-out "./python.rkt")
   #;(rename-out [py-begin begin]
               [py-set set!])
   #;(rename-out [define-user-function define])
   #;(except-out (all-from-out racket)
               begin
               define
               set!)
   #%module-begin)


  (require "./python.rkt")
  (require "./python-extras.rkt"))




