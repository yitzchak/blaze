(k:includes #~"blaze/")

(k:systems :blaze)

(k:sources :iclasp #~"src/blaze.cc")

(k:sources :install-code
           #~"src/blaze.asd"
           #~"src/interface.lisp"
           #~"src/vector.lisp")
