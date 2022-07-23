(k:includes #~"blaze/")

(k:systems :blaze)

(k:sources :iclasp #~"src/blaze.cc")

(k:sources :install-code
           #~"src/lisp/blaze.asd"
           #~"src/lisp/packages.lisp"
           #~"src/lisp/interface.lisp"
           #~"src/lisp/static-vector.lisp")
