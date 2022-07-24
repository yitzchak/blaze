(defpackage :blaze
  (:use :common-lisp)
  (:shadow #+*
           #:+
           #:zerop)
  (:export #:*
           #:+
           #:at
           #:defaultp
           #:finitep
           #:infinitep
           #:nanp
           #:norm
           #:sqr-norm
           #:transpose
           #:uniformp
           #:zerop))
