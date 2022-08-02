(in-package #:blaze)

(shadow '("ZEROP" "+" "*"))

(defgeneric add (x y))

(defgeneric multiply (x y))

(defgeneric nanp (x))

(defgeneric infinitep (x))

(defgeneric finitep (x))

(defgeneric defaultp (x))

(defgeneric uniformp (x))

(defgeneric zerop (x))

(defgeneric transpose (x))

(defgeneric norm (x))

(defgeneric sqr-norm (x))

(defun + (&rest args)
  (cond ((cdr args)
         (reduce #'add args))
        (args
         (car args))
        (t
         0)))

(defun - (&rest args)
  (cond ((cdr args)
         (reduce #'subtract args))
        (args
         (negate (car args)))
        (t
         0)))

(defun * (&rest args)
  (cond ((null args)
         1)
        ((null (cdr args))
         (car args))
        (t
         (reduce #'multiply args))))

(export '(nanp infinitep finitep defaultp uniformp zerop transpose norm sqr-norm + *))
