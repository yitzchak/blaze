(in-package #:blaze)

(defgeneric add (x y))

(defgeneric at (x i))

(defgeneric (setf at) (value x i))

(defgeneric finitep (x))

(defgeneric transpose (x))