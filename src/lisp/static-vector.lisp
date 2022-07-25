(in-package #:blaze)

(defmethod add ((x blaze/lib:static-vector-double-3-col) (y blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/+ x y))

(defmethod print-object ((object blaze/lib:static-vector-double-3-col) stream)
  (if *print-readably*
      (format stream "#3V(~a ~a ~a)" (at object 0) (at object 1) (at object 2))
      (print-unreadable-object (object stream :type t)
        (format stream "~a ~a ~a" (at object 0) (at object 1) (at object 2))))
  object)

(defmethod at ((x blaze/lib:static-vector-double-3-col) i)
  (blaze/lib:static-vector-double-3-col/at x i))

(defmethod (setf at) (value (x blaze/lib:static-vector-double-3-col) i)
  (blaze/lib:static-vector-double-3-col/setf-at value x i))

(defmethod nanp ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/nanp x))

(defmethod infinitep ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/infinitep x))

(defmethod finitep ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/finitep x))

(defmethod defaultp ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/defaultp x))

(defmethod uniformp ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/uniformp x))

(defmethod zerop ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/zerop x))

(defmethod transpose ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/transpose x))

(defmethod norm ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/norm x))

(defmethod sqr-norm ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/sqr-norm x))

(defmethod sequence:length ((x blaze/lib:static-vector-double-3-col))
  3)

(defmethod sequence:elt ((x blaze/lib:static-vector-double-3-col) i)
  (blaze/lib:static-vector-double-3-col/at x i))

(defmethod (setf sequence:elt) (value (x blaze/lib:static-vector-double-3-col) i)
  (blaze/lib:static-vector-double-3-col/setf-at value x i))

(defmethod add ((x blaze/lib:static-vector-double-3-row) (y blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/+ x y))

(defmethod print-object ((object blaze/lib:static-vector-double-3-row) stream)
  (if *print-readably*
      (format stream "#3V(~a ~a ~a)" (at object 0) (at object 1) (at object 2))
      (print-unreadable-object (object stream :type t)
        (format stream "~a ~a ~a" (at object 0) (at object 1) (at object 2))))
  object)

(defmethod at ((x blaze/lib:static-vector-double-3-row) i)
  (blaze/lib:static-vector-double-3-row/at x i))

(defmethod (setf at) (value (x blaze/lib:static-vector-double-3-row) i)
  (blaze/lib:static-vector-double-3-row/setf-at value x i))

(defmethod nanp ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/nanp x))

(defmethod infinitep ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/infinitep x))

(defmethod finitep ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/finitep x))

(defmethod defaultp ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/defaultp x))

(defmethod uniformp ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/uniformp x))

(defmethod zerop ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/zerop x))

(defmethod transpose ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/transpose x))

(defmethod norm ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/norm x))

(defmethod sqr-norm ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/sqr-norm x))

(defmethod sequence:length ((x blaze/lib:static-vector-double-3-row))
  3)


(defmethod add ((x blaze/lib:dynamic-column-vector/double) (y blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/+ x y))

(defmethod print-object ((object blaze/lib:dynamic-column-vector/double) stream)
  (if *print-readably*
      (format stream "#3V(~a ~a ~a)" (at object 0) (at object 1) (at object 2))
      (print-unreadable-object (object stream :type t)
        (format stream "~a ~a ~a" (at object 0) (at object 1) (at object 2))))
  object)

(defmethod at ((x blaze/lib:dynamic-column-vector/double) i)
  (blaze/lib:dynamic-column-vector/double/at x i))

(defmethod (setf at) (value (x blaze/lib:dynamic-column-vector/double) i)
  (blaze/lib:dynamic-column-vector/double/setf-at value x i))

(defmethod nanp ((x blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/nanp x))

(defmethod infinitep ((x blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/infinitep x))

(defmethod finitep ((x blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/finitep x))

(defmethod defaultp ((x blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/defaultp x))

(defmethod uniformp ((x blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/uniformp x))

(defmethod zerop ((x blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/zerop x))

(defmethod transpose ((x blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/transpose x))

(defmethod norm ((x blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/norm x))

(defmethod sqr-norm ((x blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/sqr-norm x))

(defmethod sequence:length ((x blaze/lib:dynamic-column-vector/double))
  (blaze/lib:dynamic-column-vector/double/length x))

(defmethod sequence:elt ((x blaze/lib:dynamic-column-vector/double) i)
  (blaze/lib:dynamic-column-vector/double/at x i))

(defmethod (setf sequence:elt) (value (x blaze/lib:dynamic-column-vector/double) i)
  (blaze/lib:dynamic-column-vector/double/setf-at value x i))


  