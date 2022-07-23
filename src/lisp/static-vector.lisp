(in-package #:blaze)

(defmethod add ((x blaze/lib:static-vector-double-3-col) (y blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/+ x y))

(defmethod print-object ((object blaze/lib:static-vector-double-3-col) stream)
  (format stream "#3V(~a ~a ~a)" (at object 0) (at object 1) (at object 2))
  object)

(defmethod at ((x blaze/lib:static-vector-double-3-col) i)
  (blaze/lib:static-vector-double-3-col/at x i))

(defmethod (setf at) (value (x blaze/lib:static-vector-double-3-col) i)
  (blaze/lib:static-vector-double-3-col/setf-at value x i))

(defmethod finitep ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/finitep x))

(defmethod transpose ((x blaze/lib:static-vector-double-3-col))
  (blaze/lib:static-vector-double-3-col/transpose x))


(defmethod add ((x blaze/lib:static-vector-double-3-row) (y blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/+ x y))

(defmethod print-object ((object blaze/lib:static-vector-double-3-row) stream)
  (format stream "#3V(~a ~a ~a)" (at object 0) (at object 1) (at object 2))
  object)

(defmethod at ((x blaze/lib:static-vector-double-3-row) i)
  (blaze/lib:static-vector-double-3-row/at x i))

(defmethod (setf at) (value (x blaze/lib:static-vector-double-3-row) i)
  (blaze/lib:static-vector-double-3-row/setf-at value x i))

(defmethod finitep ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/finitep x))

(defmethod transpose ((x blaze/lib:static-vector-double-3-row))
  (blaze/lib:static-vector-double-3-row/transpose x))    