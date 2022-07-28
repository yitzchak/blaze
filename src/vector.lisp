(in-package #:blaze)

(defmethod add ((x static-vector-double-3-col) (y static-vector-double-3-col))
  (static-vector-double-3-col/+ x y))

(defmethod print-object ((object static-vector-double-3-col) stream)
  (if *print-readably*
      (format stream "#3V(~a ~a ~a)" (at object 0) (at object 1) (at object 2))
      (print-unreadable-object (object stream :type t)
        (format stream "~a ~a ~a" (at object 0) (at object 1) (at object 2))))
  object)

(defmethod at ((x static-vector-double-3-col) i)
  (static-vector-double-3-col/at x i))

(defmethod (setf at) (value (x static-vector-double-3-col) i)
  (static-vector-double-3-col/setf-at value x i))

(defmethod nanp ((x static-vector-double-3-col))
  (static-vector-double-3-col/nanp x))

(defmethod infinitep ((x static-vector-double-3-col))
  (static-vector-double-3-col/infinitep x))

(defmethod finitep ((x static-vector-double-3-col))
  (static-vector-double-3-col/finitep x))

(defmethod defaultp ((x static-vector-double-3-col))
  (static-vector-double-3-col/defaultp x))

(defmethod uniformp ((x static-vector-double-3-col))
  (static-vector-double-3-col/uniformp x))

(defmethod zerop ((x static-vector-double-3-col))
  (static-vector-double-3-col/zerop x))

(defmethod transpose ((x static-vector-double-3-col))
  (static-vector-double-3-col/transpose x))

(defmethod norm ((x static-vector-double-3-col))
  (static-vector-double-3-col/norm x))

(defmethod sqr-norm ((x static-vector-double-3-col))
  (static-vector-double-3-col/sqr-norm x))

(defmethod sequence:length ((x static-vector-double-3-col))
  3)

(defmethod sequence:elt ((x static-vector-double-3-col) i)
  (static-vector-double-3-col/at x i))

(defmethod (setf sequence:elt) (value (x static-vector-double-3-col) i)
  (static-vector-double-3-col/setf-at value x i))

(defmethod add ((x static-vector-double-3-row) (y static-vector-double-3-row))
  (static-vector-double-3-row/+ x y))

(defmethod print-object ((object static-vector-double-3-row) stream)
  (if *print-readably*
      (format stream "#3V(~a ~a ~a)" (at object 0) (at object 1) (at object 2))
      (print-unreadable-object (object stream :type t)
        (format stream "~a ~a ~a" (at object 0) (at object 1) (at object 2))))
  object)

(defmethod at ((x static-vector-double-3-row) i)
  (static-vector-double-3-row/at x i))

(defmethod (setf at) (value (x static-vector-double-3-row) i)
  (static-vector-double-3-row/setf-at value x i))

(defmethod nanp ((x static-vector-double-3-row))
  (static-vector-double-3-row/nanp x))

(defmethod infinitep ((x static-vector-double-3-row))
  (static-vector-double-3-row/infinitep x))

(defmethod finitep ((x static-vector-double-3-row))
  (static-vector-double-3-row/finitep x))

(defmethod defaultp ((x static-vector-double-3-row))
  (static-vector-double-3-row/defaultp x))

(defmethod uniformp ((x static-vector-double-3-row))
  (static-vector-double-3-row/uniformp x))

(defmethod zerop ((x static-vector-double-3-row))
  (static-vector-double-3-row/zerop x))

(defmethod transpose ((x static-vector-double-3-row))
  (static-vector-double-3-row/transpose x))

(defmethod norm ((x static-vector-double-3-row))
  (static-vector-double-3-row/norm x))

(defmethod sqr-norm ((x static-vector-double-3-row))
  (static-vector-double-3-row/sqr-norm x))

(defmethod sequence:length ((x static-vector-double-3-row))
  3)


(defmethod add ((x dynamic-column-vector/double) (y dynamic-column-vector/double))
  (dynamic-column-vector/double/+ x y))

(defmethod print-object ((object dynamic-column-vector/double) stream)
  (if *print-readably*
      (loop for i below (length object)
            finally (write-char #\) stream)
            when (cl:zerop i)
              do (write-string "#V(" stream)
            else
              do (write-char #\Space stream)
            do (write (elt object i) :stream stream))
      (print-unreadable-object (object stream :type t)
        (loop for i below (length object)
              unless (cl:zerop i)
                do (write-char #\Space stream)
              do (write (elt object i) :stream stream))))
  object)

(defmethod at ((x dynamic-column-vector/double) i)
  (dynamic-column-vector/double/at x i))

(defmethod (setf at) (value (x dynamic-column-vector/double) i)
  (dynamic-column-vector/double/setf-at value x i))

(defmethod nanp ((x dynamic-column-vector/double))
  (dynamic-column-vector/double/nanp x))

(defmethod infinitep ((x dynamic-column-vector/double))
  (dynamic-column-vector/double/infinitep x))

(defmethod finitep ((x dynamic-column-vector/double))
  (dynamic-column-vector/double/finitep x))

(defmethod defaultp ((x dynamic-column-vector/double))
  (dynamic-column-vector/double/defaultp x))

(defmethod uniformp ((x dynamic-column-vector/double))
  (dynamic-column-vector/double/uniformp x))

(defmethod zerop ((x dynamic-column-vector/double))
  (dynamic-column-vector/double/zerop x))

(defmethod transpose ((x dynamic-column-vector/double))
  (dynamic-column-vector/double/transpose x))

(defmethod norm ((x dynamic-column-vector/double))
  (dynamic-column-vector/double/norm x))

(defmethod sqr-norm ((x dynamic-column-vector/double))
  (dynamic-column-vector/double/sqr-norm x))

(defmethod sequence:length ((x dynamic-column-vector/double))
  (dynamic-column-vector/double/length x))

(defmethod sequence:elt ((x dynamic-column-vector/double) i)
  (dynamic-column-vector/double/at x i))

(defmethod (setf sequence:elt) (value (x dynamic-column-vector/double) i)
  (dynamic-column-vector/double/setf-at value x i))


(defmethod add ((x dynamic-row-vector/double) (y dynamic-row-vector/double))
  (dynamic-row-vector/double/+ x y))

(defmethod print-object ((object dynamic-row-vector/double) stream)
  (if *print-readably*
      (loop for i below (length object)
            finally (write-char #\) stream)
            when (cl:zerop i)
              do (write-string "#V(" stream)
            else
              do (write-char #\Space stream)
            do (write (elt object i) :stream stream))
      (print-unreadable-object (object stream :type t)
        (loop for i below (length object)
              unless (cl:zerop i)
                do (write-char #\Space stream)
              do (write (elt object i) :stream stream))))
  object)

(defmethod at ((x dynamic-row-vector/double) i)
  (dynamic-row-vector/double/at x i))

(defmethod (setf at) (value (x dynamic-row-vector/double) i)
  (dynamic-row-vector/double/setf-at value x i))

(defmethod nanp ((x dynamic-row-vector/double))
  (dynamic-row-vector/double/nanp x))

(defmethod infinitep ((x dynamic-row-vector/double))
  (dynamic-row-vector/double/infinitep x))

(defmethod finitep ((x dynamic-row-vector/double))
  (dynamic-row-vector/double/finitep x))

(defmethod defaultp ((x dynamic-row-vector/double))
  (dynamic-row-vector/double/defaultp x))

(defmethod uniformp ((x dynamic-row-vector/double))
  (dynamic-row-vector/double/uniformp x))

(defmethod zerop ((x dynamic-row-vector/double))
  (dynamic-row-vector/double/zerop x))

(defmethod transpose ((x dynamic-row-vector/double))
  (dynamic-row-vector/double/transpose x))

(defmethod norm ((x dynamic-row-vector/double))
  (dynamic-row-vector/double/norm x))

(defmethod sqr-norm ((x dynamic-row-vector/double))
  (dynamic-row-vector/double/sqr-norm x))

(defmethod sequence:length ((x dynamic-row-vector/double))
  (dynamic-row-vector/double/length x))

(defmethod sequence:elt ((x dynamic-row-vector/double) i)
  (dynamic-row-vector/double/at x i))

(defmethod (setf sequence:elt) (value (x dynamic-row-vector/double) i)
  (dynamic-row-vector/double/setf-at value x i))

(defmethod multiply ((x dynamic-row-vector/double) (y dynamic-column-vector/double))
  (multiply/drd/dcd x y))

(defmethod multiply ((x dynamic-column-vector/double) (y dynamic-row-vector/double))
  (multiply/dcd/drd x y))

  