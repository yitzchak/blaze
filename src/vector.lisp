(in-package #:blaze)

(defmethod print-object ((object dense-real-column-vector-double) stream)
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

(defmethod print-object ((object dense-real-row-vector-double) stream)
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

(defmethod print-object ((object dense-complex-column-vector-double) stream)
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

(defmethod print-object ((object dense-complex-row-vector-double) stream)
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

(defmethod nanp ((x dense-real-column-vector-double))
  (nanp@drcv2 x))

(defmethod nanp ((x dense-real-row-vector-double))
  (nanp@drrv2 x))

(defmethod infinitep ((x dense-real-column-vector-double))
  (infinitep@drcv2 x))

(defmethod infinitep ((x dense-real-row-vector-double))
  (infinitep@drrv2 x))

(defmethod finitep ((x dense-real-column-vector-double))
  (finitep@drcv2 x))

(defmethod finitep ((x dense-real-row-vector-double))
  (finitep@drrv2 x))

(defmethod defaultp ((x dense-real-column-vector-double))
  (defaultp@drcv2 x))

(defmethod defaultp ((x dense-real-row-vector-double))
  (defaultp@drrv2 x))

(defmethod uniformp ((x dense-real-column-vector-double))
  (uniformp@drcv2 x))

(defmethod uniformp ((x dense-real-row-vector-double))
  (uniformp@drrv2 x))

(defmethod zerop ((x dense-real-column-vector-double))
  (zerop@drcv2 x))

(defmethod zerop ((x dense-real-row-vector-double))
  (zerop@drrv2 x))

(defmethod transpose ((x dense-real-column-vector-double))
  (transpose@drcv2 x))

(defmethod transpose ((x dense-real-row-vector-double))
  (transpose@drrv2 x))

(defmethod norm ((x dense-real-column-vector-double))
  (norm@drcv2 x))

(defmethod norm ((x dense-real-row-vector-double))
  (norm@drrv2 x))

(defmethod sqr-norm ((x dense-real-column-vector-double))
  (sqr-norm@drcv2 x))

(defmethod sqr-norm ((x dense-real-row-vector-double))
  (sqr-norm@drrv2 x))

(defmethod sequence:length ((x dense-real-column-vector-double))
  (length@drcv2 x))

(defmethod sequence:length ((x dense-real-row-vector-double))
  (length@drrv2 x))

(defmethod sequence:length ((x dense-complex-column-vector-double))
  (length@dccv2 x))

(defmethod sequence:length ((x dense-complex-row-vector-double))
  (length@dcrv2 x))

(defmethod sequence:elt ((x dense-real-column-vector-double) i)
  (elt@drcv2 x i))

(defmethod sequence:elt ((x dense-real-row-vector-double) i)
  (elt@drrv2 x i))

(defmethod sequence:elt ((x dense-complex-column-vector-double) i)
  (elt@dccv2 x i))

(defmethod sequence:elt ((x dense-complex-row-vector-double) i)
  (elt@dcrv2 x i))

(defmethod (setf sequence:elt) (value (x dense-real-column-vector-double) i)
  (setf-elt@drcv2 value x i))

(defmethod (setf sequence:elt) (value (x dense-real-row-vector-double) i)
  (setf-elt@drrv2 value x i))

(defmethod (setf sequence:elt) (value (x dense-complex-column-vector-double) i)
  (setf-elt@dccv2 value x i))

(defmethod (setf sequence:elt) (value (x dense-complex-row-vector-double) i)
  (setf-elt@dcrv2 value x i))

(defmethod add ((x dense-real-column-vector-double) (y dense-real-column-vector-double))
  (add@drcv2@drcv2 x y))

(defmethod add ((x dense-real-row-vector-double) (y dense-real-row-vector-double))
  (add@drrv2@drrv2 x y))

(defmethod add ((x dense-complex-column-vector-double) (y dense-real-column-vector-double))
  (add@dccv2@drcv2 x y))

(defmethod add ((x dense-complex-row-vector-double) (y dense-real-row-vector-double))
  (add@dcrv2@drrv2 x y))

(defmethod add ((x dense-real-column-vector-double) (y dense-complex-column-vector-double))
  (add@dccv2@drcv2 x y))

(defmethod add ((x dense-real-row-vector-double) (y dense-complex-row-vector-double))
  (add@dcrv2@drrv2 x y))

(defmethod add ((x dense-complex-column-vector-double) (y dense-complex-column-vector-double))
  (add@dccv2@dccv2 x y))

(defmethod add ((x dense-complex-row-vector-double) (y dense-complex-row-vector-double))
  (add@dcrv2@dcrv2 x y))

(defmethod multiply ((x number) (y dense-real-column-vector-double))
  (multiply@2@drcv2 x y))

(defmethod multiply ((x dense-real-column-vector-double) (y number))
  (multiply@2@drcv2 x y))

(defmethod multiply ((x number) (y dense-real-row-vector-double))
  (multiply@2@drrv2 x y))

(defmethod multiply ((x dense-real-row-vector-double) (y number))
  (multiply@2@drrv2 x y))

(defmethod multiply ((x dense-real-row-vector-double) (y dense-real-column-vector-double))
  (multiply@drrv2@drcv2 x y))

(defmethod multiply ((x dense-real-column-vector-double) (y dense-real-row-vector-double))
  (multiply@drcv2@drrv2 x y))

  
