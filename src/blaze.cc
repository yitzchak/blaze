#include <clasp/clasp.h>
#include <clasp/core/translators.h>
#include <blaze/Blaze.h>

PACKAGE_USE("COMMON-LISP");
NAMESPACE_PACKAGE_ASSOCIATION(blaze_lib, blaze_lib_pkg, "BLAZE/LIB");

namespace blaze_lib {

typedef blaze::StaticVector<double, 3UL, blaze::columnVector> static_vector_double_3_col;
typedef blaze::StaticVector<double, 3UL, blaze::rowVector> static_vector_double_3_row;
typedef blaze::DynamicVector<double, blaze::columnVector> dynamic_column_vector_double;
typedef blaze::DynamicVector<double, blaze::rowVector> dynamic_row_vector_double;

CL_EXPOSE void blaze_startup() {
  clbind::package_ pkg(blaze_lib_pkg);

  clbind::class_<static_vector_double_3_col>(pkg, "static-vector-double-3-col")
      .def_constructor("static-vector-double-3-col/make", clbind::constructor<>())
      .def_constructor("static-vector-double-3-col/copy", clbind::constructor<const static_vector_double_3_col &>());

  pkg.def(
      "static-vector-double-3-col/+",
      +[](const static_vector_double_3_col &x, const static_vector_double_3_col &y) { return blaze::evaluate(x + y); });
  pkg.def(
      "static-vector-double-3-col/-",
      +[](const static_vector_double_3_col &x, const static_vector_double_3_col &y) { return blaze::evaluate(x - y); });
  pkg.def(
      "static-vector-double-3-col/at", +[](const static_vector_double_3_col &x, size_t i) { return x[i]; });
  pkg.def(
      "static-vector-double-3-col/setf-at", +[](double value, static_vector_double_3_col &x, size_t i) { return x[i] = value; });
  pkg.def(
      "static-vector-double-3-col/nanp", +[](const static_vector_double_3_col &x) { return blaze::isnan(x); });
  pkg.def(
      "static-vector-double-3-col/infinitep", +[](const static_vector_double_3_col &x) { return blaze::isinf(x); });
  pkg.def(
      "static-vector-double-3-col/finitep", +[](const static_vector_double_3_col &x) { return blaze::isfinite(x); });
  pkg.def(
      "static-vector-double-3-col/defaultp", +[](const static_vector_double_3_col &x) { return blaze::isDefault(x); });
  pkg.def(
      "static-vector-double-3-col/uniformp", +[](const static_vector_double_3_col &x) { return blaze::isUniform(x); });
  pkg.def(
      "static-vector-double-3-col/zerop", +[](const static_vector_double_3_col &x) { return blaze::isZero(x); });
  pkg.def(
      "static-vector-double-3-col/norm", +[](const static_vector_double_3_col &x) { return blaze::norm(x); });
  pkg.def(
      "static-vector-double-3-col/sqr-norm", +[](const static_vector_double_3_col &x) { return blaze::sqrNorm(x); });
  pkg.def(
      "static-vector-double-3-col/transpose",
      +[](const static_vector_double_3_col &x) { return blaze::evaluate(blaze::trans(x)); });

  clbind::class_<static_vector_double_3_row>(pkg, "static-vector-double-3-row")
      .def_constructor("static-vector-double-3-row/make", clbind::constructor<>())
      .def_constructor("static-vector-double-3-row/copy", clbind::constructor<const static_vector_double_3_row &>());

  pkg.def(
      "static-vector-double-3-row/+",
      +[](const static_vector_double_3_row &x, const static_vector_double_3_row &y) { return blaze::evaluate(x + y); });
  pkg.def(
      "static-vector-double-3-row/-",
      +[](const static_vector_double_3_row &x, const static_vector_double_3_row &y) { return blaze::evaluate(x - y); });
  pkg.def(
      "static-vector-double-3-row/at", +[](const static_vector_double_3_row &x, size_t i) { return x[i]; });
  pkg.def(
      "static-vector-double-3-row/setf-at", +[](double value, static_vector_double_3_row &x, size_t i) { return x[i] = value; });
  pkg.def(
      "static-vector-double-3-row/nanp", +[](const static_vector_double_3_row &x) { return blaze::isnan(x); });
  pkg.def(
      "static-vector-double-3-row/infinitep", +[](const static_vector_double_3_row &x) { return blaze::isinf(x); });
  pkg.def(
      "static-vector-double-3-row/finitep", +[](const static_vector_double_3_row &x) { return blaze::isfinite(x); });
  pkg.def(
      "static-vector-double-3-row/defaultp", +[](const static_vector_double_3_row &x) { return blaze::isDefault(x); });
  pkg.def(
      "static-vector-double-3-row/uniformp", +[](const static_vector_double_3_row &x) { return blaze::isUniform(x); });
  pkg.def(
      "static-vector-double-3-row/zerop", +[](const static_vector_double_3_row &x) { return blaze::isZero(x); });
  pkg.def(
      "static-vector-double-3-row/norm", +[](const static_vector_double_3_row &x) { return blaze::norm(x); });
  pkg.def(
      "static-vector-double-3-row/sqr-norm", +[](const static_vector_double_3_row &x) { return blaze::sqrNorm(x); });
  pkg.def(
      "static-vector-double-3-row/transpose",
      +[](const static_vector_double_3_row &x) { return blaze::evaluate(blaze::trans(x)); });

  pkg.def(
      "*/svd3r-svd3c",
      +[](const static_vector_double_3_row &x, const static_vector_double_3_col &y) { return blaze::evaluate(x * y); });

  clbind::class_<dynamic_column_vector_double>(pkg, "dynamic-column-vector/double")
      .def_constructor("make-dynamic-column-vector/double", clbind::constructor<unsigned long>())
      .def_constructor("copy-dynamic-column-vector/double", clbind::constructor<const dynamic_column_vector_double &>());

  pkg.def(
      "dynamic-column-vector/double/length",
      +[](const dynamic_column_vector_double &x) { return blaze::size(x); });
  pkg.def(
      "dynamic-column-vector/double/+",
      +[](const dynamic_column_vector_double &x, const dynamic_column_vector_double &y) { return blaze::evaluate(x + y); });
  pkg.def(
      "dynamic-column-vector/double/-",
      +[](const dynamic_column_vector_double &x, const dynamic_column_vector_double &y) { return blaze::evaluate(x - y); });
  pkg.def(
      "dynamic-column-vector/double/at", +[](const dynamic_column_vector_double &x, size_t i) { return x[i]; });
  pkg.def(
      "dynamic-column-vector/double/setf-at", +[](double value, dynamic_column_vector_double &x, size_t i) { return x[i] = value; });
  pkg.def(
      "dynamic-column-vector/double/nanp", +[](const dynamic_column_vector_double &x) { return blaze::isnan(x); });
  pkg.def(
      "dynamic-column-vector/double/infinitep", +[](const dynamic_column_vector_double &x) { return blaze::isinf(x); });
  pkg.def(
      "dynamic-column-vector/double/finitep", +[](const dynamic_column_vector_double &x) { return blaze::isfinite(x); });
  pkg.def(
      "dynamic-column-vector/double/defaultp", +[](const dynamic_column_vector_double &x) { return blaze::isDefault(x); });
  pkg.def(
      "dynamic-column-vector/double/uniformp", +[](const dynamic_column_vector_double &x) { return blaze::isUniform(x); });
  pkg.def(
      "dynamic-column-vector/double/zerop", +[](const dynamic_column_vector_double &x) { return blaze::isZero(x); });
  pkg.def(
      "dynamic-column-vector/double/norm", +[](const dynamic_column_vector_double &x) { return blaze::norm(x); });
  pkg.def(
      "dynamic-column-vector/double/sqr-norm", +[](const dynamic_column_vector_double &x) { return blaze::sqrNorm(x); });
  pkg.def(
      "dynamic-column-vector/double/transpose",
      +[](const dynamic_column_vector_double &x) { return blaze::evaluate(blaze::trans(x)); });

}

} // namespace blaze_lib
