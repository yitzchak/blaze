#include <clasp/clasp.h>
#include <clasp/core/translators.h>
#include <blaze/Blaze.h>

PACKAGE_USE("COMMON-LISP");
NAMESPACE_PACKAGE_ASSOCIATION(blaze_ns, blaze_pkg, "BLAZE");

namespace blaze_ns {

typedef blaze::DynamicVector<double, blaze::columnVector> dynamic_column_vector_double;
typedef blaze::DynamicVector<double, blaze::rowVector> dynamic_row_vector_double;

CL_EXPOSE void blaze_startup() {
  clbind::package_ pkg(blaze_pkg);

  clbind::class_<dynamic_column_vector_double>(pkg, "dynamic-column-vector/double")
      .def_constructor("make-dynamic-column-vector/double", clbind::constructor<unsigned long>())
      .def_constructor("copy-dynamic-column-vector/double", clbind::constructor<const dynamic_column_vector_double &>());

  clbind::class_<dynamic_row_vector_double>(pkg, "dynamic-row-vector/double")
      .def_constructor("make-dynamic-row-vector/double", clbind::constructor<unsigned long>())
      .def_constructor("copy-dynamic-row-vector/double", clbind::constructor<const dynamic_row_vector_double &>());

  pkg.def(
      "dynamic-column-vector/double",
      +[](core::Vaslist_sp args) {
        dynamic_column_vector_double res = dynamic_column_vector_double(args->total_nargs());
        for (size_t i = 0; args->remaining_nargs() > 0; i++) {
          res[i] = core::clasp_to_double(args->next_arg());
        }
        return res;
      },
      "(core:&va-rest args)"_ll);

  pkg.def(
      "dynamic-row-vector/double",
      +[](core::Vaslist_sp args) {
        dynamic_row_vector_double res = dynamic_row_vector_double(args->total_nargs());
        for (size_t i = 0; args->remaining_nargs() > 0; i++) {
          res[i] = core::clasp_to_double(args->next_arg());
        }
        return res;
      },
      "(core:&va-rest args)"_ll);

  pkg.def(
      "dynamic-column-vector/double/length", +[](const dynamic_column_vector_double &x) { return blaze::size(x); });
  pkg.def(
      "dynamic-row-vector/double/length", +[](const dynamic_row_vector_double &x) { return blaze::size(x); });

  pkg.def(
      "dynamic-column-vector/double/at", +[](const dynamic_column_vector_double &x, size_t i) { return x[i]; });
  pkg.def(
      "dynamic-row-vector/double/at", +[](const dynamic_row_vector_double &x, size_t i) { return x[i]; });

  pkg.def(
      "dynamic-column-vector/double/setf-at",
      +[](double value, dynamic_column_vector_double &x, size_t i) { return x[i] = value; });
  pkg.def(
      "dynamic-row-vector/double/setf-at",
      +[](double value, dynamic_row_vector_double &x, size_t i) { return x[i] = value; });

  pkg.def(
      "dynamic-column-vector/double/nanp", +[](const dynamic_column_vector_double &x) { return blaze::isnan(x); });
  pkg.def(
      "dynamic-row-vector/double/nanp", +[](const dynamic_row_vector_double &x) { return blaze::isnan(x); });

  pkg.def(
      "dynamic-column-vector/double/infinitep", +[](const dynamic_column_vector_double &x) { return blaze::isinf(x); });
  pkg.def(
      "dynamic-row-vector/double/infinitep", +[](const dynamic_row_vector_double &x) { return blaze::isinf(x); });

  pkg.def(
      "dynamic-column-vector/double/finitep", +[](const dynamic_column_vector_double &x) { return blaze::isfinite(x); });
  pkg.def(
      "dynamic-row-vector/double/finitep", +[](const dynamic_row_vector_double &x) { return blaze::isfinite(x); });

  pkg.def(
      "dynamic-column-vector/double/defaultp", +[](const dynamic_column_vector_double &x) { return blaze::isDefault(x); });
  pkg.def(
      "dynamic-row-vector/double/defaultp", +[](const dynamic_row_vector_double &x) { return blaze::isDefault(x); });

  pkg.def(
      "dynamic-column-vector/double/uniformp", +[](const dynamic_column_vector_double &x) { return blaze::isUniform(x); });
  pkg.def(
      "dynamic-row-vector/double/uniformp", +[](const dynamic_row_vector_double &x) { return blaze::isUniform(x); });

  pkg.def(
      "dynamic-column-vector/double/zerop", +[](const dynamic_column_vector_double &x) { return blaze::isZero(x); });
  pkg.def(
      "dynamic-row-vector/double/zerop", +[](const dynamic_row_vector_double &x) { return blaze::isZero(x); });

  pkg.def(
      "dynamic-column-vector/double/norm", +[](const dynamic_column_vector_double &x) { return blaze::norm(x); });
  pkg.def(
      "dynamic-row-vector/double/norm", +[](const dynamic_row_vector_double &x) { return blaze::norm(x); });

  pkg.def(
      "dynamic-column-vector/double/sqr-norm", +[](const dynamic_column_vector_double &x) { return blaze::sqrNorm(x); });
  pkg.def(
      "dynamic-row-vector/double/sqr-norm", +[](const dynamic_row_vector_double &x) { return blaze::sqrNorm(x); });

  pkg.def(
      "dynamic-column-vector/double/transpose",
      +[](const dynamic_column_vector_double &x) { return blaze::evaluate(blaze::trans(x)); });
  pkg.def(
      "dynamic-row-vector/double/transpose",
      +[](const dynamic_row_vector_double &x) { return blaze::evaluate(blaze::trans(x)); });

  pkg.def(
      "dynamic-column-vector/double/+",
      +[](const dynamic_column_vector_double &x, const dynamic_column_vector_double &y) { return blaze::evaluate(x + y); });
  pkg.def(
      "dynamic-row-vector/double/+",
      +[](const dynamic_row_vector_double &x, const dynamic_row_vector_double &y) { return blaze::evaluate(x + y); });

  pkg.def(
      "dynamic-column-vector/double/-",
      +[](const dynamic_column_vector_double &x, const dynamic_column_vector_double &y) { return blaze::evaluate(x - y); });
  pkg.def(
      "dynamic-row-vector/double/-",
      +[](const dynamic_row_vector_double &x, const dynamic_row_vector_double &y) { return blaze::evaluate(x - y); });

  pkg.def(
      "multiply/d/dcd",
      +[](double x, const dynamic_column_vector_double &y) { return blaze::evaluate(x * y); });
  pkg.def(
      "multiply/d/drd",
      +[](double x, const dynamic_row_vector_double &y) { return blaze::evaluate(x * y); });
  pkg.def(
      "multiply/drd/dcd",
      +[](const dynamic_row_vector_double &x, const dynamic_column_vector_double &y) { return blaze::evaluate(x * y); });
  pkg.def(
      "multiply/dcd/drd",
      +[](const dynamic_column_vector_double &x, const dynamic_row_vector_double &y) { return blaze::evaluate(x * y); });
}

} // namespace blaze_ns
