#include <clasp/clasp.h>
#include <clasp/core/translators.h>
#include <blaze/Blaze.h>

PACKAGE_USE("COMMON-LISP");
NAMESPACE_PACKAGE_ASSOCIATION(blaze_ns, blaze_pkg, "BLAZE");

namespace translate {
template <> struct translate::to_object<std::complex<double>> {
  static core::T_sp convert(const std::complex<double> &v) { return core::Complex_O::create(v.real(), v.imag()); }
};
} // namespace translate

namespace blaze_ns {

typedef blaze::DynamicVector<double, blaze::columnVector> drcv2;
typedef blaze::DynamicVector<double, blaze::rowVector> drrv2;
typedef blaze::DynamicVector<std::complex<double>, blaze::columnVector> dccv2;
typedef blaze::DynamicVector<std::complex<double>, blaze::rowVector> dcrv2;
typedef blaze::DynamicMatrix<double, blaze::columnMajor> drcm2;
typedef blaze::DynamicMatrix<double, blaze::rowMajor> drrm2;
typedef blaze::DynamicMatrix<std::complex<double>, blaze::columnMajor> dccm2;
typedef blaze::DynamicMatrix<std::complex<double>, blaze::rowMajor> dcrm2;

CL_EXPOSE void blaze_startup() {
  clbind::package_ pkg(blaze_pkg);

  clbind::class_<drcv2>(pkg, "dense-real-column-vector-double")
      .def_constructor("make-dense-real-column-vector-double", clbind::constructor<unsigned long>())
      .def_constructor("copy-dense-real-column-vector-double", clbind::constructor<const drcv2 &>());

  clbind::class_<drrv2>(pkg, "dense-real-row-vector-double")
      .def_constructor("make-dense-real-row-vector-double", clbind::constructor<unsigned long>())
      .def_constructor("copy-dense-real-row-vector-double", clbind::constructor<const drrv2 &>());

  clbind::class_<dccv2>(pkg, "dense-complex-column-vector-double")
      .def_constructor("make-dense-complex-column-vector-double", clbind::constructor<unsigned long>())
      .def_constructor("copy-dense-complex-column-vector-double", clbind::constructor<const dccv2 &>());

  clbind::class_<dcrv2>(pkg, "dense-complex-row-vector-double")
      .def_constructor("make-dense-complex-row-vector-double", clbind::constructor<unsigned long>())
      .def_constructor("copy-dense-complex-row-vector-double", clbind::constructor<const dcrv2 &>());

  clbind::class_<drcm2>(pkg, "dense-real-column-matrix-double")
      .def_constructor("make-dense-real-column-matrix-double", clbind::constructor<unsigned long, unsigned long>())
      .def_constructor("copy-dense-real-column-matrix-double", clbind::constructor<const drcm2 &>());

  clbind::class_<drrm2>(pkg, "dense-real-row-matrix-double")
      .def_constructor("make-dense-real-row-matrix-double", clbind::constructor<unsigned long, unsigned long>())
      .def_constructor("copy-dense-real-row-matrix-double", clbind::constructor<const drrm2 &>());

  clbind::class_<dccm2>(pkg, "dense-complex-column-matrix-double")
      .def_constructor("make-dense-complex-column-matrix-double", clbind::constructor<unsigned long, unsigned long>())
      .def_constructor("copy-dense-complex-column-matrix-double", clbind::constructor<const dccm2 &>());

  clbind::class_<dcrm2>(pkg, "dense-complex-row-matrix-double")
      .def_constructor("make-dense-complex-row-matrix-double", clbind::constructor<unsigned long, unsigned long>())
      .def_constructor("copy-dense-complex-row-matrix-double", clbind::constructor<const dcrm2 &>());

  pkg.def(
      "dense-real-column-vector-double",
      +[](core::Vaslist_sp args) {
        drcv2 res = drcv2(args->total_nargs());
        for (size_t i = 0; args->remaining_nargs() > 0; i++) {
          res[i] = core::clasp_to_double(args->next_arg());
        }
        return res;
      },
      "(core:&va-rest args)"_ll);

  pkg.def(
      "dense-real-row-vector-double",
      +[](core::Vaslist_sp args) {
        drrv2 res = drrv2(args->total_nargs());
        for (size_t i = 0; args->remaining_nargs() > 0; i++) {
          res[i] = core::clasp_to_double(args->next_arg());
        }
        return res;
      },
      "(core:&va-rest args)"_ll);

  pkg.def(
      "dense-complex-column-vector-double",
      +[](core::Vaslist_sp args) {
        dccv2 res = dccv2(args->total_nargs());
        for (size_t i = 0; args->remaining_nargs() > 0; i++) {
          core::T_sp arg = args->next_arg();
          if (gctools::IsA<core::Complex_sp>(arg)) {
            res[i] = std::complex<double>(core::clasp_to_double(gctools::As<core::Complex_sp>(arg)->real()),
                                          core::clasp_to_double(gctools::As<core::Complex_sp>(arg)->imaginary()));
          } else {
            res[i] = core::clasp_to_double(arg);
          }
        }
        return res;
      },
      "(core:&va-rest args)"_ll);

  pkg.def(
      "dense-complex-row-vector-double",
      +[](core::Vaslist_sp args) {
        dcrv2 res = dcrv2(args->total_nargs());
        for (size_t i = 0; args->remaining_nargs() > 0; i++) {
          core::T_sp arg = args->next_arg();
          if (gctools::IsA<core::Complex_sp>(arg)) {
            res[i] = std::complex<double>(core::clasp_to_double(gctools::As<core::Complex_sp>(arg)->real()),
                                          core::clasp_to_double(gctools::As<core::Complex_sp>(arg)->imaginary()));
          } else {
            res[i] = core::clasp_to_double(arg);
          }
        }
        return res;
      },
      "(core:&va-rest args)"_ll);

  pkg.def(
      "length@drcv2", +[](const drcv2 &x) { return blaze::size(x); }, clbind::noAutoExport());
  pkg.def(
      "length@drrv2", +[](const drrv2 &x) { return blaze::size(x); }, clbind::noAutoExport());
  pkg.def(
      "length@dccv2", +[](const dccv2 &x) { return blaze::size(x); }, clbind::noAutoExport());
  pkg.def(
      "length@dcrv2", +[](const dcrv2 &x) { return blaze::size(x); }, clbind::noAutoExport());

  pkg.def(
      "elt@drcv2", +[](const drcv2 &x, size_t i) { return x[i]; }, clbind::noAutoExport());
  pkg.def(
      "elt@drrv2", +[](const drrv2 &x, size_t i) { return x[i]; }, clbind::noAutoExport());
  pkg.def(
      "elt@dccv2", +[](const dccv2 &x, size_t i) { return x[i]; }, clbind::noAutoExport());
  pkg.def(
      "elt@dcrv2", +[](const dcrv2 &x, size_t i) { return x[i]; }, clbind::noAutoExport());

  pkg.def(
      "setf-elt@drcv2", +[](double value, drcv2 &x, size_t i) { return x[i] = value; }, clbind::noAutoExport());
  pkg.def(
      "setf-elt@drrv2", +[](double value, drrv2 &x, size_t i) { return x[i] = value; }, clbind::noAutoExport());
  pkg.def(
      "setf-elt@dccv2", +[](double value, dccv2 &x, size_t i) { return x[i] = value; }, clbind::noAutoExport());
  pkg.def(
      "setf-elt@dcrv2", +[](double value, dcrv2 &x, size_t i) { return x[i] = value; }, clbind::noAutoExport());

  pkg.def(
      "nanp@drcv2", +[](const drcv2 &x) { return blaze::isnan(x); }, clbind::noAutoExport());
  pkg.def(
      "nanp@drrv2", +[](const drrv2 &x) { return blaze::isnan(x); }, clbind::noAutoExport());
  pkg.def(
      "nanp@dccv2", +[](const dccv2 &x) { return blaze::isnan(x); }, clbind::noAutoExport());
  pkg.def(
      "nanp@dcrv2", +[](const dcrv2 &x) { return blaze::isnan(x); }, clbind::noAutoExport());

  pkg.def(
      "infinitep@drcv2", +[](const drcv2 &x) { return blaze::isinf(x); }, clbind::noAutoExport());
  pkg.def(
      "infinitep@drrv2", +[](const drrv2 &x) { return blaze::isinf(x); }, clbind::noAutoExport());
  pkg.def(
      "infinitep@dccv2", +[](const dccv2 &x) { return blaze::isinf(x); }, clbind::noAutoExport());
  pkg.def(
      "infinitep@dcrv2", +[](const dcrv2 &x) { return blaze::isinf(x); }, clbind::noAutoExport());

  pkg.def(
      "finitep@drcv2", +[](const drcv2 &x) { return blaze::isfinite(x); }, clbind::noAutoExport());
  pkg.def(
      "finitep@drrv2", +[](const drrv2 &x) { return blaze::isfinite(x); }, clbind::noAutoExport());
  pkg.def(
      "finitep@dccv2", +[](const dccv2 &x) { return blaze::isfinite(x); }, clbind::noAutoExport());
  pkg.def(
      "finitep@dcrv2", +[](const dcrv2 &x) { return blaze::isfinite(x); }, clbind::noAutoExport());

  pkg.def(
      "defaultp@drcv2", +[](const drcv2 &x) { return blaze::isDefault(x); }, clbind::noAutoExport());
  pkg.def(
      "defaultp@drrv2", +[](const drrv2 &x) { return blaze::isDefault(x); }, clbind::noAutoExport());
  pkg.def(
      "defaultp@dccv2", +[](const dccv2 &x) { return blaze::isDefault(x); }, clbind::noAutoExport());
  pkg.def(
      "defaultp@dcrv2", +[](const dcrv2 &x) { return blaze::isDefault(x); }, clbind::noAutoExport());

  pkg.def(
      "uniformp@drcv2", +[](const drcv2 &x) { return blaze::isUniform(x); }, clbind::noAutoExport());
  pkg.def(
      "uniformp@drrv2", +[](const drrv2 &x) { return blaze::isUniform(x); }, clbind::noAutoExport());

  pkg.def(
      "zerop@drcv2", +[](const drcv2 &x) { return blaze::isZero(x); }, clbind::noAutoExport());
  pkg.def(
      "zerop@drrv2", +[](const drrv2 &x) { return blaze::isZero(x); }, clbind::noAutoExport());
  pkg.def(
      "zerop@dccv2", +[](const dccv2 &x) { return blaze::isZero(x); }, clbind::noAutoExport());
  pkg.def(
      "zerop@dcrv2", +[](const dcrv2 &x) { return blaze::isZero(x); }, clbind::noAutoExport());

  pkg.def(
      "norm@drcv2", +[](const drcv2 &x) { return blaze::norm(x); }, clbind::noAutoExport());
  pkg.def(
      "norm@drrv2", +[](const drrv2 &x) { return blaze::norm(x); }, clbind::noAutoExport());
  pkg.def(
      "norm@dccv2", +[](const dccv2 &x) { return blaze::norm(x); }, clbind::noAutoExport());
  pkg.def(
      "norm@dcrv2", +[](const dcrv2 &x) { return blaze::norm(x); }, clbind::noAutoExport());

  pkg.def(
      "sqr-norm@drcv2", +[](const drcv2 &x) { return blaze::sqrNorm(x); }, clbind::noAutoExport());
  pkg.def(
      "sqr-norm@drrv2", +[](const drrv2 &x) { return blaze::sqrNorm(x); }, clbind::noAutoExport());
  pkg.def(
      "sqr-norm@dccv2", +[](const dccv2 &x) { return blaze::sqrNorm(x); }, clbind::noAutoExport());
  pkg.def(
      "sqr-norm@dcrv2", +[](const dcrv2 &x) { return blaze::sqrNorm(x); }, clbind::noAutoExport());

  pkg.def(
      "transpose@drcv2", +[](const drcv2 &x) { return blaze::evaluate(blaze::trans(x)); }, clbind::noAutoExport());
  pkg.def(
      "transpose@drrv2", +[](const drrv2 &x) { return blaze::evaluate(blaze::trans(x)); }, clbind::noAutoExport());
  pkg.def(
      "transpose@dccv2", +[](const dccv2 &x) { return blaze::evaluate(blaze::trans(x)); }, clbind::noAutoExport());
  pkg.def(
      "transpose@dcrv2", +[](const dcrv2 &x) { return blaze::evaluate(blaze::trans(x)); }, clbind::noAutoExport());

  pkg.def(
      "add@drcv2d@drcv2", +[](const drcv2 &x, const drcv2 &y) { return blaze::evaluate(x + y); }, clbind::noAutoExport());
  pkg.def(
      "add@drrv2d@drrv2", +[](const drrv2 &x, const drrv2 &y) { return blaze::evaluate(x + y); }, clbind::noAutoExport());
  pkg.def(
      "add@dccv2d@drcv2", +[](const dccv2 &x, const drcv2 &y) { return blaze::evaluate(x + y); }, clbind::noAutoExport());
  pkg.def(
      "add@dcrv2d@drrv2", +[](const dcrv2 &x, const drrv2 &y) { return blaze::evaluate(x + y); }, clbind::noAutoExport());
  pkg.def(
      "add@dccv2d@dccv2", +[](const dccv2 &x, const dccv2 &y) { return blaze::evaluate(x + y); }, clbind::noAutoExport());
  pkg.def(
      "add@dcrv2d@dcrv2", +[](const dcrv2 &x, const dcrv2 &y) { return blaze::evaluate(x + y); }, clbind::noAutoExport());

  pkg.def(
      "negate@drcv2d", +[](const drcv2 &x) { return blaze::evaluate(-x); }, clbind::noAutoExport());
  pkg.def(
      "negate@drrv2d", +[](const drrv2 &x) { return blaze::evaluate(-x); }, clbind::noAutoExport());
  pkg.def(
      "negate@dccv2d", +[](const dccv2 &x) { return blaze::evaluate(-x); }, clbind::noAutoExport());
  pkg.def(
      "negate@dcrv2d", +[](const dcrv2 &x) { return blaze::evaluate(-x); }, clbind::noAutoExport());

  pkg.def(
      "subtract@drcv2d@drcv2", +[](const drcv2 &x, const drcv2 &y) { return blaze::evaluate(x - y); }, clbind::noAutoExport());
  pkg.def(
      "subtract@drrv2d@drrv2", +[](const drrv2 &x, const drrv2 &y) { return blaze::evaluate(x - y); }, clbind::noAutoExport());
  pkg.def(
      "subtract@dccv2d@drcv2", +[](const dccv2 &x, const drcv2 &y) { return blaze::evaluate(x - y); }, clbind::noAutoExport());
  pkg.def(
      "subtract@dcrv2d@drrv2", +[](const dcrv2 &x, const drrv2 &y) { return blaze::evaluate(x - y); }, clbind::noAutoExport());
  pkg.def(
      "subtract@dccv2d@dccv2", +[](const dccv2 &x, const dccv2 &y) { return blaze::evaluate(x - y); }, clbind::noAutoExport());
  pkg.def(
      "subtract@dcrv2d@dcrv2", +[](const dcrv2 &x, const dcrv2 &y) { return blaze::evaluate(x - y); }, clbind::noAutoExport());

  pkg.def(
      "multiply@2@drcv2", +[](double x, const drcv2 &y) { return blaze::evaluate(x * y); }, clbind::noAutoExport());
  pkg.def(
      "multiply@2@drrv2", +[](double x, const drrv2 &y) { return blaze::evaluate(x * y); }, clbind::noAutoExport());
  pkg.def(
      "multiply@drrv2@drcv2", +[](const drrv2 &x, const drcv2 &y) { return blaze::evaluate(x * y); }, clbind::noAutoExport());
  pkg.def(
      "multiply@drcv2@drrv2", +[](const drcv2 &x, const drrv2 &y) { return blaze::evaluate(x * y); }, clbind::noAutoExport());
}

} // namespace blaze_ns
