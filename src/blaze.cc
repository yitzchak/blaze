#include <clasp/clasp.h>
#include <clasp/core/translators.h>
#include <blaze/Blaze.h>

PACKAGE_USE("COMMON-LISP");
NAMESPACE_PACKAGE_ASSOCIATION(blaze_sys, blaze_sys_pkg, "BLAZE/SYS");

namespace blaze_sys {

CL_EXPOSE void blaze_startup() {
  clbind::package_ _blaze_sys(blaze_sys_pkg);
}

}
