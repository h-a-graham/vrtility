// R-callable wrapper for registering vrtility pixel functions with GDAL
#include <Rcpp.h>

#ifdef HAVE_GDAL_PIXFUN
extern "C" void vrtility_register_pixfuns(void);
#endif

// [[Rcpp::export]]
bool register_vrtility_pixel_functions() {
#ifdef HAVE_GDAL_PIXFUN
  vrtility_register_pixfuns();
  return true;
#else
  return false;
#endif
}
