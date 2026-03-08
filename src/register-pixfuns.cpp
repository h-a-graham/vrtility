// R-callable wrapper for registering vrtility pixel functions with GDAL
// GDAL >= 3.4 is required — no fallback.
#include <Rcpp.h>

extern "C" void vrtility_register_pixfuns(void);

// [[Rcpp::export]]
bool register_vrtility_pixel_functions() {
  vrtility_register_pixfuns();
  return true;
}
