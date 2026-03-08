#' C++ pixel functions for compositing.
#' @description These functions return the name of a registered C++ pixel
#' function for use with `vrt_set_gdal_pixelfun()`. They are faster than
#' the Python equivalents and don't require Python dependencies.
#' Available when the package is compiled with GDAL >= 3.4.
#' @rdname vrt_set_gdal_pixelfun
#' @details `median_cpp` is a C++ pixel function that calculates the median of
#' the input arrays, skipping NoData values. Falls back to `median_numpy()`
#' if C++ pixel functions are not available.
#' @export
median_cpp <- function() {
  if (!has_cpp_pixfuns()) {
    cli::cli_warn(
      c(
        "!" = "C++ pixel functions not available, falling back to Python.",
        "i" = "Ensure GDAL >= 3.4 headers are available at compile time."
      )
    )
    return(median_numpy())
  }
  "vrtility_median"
}

#' @rdname vrt_set_gdal_pixelfun
#' @details `mean_cpp` is a C++ pixel function that calculates the mean of the
#' input arrays, skipping NoData values.
#' @export
mean_cpp <- function() {
  if (!has_cpp_pixfuns()) {
    cli::cli_warn(
      c(
        "!" = "C++ pixel functions not available, falling back to Python.",
        "i" = "Ensure GDAL >= 3.4 headers are available at compile time."
      )
    )
    return(mean_numpy())
  }
  "vrtility_mean"
}

#' @rdname vrt_set_gdal_pixelfun
#' @details `geomean_cpp` is a C++ pixel function that calculates the geometric
#' mean of the input arrays, skipping NoData values.
#' @export
geomean_cpp <- function() {
  if (!has_cpp_pixfuns()) {
    cli::cli_warn(
      c(
        "!" = "C++ pixel functions not available, falling back to Python.",
        "i" = "Ensure GDAL >= 3.4 headers are available at compile time."
      )
    )
    return(geomean_numpy())
  }
  "vrtility_geomean"
}

#' @param q Probability of the quantile to compute. Values must be between
#' 0 and 1.
#' @rdname vrt_set_gdal_pixelfun
#' @details `quantile_cpp` is a C++ pixel function that calculates the quantile
#' of the input arrays, skipping NoData values.
#' @export
quantile_cpp <- function(q = 0.5) {
  if (!has_cpp_pixfuns()) {
    cli::cli_warn(
      c(
        "!" = "C++ pixel functions not available, falling back to Python.",
        "i" = "Ensure GDAL >= 3.4 headers are available at compile time."
      )
    )
    return(quantile_numpy(q))
  }
  "vrtility_quantile"
}

#' @rdname vrt_set_gdal_pixelfun
#' @details `mean_db_cpp` is a C++ pixel function that calculates the mean of
#' the input arrays in decibels, skipping NoData values. Useful for radar
#' backscatter data.
#' @export
mean_db_cpp <- function() {
  if (!has_cpp_pixfuns()) {
    cli::cli_warn(
      c(
        "!" = "C++ pixel functions not available, falling back to Python.",
        "i" = "Ensure GDAL >= 3.4 headers are available at compile time."
      )
    )
    return(mean_db_numpy())
  }
  "vrtility_mean_db"
}
