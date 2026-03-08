#' Masking functions VRT pixel functions.
#' @param buffer_size A buffer size to apply to the mask (numeric, default: 0).
#' A buffer
#' size > 0 will dilate the mask by the specified number of pixels.
#' This can be useful to remove edge effects around clouds.
#' If a buffer size > 0 is specified, the `scipy` python library will
#' automatically be installed and the legacy Python path will be used.
#' @noRd
#' @keywords internal
#' @details `set_mask` applies a given mask. Only used in the legacy
#' pixel-function-per-band path (when buffer_size > 0). For buffer_size == 0,
#' the MaskBand approach is used instead.
#' @rdname vrt_set_maskfun
set_mask <- function(buffer_size = 0) {
  v_assert_type(
    buffer_size,
    "buffer_size",
    c("numeric", "integer"),
    multiple = TRUE
  )

  v_assert_within_range(
    buffer_size,
    "buffer_size",
    0,
    Inf
  )

  if (buffer_size > 0) {
    vrtility_py_require("scipy")
  }

  return(set_mask_python(buffer_size))
}

#' @details `set_mask_python` provides a Python pixel function for masking.
#' Only used for buffered masking (buffer_size > 0).
#' @noRd
#' @keywords internal
set_mask_python <- function(buffer_size) {
  pf <- glue::glue(
    "
import numpy as np

def bitmask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    valid_vals =  [int(x) for x in kwargs['valid_values'].decode().split(',')]
    no_data_val = int(kwargs['no_data_value'])
    buffer_size = {buffer_size}

    if buffer_size > 0:
        from scipy import ndimage
        # Create mask (True = nodata)
        mask = in_ar[1] == 0

        # Buffer the mask
        structure = ndimage.generate_binary_structure(2, 2)  # 8-connectivity
        buffered_mask = ndimage.binary_dilation(mask, structure=structure, iterations=buffer_size)

        # Apply buffered mask
        out_ar[:] = np.where(buffered_mask, no_data_val, in_ar[0])
    else:
        # no buffering
        out_ar[:] = np.where(in_ar[1] > 0, in_ar[0], no_data_val)
"
  )
  class(pf) <- c("python_pixel_function", class(pf))
  return(pf)
}


#' @details `build_intmask` returns a C++ pixel function for integer value
#' masking. This is used to mask out pixels based on a band containing integer
#' values (e.g. the Sentinel-2 SCL band). The C++ function is registered with
#' GDAL at package load time.
#' @export
#' @rdname vrt_set_maskfun
build_intmask <- function() {
  assert_cpp_pixfuns()
  build_intmask_cpp()
}

#' @noRd
#' @keywords internal
build_intmask_cpp <- function() {
  pf <- "vrtility_intmask"
  class(pf) <- c("cpp_pixel_function", class(pf))
  return(pf)
}


#' @details `build_bitmask` returns a C++ pixel function for bitwise masking.
#' This should be used where bitwise operations are required, e.g. for HLS
#' data where the "Fmask" band requires bitwise operations.
#' @export
#' @rdname vrt_set_maskfun
build_bitmask <- function() {
  assert_cpp_pixfuns()
  build_bitmask_cpp()
}

#' @noRd
#' @keywords internal
build_bitmask_cpp <- function() {
  pf <- "vrtility_bitmask"
  class(pf) <- c("cpp_pixel_function", class(pf))
  return(pf)
}

#' Assert that C++ pixel functions are registered
#' @noRd
#' @keywords internal
assert_cpp_pixfuns <- function() {
  if (!has_cpp_pixfuns()) {
    cli::cli_abort(
      c(
        "!" = "C++ pixel functions are not available.",
        "i" = "vrtility requires GDAL >= 3.4 headers at compile time.",
        "i" = "Reinstall the package with GDAL >= 3.4 available."
      )
    )
  }
}

#' @title Construct a cloud mask using the OmniCloudMask python library.
#' @description This function constructs a cloud mask using the OmniCloudMask
#' python library. It is designed to be used with the `vrt_set_maskfun()`
#' function.
#' @param patch_size The size/dimension of the patches to use for prediction
#' (numeric default: 600).
#' @param patch_overlap The overlap between patches (numeric default: 300).
#' @param batch_size The batch size to use for prediction (numeric default: 1).
#' @param inference_dtype The data type to use for inference. Options include
#' "bfloat16" and "float32" (character, default: "bfloat16"). Using "bfloat16"
#' should be faster if supported by the hardware.
#' @param nodata_value The nodata value to use in the output mask (numeric,
#' default: 0).
#' @param model_version The version of the OmniCloudMask model to use; options
#' include "4.0", "3.0", "2.0", and "1.0". If `NULL`, the latest version will be
#' used (character, default: `NULL`).
#' @return A Python function that can be used as a pixel function in a VRT
#' raster. The function will apply the OmniCloudMask model to the specified
#' bands and create a cloud mask.
#' @rdname vrt_create_mask
#' @references
#' OmniCloudMask GitHub repository: \url{https://github.com/DPIRD-DMA/OmniCloudMask}
#' @export
create_omnicloudmask <- function(
  patch_size = 1000,
  patch_overlap = 300,
  batch_size = 1,
  inference_dtype = c("bfloat16", "float32"),
  nodata_value = 0,
  model_version = NULL
) {
  # assert omnicloudmask is installed
  if (!reticulate::py_module_available("omnicloudmask")) {
    vrtility_py_require("omnicloudmask")
  }

  # fastai is only required for legacy model versions (1-3)
  if (!is.null(model_version) && model_version %in% c("1.0", "2.0", "3.0")) {
    if (!reticulate::py_module_available("fastai")) {
      vrtility_py_require("fastai")
    }
  }

  # assert args
  v_assert_type(
    patch_size,
    "patch_size",
    c("numeric", "integer"),
    multiple = TRUE
  )
  v_assert_type(
    patch_overlap,
    "patch_overlap",
    c("numeric", "integer"),
    multiple = TRUE
  )
  v_assert_type(
    batch_size,
    "batch_size",
    c("numeric", "integer"),
    multiple = TRUE
  )

  inference_dtype <- rlang::arg_match(inference_dtype)

  if (!is.null(model_version)) {
    model_version <- rlang::arg_match(
      model_version,
      c("4.0", "3.0", "2.0", "1.0")
    )
  } else {
    model_version <- "None"
  }

  pyfun <- glue::glue(
    "
import numpy as np
import omnicloudmask as omc

def create_mask(in_ar, out_ar, xoff, yoff, xsize, ysize, raster_xsize,
                  raster_ysize, buf_radius, gt, **kwargs):
    np_rgn = np.stack([in_ar[0], in_ar[1], in_ar[2]], axis=0)

    pred_mask = omc.predict_from_array(
      np_rgn,
      patch_size = {patch_size},
      patch_overlap = {patch_overlap},
      batch_size = {batch_size},
      inference_dtype = '{inference_dtype}',
      no_data_value = {nodata_value},
      model_version = {model_version}
    )

    out_ar[:] = pred_mask[0]
"
  )

  attr(pyfun, "mask_name") <- "create_omnicloudmask"
  attr(pyfun, "mask_description") <- "omnicloudmask"
  attr(pyfun, "required_bands") <- c("red", "green", "nir")
  return(pyfun)
}
