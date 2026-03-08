read_vals <- function(r) {
  compute_with_py_env({
    ds <- methods::new(gdalraster::GDALRaster, r)
    on.exit(ds$close())
    gdalraster::read_ds(ds)
  })
}

bitmask_test_vals <- function() {
  hls_files <- fs::dir_ls(system.file("hls-data", package = "vrtility"))

  ex_collect <- vrt_collect(hls_files[1])

  ex_collect_mask_bit <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "Fmask",
      mask_values = c(3, 2),
      build_mask_pixfun = build_bitmask(),
      drop_mask_band = FALSE
    )

  ex_nm <- sum(read_vals(ex_collect$vrt_src), na.rm = TRUE)
  ex_bm <- sum(read_vals(ex_collect_mask_bit$vrt_src), na.rm = TRUE)
  return(list(
    no_mask = ex_nm,
    mask_bit = ex_bm
  ))
}

intmask_test_vals <- function() {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))

  ex_collect <- vrt_collect(s2files[3])

  ex_collect_mask_0buff <- ex_collect |>
    vrt_set_maskfun(
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3, 8, 9, 10, 11),
      build_mask_pixfun = build_intmask(),
      drop_mask_band = FALSE,
      buffer_size = 0
    )

  # test that masking changes the output
  ex_nm <- sum(read_vals(ex_collect$vrt_src), na.rm = TRUE)
  ex_0b <- sum(read_vals(ex_collect_mask_0buff$vrt_src), na.rm = TRUE)
  return(list(
    no_mask = ex_nm,
    mask_0buff = ex_0b
  ))
}


test_that("vrt_set_maskfun works with C++ intmask (MaskBand approach)", {
  vals <- intmask_test_vals()
  ex_nm <- vals$no_mask
  ex_0b <- vals$mask_0buff

  expect_gt(ex_nm, ex_0b)

  expect_error(set_mask(buffer_size = -1))
})


test_that("C++ mask functions return correct classes", {
  bi <- build_intmask()
  expect_s3_class(bi, "cpp_pixel_function")
  expect_equal(as.character(bi), "vrtility_intmask")

  bb <- build_bitmask()
  expect_s3_class(bb, "cpp_pixel_function")
  expect_equal(as.character(bb), "vrtility_bitmask")
})


test_that("set_mask with buffer returns python function", {
  sm_buff5 <- set_mask(buffer_size = 5)
  expect_s3_class(sm_buff5, "python_pixel_function")

  sm_buff0 <- set_mask(buffer_size = 0)
  expect_s3_class(sm_buff0, "python_pixel_function")
})


test_that("bitmask works with C++ pixel function", {
  vals <- bitmask_test_vals()
  expect_gt(vals$no_mask, vals$mask_bit)
})


test_that("non-C++ pixel function errors without buffering", {
  s2files <- fs::dir_ls(system.file("s2-data", package = "vrtility"))
  ex_collect <- vrt_collect(s2files[3])

  # Passing a plain string (not cpp_pixel_function class) should error
  expect_error(
    vrt_set_maskfun(
      ex_collect[[1]][[1]],
      mask_band = "SCL",
      mask_values = c(0, 1, 2, 3),
      build_mask_pixfun = "some_python_func",
      buffer_size = 0
    ),
    "Not supported"
  )
})
