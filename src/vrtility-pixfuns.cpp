// vrtility C++ pixel functions for GDAL VRT derived bands
// Registered via GDALAddDerivedBandPixelFuncWithArgs at package load time.
// These replace Python/muparser pixel functions for masking and compositing.
// Requires GDAL >= 3.4 for GDALAddDerivedBandPixelFuncWithArgs.

#include <gdal.h>
#include <gdal_vrt.h>
#include <cpl_string.h>
#include <cpl_conv.h>

#include <vector>
#include <algorithm>
#include <cmath>
#include <cstring>
#include <cstdlib>

// ---------------------------------------------------------------------------
// Helper: extract a double value from source buffer at pixel position
// ---------------------------------------------------------------------------
static inline double get_src_value(void *pSrc, int iPixel,
                                   GDALDataType eSrcType) {
  switch (eSrcType) {
    case GDT_Byte:
      return static_cast<double>(static_cast<GByte *>(pSrc)[iPixel]);
    case GDT_Int16:
      return static_cast<double>(static_cast<GInt16 *>(pSrc)[iPixel]);
    case GDT_UInt16:
      return static_cast<double>(static_cast<GUInt16 *>(pSrc)[iPixel]);
    case GDT_Int32:
      return static_cast<double>(static_cast<GInt32 *>(pSrc)[iPixel]);
    case GDT_UInt32:
      return static_cast<double>(static_cast<GUInt32 *>(pSrc)[iPixel]);
    case GDT_Float32:
      return static_cast<double>(static_cast<float *>(pSrc)[iPixel]);
    case GDT_Float64:
      return static_cast<double *>(pSrc)[iPixel];
    default:
      return 0.0;
  }
}

// ---------------------------------------------------------------------------
// Helper: set a double value in the output buffer at pixel position
// ---------------------------------------------------------------------------
static inline void set_dst_value(void *pData, int iPixel,
                                 GDALDataType eBufType, double val) {
  switch (eBufType) {
    case GDT_Byte:
      static_cast<GByte *>(pData)[iPixel] =
          static_cast<GByte>(std::max(0.0, std::min(255.0, val)));
      break;
    case GDT_Int16:
      static_cast<GInt16 *>(pData)[iPixel] = static_cast<GInt16>(val);
      break;
    case GDT_UInt16:
      static_cast<GUInt16 *>(pData)[iPixel] = static_cast<GUInt16>(val);
      break;
    case GDT_Int32:
      static_cast<GInt32 *>(pData)[iPixel] = static_cast<GInt32>(val);
      break;
    case GDT_UInt32:
      static_cast<GUInt32 *>(pData)[iPixel] = static_cast<GUInt32>(val);
      break;
    case GDT_Float32:
      static_cast<float *>(pData)[iPixel] = static_cast<float>(val);
      break;
    case GDT_Float64:
      static_cast<double *>(pData)[iPixel] = val;
      break;
    default:
      break;
  }
}

// ---------------------------------------------------------------------------
// Helper: parse comma-separated integers from a string
// ---------------------------------------------------------------------------
static std::vector<int> parse_int_list(const char *str) {
  std::vector<int> result;
  if (str == nullptr) return result;
  char *buf = CPLStrdup(str);
  char *token = std::strtok(buf, ",");
  while (token != nullptr) {
    result.push_back(std::atoi(token));
    token = std::strtok(nullptr, ",");
  }
  CPLFree(buf);
  return result;
}


// ===========================================================================
// MASKING PIXEL FUNCTIONS
// ===========================================================================

// ---------------------------------------------------------------------------
// vrtility_intmask: Integer value masking → 0/255 (RFC 15 mask standard)
//
// Input: single source band (classification band like SCL)
// Output: Byte band (0 = invalid/masked, 255 = valid)
// Args: mask_values = comma-separated list of values to mask out
// ---------------------------------------------------------------------------
CPLErr vrtility_intmask(void **papoSources, int nSources, void *pData,
                        int nBufXSize, int nBufYSize,
                        GDALDataType eSrcType, GDALDataType eBufType,
                        int nPixelSpace, int nLineSpace,
                        CSLConstList papszFunctionArgs) {
  const char *mask_values_str =
      CSLFetchNameValue(papszFunctionArgs, "mask_values");
  if (mask_values_str == nullptr || nSources < 1) {
    CPLError(CE_Failure, CPLE_AppDefined,
             "vrtility_intmask: requires 'mask_values' argument and "
             "at least 1 source");
    return CE_Failure;
  }

  std::vector<int> mask_values = parse_int_list(mask_values_str);
  int nPixels = nBufXSize * nBufYSize;

  for (int i = 0; i < nPixels; i++) {
    double val = get_src_value(papoSources[0], i, eSrcType);
    int ival = static_cast<int>(val);
    bool is_masked = false;
    for (size_t m = 0; m < mask_values.size(); m++) {
      if (ival == mask_values[m]) {
        is_masked = true;
        break;
      }
    }
    // RFC 15: 0 = invalid, 255 = valid
    set_dst_value(pData, i, eBufType, is_masked ? 0.0 : 255.0);
  }
  return CE_None;
}

// ---------------------------------------------------------------------------
// vrtility_bitmask: Bitwise masking → 0/255
//
// Input: single source band (bitmask band like Fmask)
// Output: Byte band (0 = invalid/masked, 255 = valid)
// Args: mask_values = comma-separated list of bit positions to check
// ---------------------------------------------------------------------------
CPLErr vrtility_bitmask(void **papoSources, int nSources, void *pData,
                        int nBufXSize, int nBufYSize,
                        GDALDataType eSrcType, GDALDataType eBufType,
                        int nPixelSpace, int nLineSpace,
                        CSLConstList papszFunctionArgs) {
  const char *mask_values_str =
      CSLFetchNameValue(papszFunctionArgs, "mask_values");
  if (mask_values_str == nullptr || nSources < 1) {
    CPLError(CE_Failure, CPLE_AppDefined,
             "vrtility_bitmask: requires 'mask_values' argument and "
             "at least 1 source");
    return CE_Failure;
  }

  std::vector<int> bit_positions = parse_int_list(mask_values_str);
  int nPixels = nBufXSize * nBufYSize;

  // Pre-compute bitmask from bit positions
  unsigned int combined_mask = 0;
  for (size_t b = 0; b < bit_positions.size(); b++) {
    combined_mask |= (1u << bit_positions[b]);
  }

  for (int i = 0; i < nPixels; i++) {
    double val = get_src_value(papoSources[0], i, eSrcType);
    unsigned int ival = static_cast<unsigned int>(val);
    bool is_masked = (ival & combined_mask) != 0;
    set_dst_value(pData, i, eBufType, is_masked ? 0.0 : 255.0);
  }
  return CE_None;
}


// ===========================================================================
// COMPOSITING PIXEL FUNCTIONS (with NoData skip semantics)
// ===========================================================================

// ---------------------------------------------------------------------------
// vrtility_median: Median with NoData skip
//
// Input: N source bands (same pixel from N items)
// Output: Median of valid (non-NoData) values, or NoData if all invalid
// Args: no_data_value = the NoData value to skip/output
// ---------------------------------------------------------------------------
CPLErr vrtility_median(void **papoSources, int nSources, void *pData,
                       int nBufXSize, int nBufYSize,
                       GDALDataType eSrcType, GDALDataType eBufType,
                       int nPixelSpace, int nLineSpace,
                       CSLConstList papszFunctionArgs) {
  const char *nd_str =
      CSLFetchNameValueDef(papszFunctionArgs, "no_data_value", "0");
  double nodata = CPLAtof(nd_str);
  int nPixels = nBufXSize * nBufYSize;
  std::vector<double> valid;
  valid.reserve(nSources);

  for (int i = 0; i < nPixels; i++) {
    valid.clear();
    for (int s = 0; s < nSources; s++) {
      double val = get_src_value(papoSources[s], i, eSrcType);
      if (val != nodata && !std::isnan(val)) {
        valid.push_back(val);
      }
    }
    double result;
    if (valid.empty()) {
      result = nodata;
    } else {
      size_t n = valid.size();
      size_t mid = n / 2;
      std::nth_element(valid.begin(), valid.begin() + mid, valid.end());
      if (n % 2 == 0) {
        double a = valid[mid];
        std::nth_element(valid.begin(), valid.begin() + mid - 1, valid.end());
        result = (a + valid[mid - 1]) / 2.0;
      } else {
        result = valid[mid];
      }
    }
    set_dst_value(pData, i, eBufType, result);
  }
  return CE_None;
}

// ---------------------------------------------------------------------------
// vrtility_mean: Mean with NoData skip
// ---------------------------------------------------------------------------
CPLErr vrtility_mean(void **papoSources, int nSources, void *pData,
                     int nBufXSize, int nBufYSize,
                     GDALDataType eSrcType, GDALDataType eBufType,
                     int nPixelSpace, int nLineSpace,
                     CSLConstList papszFunctionArgs) {
  const char *nd_str =
      CSLFetchNameValueDef(papszFunctionArgs, "no_data_value", "0");
  double nodata = CPLAtof(nd_str);
  int nPixels = nBufXSize * nBufYSize;

  for (int i = 0; i < nPixels; i++) {
    double sum = 0.0;
    int count = 0;
    for (int s = 0; s < nSources; s++) {
      double val = get_src_value(papoSources[s], i, eSrcType);
      if (val != nodata && !std::isnan(val)) {
        sum += val;
        count++;
      }
    }
    set_dst_value(pData, i, eBufType, count > 0 ? sum / count : nodata);
  }
  return CE_None;
}

// ---------------------------------------------------------------------------
// vrtility_geomean: Geometric mean with NoData skip
// ---------------------------------------------------------------------------
CPLErr vrtility_geomean(void **papoSources, int nSources, void *pData,
                        int nBufXSize, int nBufYSize,
                        GDALDataType eSrcType, GDALDataType eBufType,
                        int nPixelSpace, int nLineSpace,
                        CSLConstList papszFunctionArgs) {
  const char *nd_str =
      CSLFetchNameValueDef(papszFunctionArgs, "no_data_value", "0");
  double nodata = CPLAtof(nd_str);
  int nPixels = nBufXSize * nBufYSize;

  for (int i = 0; i < nPixels; i++) {
    double log_sum = 0.0;
    int count = 0;
    for (int s = 0; s < nSources; s++) {
      double val = get_src_value(papoSources[s], i, eSrcType);
      if (val != nodata && !std::isnan(val) && val > 0.0) {
        log_sum += std::log(val);
        count++;
      }
    }
    set_dst_value(pData, i, eBufType,
                  count > 0 ? std::exp(log_sum / count) : nodata);
  }
  return CE_None;
}

// ---------------------------------------------------------------------------
// vrtility_quantile: Quantile with NoData skip
// Args: no_data_value, q (probability 0-1)
// ---------------------------------------------------------------------------
CPLErr vrtility_quantile(void **papoSources, int nSources, void *pData,
                         int nBufXSize, int nBufYSize,
                         GDALDataType eSrcType, GDALDataType eBufType,
                         int nPixelSpace, int nLineSpace,
                         CSLConstList papszFunctionArgs) {
  const char *nd_str =
      CSLFetchNameValueDef(papszFunctionArgs, "no_data_value", "0");
  const char *q_str = CSLFetchNameValueDef(papszFunctionArgs, "q", "0.5");
  double nodata = CPLAtof(nd_str);
  double q = CPLAtof(q_str);
  int nPixels = nBufXSize * nBufYSize;
  std::vector<double> valid;
  valid.reserve(nSources);

  for (int i = 0; i < nPixels; i++) {
    valid.clear();
    for (int s = 0; s < nSources; s++) {
      double val = get_src_value(papoSources[s], i, eSrcType);
      if (val != nodata && !std::isnan(val)) {
        valid.push_back(val);
      }
    }
    double result;
    if (valid.empty()) {
      result = nodata;
    } else {
      std::sort(valid.begin(), valid.end());
      double idx = q * (valid.size() - 1);
      size_t lo = static_cast<size_t>(std::floor(idx));
      size_t hi = static_cast<size_t>(std::ceil(idx));
      if (lo == hi || hi >= valid.size()) {
        result = valid[lo];
      } else {
        double frac = idx - lo;
        result = valid[lo] * (1.0 - frac) + valid[hi] * frac;
      }
    }
    set_dst_value(pData, i, eBufType, result);
  }
  return CE_None;
}

// ---------------------------------------------------------------------------
// vrtility_mean_db: Mean in decibels with NoData skip
// Computes: 10 * log10(mean(linear_values))
// ---------------------------------------------------------------------------
CPLErr vrtility_mean_db(void **papoSources, int nSources, void *pData,
                        int nBufXSize, int nBufYSize,
                        GDALDataType eSrcType, GDALDataType eBufType,
                        int nPixelSpace, int nLineSpace,
                        CSLConstList papszFunctionArgs) {
  const char *nd_str =
      CSLFetchNameValueDef(papszFunctionArgs, "no_data_value", "0");
  double nodata = CPLAtof(nd_str);
  int nPixels = nBufXSize * nBufYSize;

  for (int i = 0; i < nPixels; i++) {
    double sum = 0.0;
    int count = 0;
    for (int s = 0; s < nSources; s++) {
      double val = get_src_value(papoSources[s], i, eSrcType);
      if (val != nodata && !std::isnan(val)) {
        sum += val;
        count++;
      }
    }
    double result;
    if (count > 0) {
      double mean_val = sum / count;
      result = (mean_val > 0.0) ? 10.0 * std::log10(mean_val) : nodata;
    } else {
      result = nodata;
    }
    set_dst_value(pData, i, eBufType, result);
  }
  return CE_None;
}


// ===========================================================================
// Registration function — called from R via .Call
// ===========================================================================

// Use extern "C" for R-callable registration
extern "C" {

void vrtility_register_pixfuns(void) {
  GDALAddDerivedBandPixelFuncWithArgs("vrtility_intmask", vrtility_intmask, nullptr);
  GDALAddDerivedBandPixelFuncWithArgs("vrtility_bitmask", vrtility_bitmask, nullptr);
  GDALAddDerivedBandPixelFuncWithArgs("vrtility_median", vrtility_median, nullptr);
  GDALAddDerivedBandPixelFuncWithArgs("vrtility_mean", vrtility_mean, nullptr);
  GDALAddDerivedBandPixelFuncWithArgs("vrtility_geomean", vrtility_geomean, nullptr);
  GDALAddDerivedBandPixelFuncWithArgs("vrtility_quantile", vrtility_quantile, nullptr);
  GDALAddDerivedBandPixelFuncWithArgs("vrtility_mean_db", vrtility_mean_db, nullptr);
}

}  // extern "C"
