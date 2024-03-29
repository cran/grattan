#ifndef grattan_2024_H
#define grattan_2024_H
#include "grattan.h"

System System2024 = {
  .yr = 2024,
  .nb = 4,
  .BRACKETS = {0, 18200, 45000, 200000, INT_MAX, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.300, 0.45, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 26000,
    .upr_single = 32500,
    .lwr_family = 43846,
    .upr_family = 54807,
    .has_sapto_thr = true,
    .sapto_age = 66,
    .lwr_single_sapto = 41089,
    .upr_single_sapto = 51361,
    .lwr_family_sapto = 57198,
    .upr_family_sapto = 71497,
    .lwr_thr_up_per_child = 4027,
    .taper = 0.1,
    .rate = 0.02
  },
  .has_sapto = true,
  .S = {
    .year = 2023,
    .pension_age = 66.5,
    .mxo_single = 2230,
    .mxo_couple = 1602,
    .mxo_illness = 2040,
    .lwr_single = 32279,
    .lwr_couple = 28974,
    .lwr_illness = 31279,
    .upr_single = 50119,
    .upr_couple = 83580,
    .upr_illness = 95198,
    .taper = 0.125,
    .first_tax_rate = 0.19,
    .second_tax_rate = 0.325,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 37000,
    .lito_max_offset = 445,
    .lito_1st_thresh = 37000,
    .lito_1st_taper = 0.015
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 700,
      .Thresholds = {37500, 45000},
      .Tapers = {0.05, 0.015},
      .nb = 2,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
