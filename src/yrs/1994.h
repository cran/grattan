#ifndef grattan_1994_H
#define grattan_1994_H

constexpr double ORD_TAX_BRACK_1994[6] = {0, 5400, 20700, 36e3, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1994[6] = {0, 0.2, 0.355, 0.385, 0.44125, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_1994 = 12689;
constexpr double ML_UPR_THRESHOLD_SINGLE_1994 = 13643;
constexpr double ML_LWR_THRESHOLD_FAMILY_1994 = 21367;
constexpr double ML_UPR_THRESHOLD_FAMILY_1994 = 22974;
constexpr double ML_LWR_THR_UP_PER_CHILD_1994 =  2100;
constexpr double ML_TAPER_1994 = 0.20;
constexpr double ML_RATE_1994 = 0.014;
// Act No. 27 of 1936 as amended, taking into account amendments up to Act No. 125 of 1994
constexpr double LITO_MAX_OFFSET_1994 = 150;
constexpr double LITO_1ST_TAPER_1994 = -0.04;
constexpr double LITO_1ST_THRESH_1994 = 20700;
double do_1_medicare_levy_1994(double xd, double yd, bool is_family, int n_dependants );
#endif
