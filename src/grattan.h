#ifndef grattan_H
#define grattan_H

Rcpp::NumericVector pminC(Rcpp::NumericVector x, double a);
double sapto_rcpp_singleton(double rebate_income,double max_offset,double lower_threshold,double taper_rate,bool sapto_eligible,double Spouse_income,bool is_married);

#endif
