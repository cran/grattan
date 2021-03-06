% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_rent_assistance.R
\name{model_rent_assistance}
\alias{model_rent_assistance}
\title{Model Rent Assistance}
\usage{
model_rent_assistance(
  sample_file,
  baseline_fy = NULL,
  baseline_Date = NULL,
  Per = "fortnight",
  .Prop_rent_paid_by_RA = NULL,
  Max_rate = NULL,
  Min_rent = NULL,
  calc_baseline_ra = TRUE,
  return. = c("sample_file", "new_ra", "sample_file.int")
)
}
\arguments{
\item{sample_file}{A sample file having the same variables as the data.frame in the example.}

\item{baseline_fy, baseline_Date}{(character) The financial year/date over which the baseline rent assistance is to be calculated. Only one can be provided.}

\item{Per}{Specifies the timeframe in which payments will be made. Can either take value "fortnight" or "annual".}

\item{.Prop_rent_paid_by_RA}{The proportion of the rent above the minimum threshold paid by rent assistance.}

\item{Max_rate}{If not \code{NULL}, a numeric vector indicating for each individual the maximum rent assistance payable.}

\item{Min_rent}{If not \code{NULL}, a numeric vector indicating for each individual the minimum fortnightly rent above which rent assistance is payable. \code{max_rate} and \code{min_rent}}

\item{calc_baseline_ra}{(logical, default: \code{TRUE}) Should the income tax in \code{baseline_fy} or \code{baseline_Date} be included as a column in the result?}

\item{return.}{What should the function return? One of \code{tax}, \code{sample_file}, or \code{sample_file.int}. 
If \code{tax}, the tax payable under the settings; if \code{sample_file}, the \code{sample_file},
but with variables \code{tax} and possibly \code{new_taxable_income}; if \code{sample_file.int}, same as \code{sample_file} but \code{new_tax} is coerced to integer.}
}
\description{
Model Rent Assistance
}
\examples{

library(data.table)
sample <-
  CJ(rent = 1:500,
     n_dependants = 0:3,
     has_partner = 0:1 > 0,
     is_homeowner = 0:1 > 0,
     lives_in_sharehouse = 0:1 > 0)
model_rent_assistance(sample,
                      baseline_fy = "2018-19",
                      .Prop_rent_paid_by_RA = 0.75,
                      Max_rate = 500,
                      Min_rent = 100)
}
