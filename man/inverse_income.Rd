% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inverse_income.R
\name{inverse_income}
\alias{inverse_income}
\title{Inverse income tax functions}
\usage{
inverse_income(
  tax,
  fy.year = "2012-13",
  zero.tax.income = c("maximum", "zero", "uniform", numeric(1)),
  ...
)
}
\arguments{
\item{tax}{The tax payable.}

\item{fy.year}{The relevant financial year.}

\item{zero.tax.income}{A character vector, ("maximum", "zero", "uniform", numeric(1)) Given that many incomes map to zero taxes, the \code{income_tax} function is not invertible there. As a consequence, the inverse function's value must be specified for tax = 0. "maximum" returns the maximum integer income one can have with a zero tax liability; "zero" returns zero for any tax of zero; "uniform" provides a random integer from zero to the maximum income with a zero tax. The value can also be specified explicitly.}

\item{...}{Other arguments passed to \code{income_tax}. If \code{tax} or \code{fy.year} are vectors, these should be named vectors.}
}
\value{
The approximate taxable income given the tax payable for the financial year. See Details.
}
\description{
Inverse income tax functions
}
\details{
This function has an error of $2.
}
