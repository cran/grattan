% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/medicare_levy.R
\name{medicare_levy}
\alias{medicare_levy}
\title{Medicare levy}
\usage{
medicare_levy(
  income,
  fy.year = "2013-14",
  Spouse_income = 0L,
  sapto.eligible = FALSE,
  sato = NULL,
  pto = NULL,
  family_status = "individual",
  n_dependants = 0L,
  is_married = NULL,
  .checks = FALSE
)
}
\arguments{
\item{income}{\describe{
\item{\code{numeric(N)}}{The income for medicare levy purposes of the taxpayer.}
}}

\item{fy.year}{\describe{
\item{\code{character(1)} or \code{character(N)} or \code{fy(N)} or \code{fy(1)}}{
The tax year in which \code{income} was earned. A vector satisfying \code{fy::validate_fys_permitted}.}
}}

\item{Spouse_income}{\describe{
\item{\code{numeric(1)} or \code{numeric(N)}}{The income of the taxpayer's spouse. Missing values are
imputed to zeroes. Values are truncated to integer.}
}}

\item{sapto.eligible}{\describe{
\item{\code{logical(1)} or \code{logical(N)}}{Is the taxpayer entitled to the SAPTO thresholds? Missing
values are imputed to \code{FALSE}.}
}}

\item{sato, pto}{Is the taxpayer eligible for the Senior Australians Tax Offset or Pensions Tax Offset?
\code{pto = TRUE} not supported and will be set to \code{FALSE}, with a warning.}

\item{family_status}{(Deprecated: use `is_married` and `n_dependants` instead)}

\item{n_dependants}{\describe{
\item{\code{integer(N)} or \code{integer(1)}}{Number of dependants the taxpayer has. If nonzero, 
the taxpayer is entitled to the family thresholds of the Medicare levy, and
each dependant child increases the thresholds.}
}}

\item{is_married}{\describe{
\item{\code{logical(N)}}{Is the taxpayer married? Married individuals (or those 
whose \code{Spouse_income > 0}) are deemed to be families when determining 
cut-off thresholds.}
}}

\item{.checks}{Whether or not to perform checks on inputs.}
}
\value{
The Medicare levy payable for that taxpayer.
}
\description{
The (actual) amount payable for the Medicare levy.
}
\details{
The Medicare levy for individuals is imposed by the \emph{Medicare Levy Act 1986} (Cth).
The function only calculates the levy for individuals (not trusts).
It includes the s 7 \emph{Levy in cases of small incomes}, including the differences for those
eligible for \code{\link{sapto}}.
s 8 \emph{Amount of levy---person who has spouse or dependants} (though the number of dependants
is not a variable in the sample files).

The function does \strong{not} include the Medicare levy surcharge; it assumes that all 
persons (who would potentially be liable for it) avoided it.

The Seniors and Pensioners Tax Offset was formed in 2012-13 as an amalgam
 of the Senior Australians Tax Offset and the Pensions Tax Offset. 
Medicare rates before 2012-13 were different based on these offsets. 
For most taxpayers, eligibility would be based on whether your age is over the pension age (currently 65).
If \code{sato} and \code{pto} are \code{NULL}, \code{sapto.eligible} stands for eligibility for the \code{sato} and not \code{pto}.
If \code{sato} or \code{pto} are not \code{NULL} for such years, only \code{sato} is currently considered. 
Supplying \code{pto} independently is currently a warning.

See \url{http://classic.austlii.edu.au/au/legis/cth/consol_act/mla1986131/}
for the \emph{Medicare Levy Act 1986} (Cth).
}
