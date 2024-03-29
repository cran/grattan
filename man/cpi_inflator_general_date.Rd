% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpi_general_date.R
\name{cpi_inflator_general_date}
\alias{cpi_inflator_general_date}
\title{CPI for general dates}
\usage{
cpi_inflator_general_date(from_nominal_price = 1, from_date, to_date, ...)
}
\arguments{
\item{from_nominal_price}{(numeric) the nominal prices to be converted to a real price}

\item{from_date}{(character, date-like) the 'date' contemporaneous to \code{from_nominal_price}. The acceptable forms are 'YYYY', 'YYYY-YY' (financial year), 'YYYY-MM-DD', and 'YYYY-Q[1-4]' (quarters). Note a vector cannot contain a mixture of date forms.}

\item{to_date}{(character, date-like) the date at which the real price is valued (where the nominal price equals the real price). Same forms as for \code{from_date}}

\item{...}{other arguments passed to \code{\link{cpi_inflator}}}
}
\value{
A vector of real prices in \code{to_date} dollars.
}
\description{
Deprecated in favour of \code{grattanInflators::cpi_inflator}
}
