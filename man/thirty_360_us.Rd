% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{thirty_360_us}
\alias{thirty_360_us}
\title{The years between two dates using the 30/360 (US) day basis convention.}
\usage{
thirty_360_us(date1, date2)
}
\arguments{
\item{date1}{A date-time object.}

\item{date2}{A date-time object.}
}
\value{
A numeric value representing the number of years between
\code{date1} and \code{date2}.
}
\description{
This calculates the years between two dates using the 30/360 (US) day basis
convention. This convention assumes that months consists of 30 days and
years consist of 360 calendar days.
}
\details{
The day count is determined after making the following (ordered)
modifications:
\enumerate{
  \item If both the start and end dates are the last day of February, set
        the end date to the 30th.
  \item If the start date is the last day of February, set the start date
        to the 30th.
  \item If the end date is the 31st and the start date is either the 30th or
        the 30th, set the end date to the 30th
  \item If the start date is the 31st, set the start date to the 30th.
}

The year fraction is then calculated as:

\deqn{\frac{(d_2 - d_1) + (m_2 - m_1) \times 30 + (y_2 - y_1)
 \times 30}{360}}

The order of \code{date1} and \code{date2} is not important. If \code{date1}
is less than \code{date2} then the result will be non-negative. Otherwise,
the result will be negative.

This is also known as the EOM adjusted bond basis.
}
\seealso{
Other counter methods: \code{\link{actual_360}},
  \code{\link{actual_365}},
  \code{\link{actual_actual_isda}},
  \code{\link{is_valid_day_basis}},
  \code{\link{thirty_360_eu_isda}},
  \code{\link{thirty_360_eu_plus}},
  \code{\link{thirty_360_eu}}, \code{\link{thirty_360}},
  \code{\link{yearfrac}}
}
\keyword{internal}
