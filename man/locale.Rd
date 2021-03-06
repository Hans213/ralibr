% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendars.R
\name{locale}
\alias{locale}
\title{Extract locale from calendars}
\usage{
locale(x)
}
\arguments{
\item{x}{an instance of a [`Calendar`] or [`JointCalendar`] object}
}
\value{
a string representing the locale (e.g. "AUSY")
}
\description{
Extract locale from calendars
}
\examples{
locale(AUSYCalendar())
locale(c(AUSYCalendar(), AUMECalendar()))
}
\seealso{
Other calendar methods: \code{\link{adjust}},
  \code{\link{generate_schedule}}, \code{\link{is_good}},
  \code{\link{is_valid_bdc}}, \code{\link{is}},
  \code{\link{shift}}, \code{\link{tz}}
}
