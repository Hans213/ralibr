% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/valuations.R
\name{value_fixed_leg}
\alias{value_fixed_leg}
\title{Value a fixed leg}
\usage{
value_fixed_leg(Cv, EffD, MatD, Stub = "short_front", Not = 1e+07,
  ExchNot = FALSE, Calendar = EUTACalendar(), CCY = "EUR", Rate = 0.01,
  Freq = "A", DCC = "ACT/360", BDC = "MF", Dir = "PAY")
}
\arguments{
\item{Cv}{Discount Curve}

\item{EffD}{Effective Date}

\item{MatD}{Maturity Date}

\item{Not}{Notional}

\item{ExchNot}{Exchange of Notionals TRUE/FALSE}

\item{CCY}{Currency}

\item{Rate}{Coupon Rate}

\item{Freq}{Coupon Rate Frequency}

\item{DCC}{Day Count Convention}

\item{BDC}{Business Day Convention}

\item{Dir}{Direction PAY/SELL}

\item{ValD}{Valuation Date}

\item{SecLast}{Second to last Payment Date}

\item{Out}{Output type}
}
\value{
Depends on Output type
}
\description{
Value a fixed leg
}
\examples{
require(lubridate)
value_fixed_leg(EUR_OIS_20171229,ymd(20170101),ymd(20220101))
}
