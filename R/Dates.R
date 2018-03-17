# Conversion --------------------------------------------------------------

#' Convert a date in R to an excel numeric value
#'
#' @param d1 a Date in R
#'
#' @description Every function exported to the user that returns a date
#'    must return a date value in Excel numeric value.
#'
#' @return a numeric value representing an Excel date

date_to_excel <- function(d1) {
        d <- as.numeric(d1 - as.Date(0, origin = "1899-12-30", tz = 'UTC'))
        return(d)
}

#' Parsing of dates to Excel numeric value
#'
#' @param DateToParse Input date value (integer/character/date)
#' @param DateType optional (default = "European") - format for DateToParse
#'
#' @return a Date in R date format
#'
#' @importFrom  lubridate parse_date_time
#'
parse_date_internal <-
        function(DateToParse, DateType = "European") {
                # Numerics are reserved for Excel integration and converted to R Dates.
                if (is.numeric(DateToParse)) {
                        DateToParse <- as.Date(x = DateToParse, origin = "1899-12-30")
                }

                # If DateToParse is a character we parse it with DateType formating
                if (is.character(x = DateType)) {
                        if (toupper(DateType) == "EUROPEAN") {
                                date <-
                                        as.Date(lubridate::parse_date_time(
                                                x = DateToParse,
                                                orders = c("dmy", "ymd")
                                        ))
                        } else if (toupper(DateType) == "AMERICAN") {
                                date <-
                                        as.Date(lubridate::parse_date_time(
                                                x = DateToParse,
                                                orders = c("mdy", "ymd")
                                        ))
                        } else{
                                stop("Wrong DateType")
                        }
                }

                # If DateToParse is a Date then do nothing


                # Output warning in case we return NA value
                if (any(is.na(x = date))) {
                        warning("ParseDate failed to return a Date")
                }

                # We don't convert to Excel numeric values for internal use
                return(date)
        }

#' Parsing of dates to Excel numeric value
#'
#' @param DateToParse Input date value (integer/character/date)
#' @param DateType optional (default = "European") - format for DateToParse
#'
#' @return a Date in Excel numeric value
#'
#' @export
#'
parse_date <- function(DateToParse, DateType = "European") {
        # Numerics are reserved for Excel integration
        if (is.numeric(DateToParse)) {
                DateToParse <- as.Date(x = DateToParse, origin = "1899-12-30")
        }

        # If DateToParse is a character we parse it with DateType formating
        if (is.character(x = DateType)) {
                if (toupper(DateType) == "EUROPEAN") {
                        date <-
                                as.Date(lubridate::parse_date_time(
                                        x = DateToParse,
                                        orders = c("dmy", "ymd")
                                ))
                } else if (toupper(DateType) == "AMERICAN") {
                        date <-
                                as.Date(lubridate::parse_date_time(
                                        x = DateToParse,
                                        orders = c("mdy", "ymd")
                                ))
                } else{
                        stop("Wrong DateType")
                }
        }

        # If DateToParse is a Date then do nothing


        # Output warning in case we return NA value
        if (any(is.na(x = date))) {
                warning("ParseDate failed to return a Date")
        }

        # Exportable function so output is an Excel numeric value
        date <- date_to_excel(d1 = date)

        return(date)
}
attr(parse_date, "description") <-
        list("Parsing of dates to Excel numeric value",
                DateToParse = "Input date value (integer/character/date)",
                DateType = "optional (default = European) - format for DateToParse")



# Shifters ----------------------------------------------------------------


#' Adjust to good dates
#'
#' One common financial markets date arithmetic requires a date needs to be
#' rolled to the closest business day following some convention (see
#' [is_valid_bdc()] for further details). Such rolled dates can be determined by
#' calling `adjust()`.
#'
#' @param dates a vector of dates to adjust.
#' @param bdc the business day convention used to roll the `dates` if necessary
#' @param calendar an object that inherits from [`Calendar`] or [`JointCalendar`]
#' which is used to determine the goodness of `dates`
#' @return a vector of adjusted dates - good days are unadjusted
#' @examples
#' ausy <- AUSYCalendar()
#' adjust(lubridate::ymd("20120102"), "u", ausy)
#' adjust(lubridate::ymd("20120102"), "f", ausy)
#' adjust(lubridate::ymd("20120102"), "mf", ausy)
#' adjust(lubridate::ymd("20120102"), "p", ausy)
#' adjust(lubridate::ymd("20120102"), "mp", ausy)
#' adjust(lubridate::ymd("20120102"), "ms", ausy)
#' @export
#' @family calendar methods

adjust <- function(dates, bdc, calendar) {
        assertthat::assert_that(is_valid_bdc(bdc), assertthat::is.scalar(bdc))
        if (identical(bdc, "u")) return (dates)

        # Setup
        is_preceding <- grepl("p$", bdc)
        direction <- +1 * (!is_preceding) - 1 * is_preceding
        is_modified <- grepl("^m", bdc)

        # Helper functions. Compacts the code below
        is_over_month_end <- function(d1, d2) {
                lubridate::month(d1) != lubridate::month(d2)
        }
        is_over_mid_month <- function(d1, d2) {
                is_early <- lubridate::mday(d1) <= 15
                is_early_and_over <- is_early & lubridate::mday(d2) > 15
                is_late_and_over <- !is_early & is_over_month_end(d1, d2)
                is_early_and_over | is_late_and_over
        }
        if (identical(bdc, "ms")) {
                is_over_barrier <- is_over_mid_month
        } else {
                is_over_barrier <- is_over_month_end
        }
        adjuster <- function(direction, is_over_barrier) {
                function(dates, is_modified) {
                        dts <- dates
                        to_adjust <- !is_good(dts, calendar)
                        while (any(to_adjust)) {
                                dts <- dts + to_adjust * lubridate::days(direction * 1)
                                to_adjust <- !is_good(dts, calendar)
                        }
                        to_modify <- is_over_barrier(dates, dts)
                        if (is_modified & any(to_modify)) {
                                reverse_adjuster = adjuster(-direction, is_over_month_end)
                                dts[to_modify] <- reverse_adjuster(dates[to_modify], FALSE)
                        }
                        dts
                }
        }

        # Adjustments
        fn <- adjuster(direction, is_over_barrier)
        fn(dates, is_modified)
}

#' Shifting dates to good dates
#'
#' The [adjust()] function rolls dates to the closest good dates. This function
#' shifts dates by a given [period][lubridate::period()] and adjusting the
#' resulting dates to a closest good dates following the given business day
#' convention.
#'
#' @param dates a vector of dates to shift and adjust
#' @param period an atomic instance of the [period
#'   class][lubridate::Period-class] in the sense that only one of its slots
#'   should be non-zero. It must also only be a day, month or year period type.
#' @param bdc the business day convention used to roll the `dates` if necessary
#'   (default: "u" - unadjusted)
#' @param calendar an object that inherits from [`Calendar`] or
#'   [`JointCalendar`] which is used to determine the goodness of `dates`
#'   (default: `EmptyCalendar()`)
#' @param eom_rule if one of the `dates` is the last business day of the month,
#'   is being shifted by a month or year `period` and `eom_rule` is `TRUE` then
#'   the shifted date is also the last business day of the month
#'   (default: `TRUE`)
#' @return a vector of shifted dates
#' @examples
#' library(lubridate)
#' ausy <- AUSYCalendar()
#' shift(ymd("20120229"), months(1), "u", ausy, FALSE)
#' shift(ymd("20120229"), months(1), "u", ausy, TRUE)
#' @export
#' @family calendar methods

shift <- function(dates, period, bdc = "u", calendar = EmptyCalendar(),
        eom_rule = TRUE) {
        # Period should have length one (but might have multiple period types
        # in this)
        assertthat::assert_that(assertthat::is.scalar(period),
                is_valid_bdc(bdc))

        # Figure out which period types to loop
        ps <- methods::slotNames(period)
        # .shift only supports day, month and year slots
        ps_to_be_looped <- ps %in% c("day", "month", "year")
        is_non_zero_p <- Map(methods::slot, period,
                methods::slotNames(period)) != 0
        is_p_looped <- is_non_zero_p & ps_to_be_looped
        is_p_ignored<- is_non_zero_p & !ps_to_be_looped

        if (any(is_p_ignored)) {
                message("The following period values are not supported by shift and ",
                        "are ignored: ", paste0(ps[is_p_ignored], collapse = ', '))
        }

        if (!any(is_p_looped)) {
                # Shift dates by 0 periods (i.e. adjust to good days and exit thanks)
                return(adjust(dates, bdc, calendar))
        } else {
                res <- dates
                for (p in ps[is_p_looped]) {
                        recast_p <- lubridate::period(methods::slot(period, p), p)
                        res <- shift_single(res, recast_p, bdc, calendar, eom_rule)
                }
        }
        return(res)
}

shift_single = function (dates, period, bdc, calendar, eom_rule) {
        # Day shift
        if (identical(period_type(period), "day")) {
                # Setup temp and counter variables
                i <- rep(0, NROW(dates))
                shift_length <- period_length(period)
                sgn <- sign(shift_length)

                # Step through days to get to shift_length good days for each date
                is_not_done <- abs(i) < abs(shift_length)
                while (any(is_not_done)) {
                        dates[is_not_done] <- dates[is_not_done] + sgn * lubridate::days(1)
                        to_roll <- is_good(dates, calendar)
                        i[to_roll] <- i[to_roll] + sgn * 1
                        is_not_done <- abs(i) < abs(shift_length)
                }
                return(dates)
        }
        # Month and year shifts are treated the same: one year = twelve months
        # Given assertions at the start, these should be the only options left
        res <- lubridate::`%m+%`(dates, period)
        if (!eom_rule) {
                return(adjust(res, bdc, calendar))
        } else {
                are_last_good_days <- identical(dates, adjust(eom(dates), "p", calendar))
                are_less_31_days <- lubridate::mday(dates) <= 30
                eom_applies <- are_last_good_days & are_less_31_days
                if (any(eom_applies))
                        res[eom_applies] <- adjust(eom(res[eom_applies]), "p", calendar)
                return(res)
        }
}

#' Checks whether dates are last day of month
#'
#' This checks whether the \code{dates} provided are the last day of a
#' month.
#'
#' @param dates a vector of dates.
#' @return a logical vector
#' @examples
#' library("lubridate")
#' is_eom(ymd(20110228)) # TRUE
#' is_eom(ymd(20120229)) # TRUE
#' @export

is_eom <- function (dates) {
        unname(lubridate::days_in_month(dates) == lubridate::mday(dates))
}

#' The end of month date
#'
#' The \code{dates} are rounded to the end of their respective months.
#'
#' @param dates a vector of dates.
#' @return a date vector with the same class as \code{dates}
#' @examples
#' library("lubridate")
#' eom(ymd(20120203, 20140203))
#' @export

eom <- function (dates) {
        lubridate::mday(dates) <- lubridate::days_in_month(dates)
        dates
}

#' The years between two dates using the 30/360 day basis convention.
#'
#' This calculates the years between two dates using the 30/360 day basis
#' convention. This convention assumes that months consists of 30 days and
#' years consist of 360 calendar days.
#'
#' The day count is determined after making the following (ordered)
#' modifications:
#' \enumerate{
#'   \item If the start date is the 31st, set the start dates to the 30th.
#'   \item If the start date greater than the 29nd and the end date is the
#'         31st then set the end dates to the 30th.
#' }
#'
#' The year fraction is then calculated as:
#'
#' \deqn{\frac{(d_2 - d_1) + (m_2 - m_1) \times 30 + (y_2 - y_1)
#'  \times 30}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' This is also known as the bond basis.
#'
#' @param date1 A date-time object.
#' @param date2 A date-time object.
#' @return A numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods


# Counters ----------------------------------------------------------------


thirty_360 <- function (date1, date2) {
        if (identical(date1, date2)) return (0)

        dd1 <- lubridate::mday(date1)/1
        dd2 <- lubridate::mday(date2)/1
        mm1 <- lubridate::month(date1)
        mm2 <- lubridate::month(date2)
        yy1 <- lubridate::year(date1)
        yy2 <- lubridate::year(date2)

        is_dd1_30 <- (dd1 == 31)
        dd1[is_dd1_30] <- 30
        is_dd2_30 <- (dd2 == 31) & (dd1 > 29)
        dd2[is_dd2_30] <- 30

        ((yy2 - yy1) * 360 + (mm2 - mm1) * 30 + (dd2 - dd1)) / 360
}

#' The years between two dates using the 30/360 (US) day basis convention.
#'
#' This calculates the years between two dates using the 30/360 (US) day basis
#' convention. This convention assumes that months consists of 30 days and
#' years consist of 360 calendar days.
#'
#' The day count is determined after making the following (ordered)
#' modifications:
#' \enumerate{
#'   \item If both the start and end dates are the last day of February, set
#'         the end date to the 30th.
#'   \item If the start date is the last day of February, set the start date
#'         to the 30th.
#'   \item If the end date is the 31st and the start date is either the 30th or
#'         the 30th, set the end date to the 30th
#'   \item If the start date is the 31st, set the start date to the 30th.
#' }
#'
#' The year fraction is then calculated as:
#'
#' \deqn{\frac{(d_2 - d_1) + (m_2 - m_1) \times 30 + (y_2 - y_1)
#'  \times 30}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' This is also known as the EOM adjusted bond basis.
#'
#' @param date1 A date-time object.
#' @param date2 A date-time object.
#' @return A numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
thirty_360_us <- function (date1, date2)
{
        if (identical(date1, date2)) return (0)

        dd1 <- lubridate::mday(date1)/1
        dd2 <- lubridate::mday(date2)/1
        mm1 <- lubridate::month(date1)
        mm2 <- lubridate::month(date2)
        yy1 <- lubridate::year(date1)
        yy2 <- lubridate::year(date2)

        dd1_end_feb <- is_eom(date1) && lubridate::month(date1) == 2
        dd1[dd1_end_feb] <- 30

        both_end_feb <- dd1_end_feb &
                is_eom(date2) & lubridate::month(date2) == 2
        dd2[both_end_feb] <- 30

        is_both_30s <- (dd1 == 30 | dd1 == 31) & (dd2 == 31)
        dd2[is_both_30s] <- 30

        is_dd1_30 <- (dd1 == 31)
        dd1[is_dd1_30] <- 30

        ((yy2 - yy1) * 360 + (mm2 - mm1) * 30 + (dd2 - dd1)) / 360
}

#' The years between two dates using the 30/360 (EU) day basis convention.
#'
#' This calculates the years between two dates using the 30/360 (EU) day
#' basis convention. This convention assumes that months consists of 30 days
#' and years consist of 360 calendar days. This is also known as the 30/360
#' ICMA, Eurobond (ISDA 2006) and Special German basis.
#'
#' The day count is determined after making the following (ordered)
#' modifications:
#' \enumerate{
#'   \item If the start date is the 31st, set the start date to the 30th
#'   \item If the end date is 31st, set the end date to the 30th
#' }
#'
#' The year fraction is then calculated as:
#' \deqn{\frac{(d_2-d_1) + (m_2-m_1)\times 30 + (y_2-y_1)\times 360}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
thirty_360_eu <- function (date1, date2)
{
        dd1 <- lubridate::mday(date1)/1
        dd2 <- lubridate::mday(date2)/1
        mm1 <- lubridate::month(date1)
        mm2 <- lubridate::month(date2)
        yy1 <- lubridate::year(date1)
        yy2 <- lubridate::year(date2)

        is_31 <- dd1 == 31
        dd1[is_31] <- 30
        is_31 <- dd2 == 31
        dd2[is_31] <- 30

        ((yy2 - yy1) * 360 + (mm2 - mm1) * 30 + (dd2 - dd1)) / 360
}

#' The years between two dates using the 30E/360 ISDA day basis convention.
#'
#' This calculates the years between two dates using the 30E/360 ISDA day
#' basis convention. This convention assumes that months consists of 30 days
#' and years consist of 360 calendar days. This is also known as the Eurobond
#' (ISDA 2000) and German basis.
#'
#' The day count is determined after making the following (ordered)
#' modifications:
#' \enumerate{
#'   \item If the start date is the last day of the month, set the start date
#'          to the 30th
#'   \item If the end date is the last day of the month, set the end date to
#'          the 30th unless the end date is the maturity date and the end
#'          month is February
#' }
#'
#' The year fraction is then calculated as:
#' \deqn{\frac{(d_2-d_1) + (m_2-m_1)\times 30 + (y_2-y_1)\times 30}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @param maturity_date the maturity date of the instrument
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
thirty_360_eu_isda <- function (date1, date2, maturity_date)
{
        dd1 <- lubridate::mday(date1)/1
        dd2 <- lubridate::mday(date2)/1
        mm1 <- lubridate::month(date1)
        mm2 <- lubridate::month(date2)
        yy1 <- lubridate::year(date1)
        yy2 <- lubridate::year(date2)

        d1_eom <- eom(date1)
        d2_eom <- eom(date2)

        is_eom <- (date1 == d1_eom)
        dd1[is_eom] <- 30
        is_d2_eo_feb <- (date2 == d2_eom) & (date2 == maturity_date) & (mm2 == 2)
        dd2[is_d2_eo_feb] <- 30

        ((yy2 - yy1) * 360 + (mm2 - mm1) * 30 + (dd2 - dd1)) / 360
}

#' The years between two dates using the 30E+/360 day basis convention.
#'
#' This calculates the years between two dates using the 30E+/360 day
#' basis convention. This convention assumes that months consists of 30 days
#' and years consist of 360 calendar days.
#'
#' The day count is determined after making the following (ordered)
#' modifications:
#' \enumerate{
#'   \item If the start date is the 31st, set the start date to the 30th
#'   \item If the end date is the 31st, set the end date to the 1st of the
#'   following month.
#' }
#'
#' The year fraction is then calculated as:
#' \deqn{\frac{(d_2-d_1) + (m_2-m_1)\times 30 + (y_2-y_1)\times 360}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
thirty_360_eu_plus <- function (date1, date2)
{
        dd1 <- lubridate::mday(date1)/1
        dd2 <- lubridate::mday(date2)/1
        mm1 <- lubridate::month(date1)
        mm2 <- lubridate::month(date2)
        yy1 <- lubridate::year(date1)
        yy2 <- lubridate::year(date2)

        is_31 <- (dd1 == 31)
        dd1[is_31] <- 30
        is_31 <- (dd2 == 31)
        date2[is_31] <- lubridate::ceiling_date(date2[is_31], "month")
        dd2[is_31] <- lubridate::mday(date2[is_31])/1
        mm2[is_31] <- lubridate::month(date2[is_31])
        yy2[is_31] <- lubridate::year(date2[is_31])

        ((yy2 - yy1) * 360 + (mm2 - mm1) * 30 + (dd2 - dd1)) / 360
}

#' The years between two dates using the actual/360 day basis convention.
#'
#' This calculates the years between two dates using the actual/360 day basis
#' convention. This convention counts the number of calendars between the start
#' and end dates and assumes a year consists of 360 days. This is also known
#' as the A/360, Act/360 or French day basis convention.
#'
#' The year fraction is calculated as:
#' \deqn{\frac{Number of calendar days}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
actual_360 <- function (date1, date2) {
        as.numeric(date2 - date1) / 360
}


#' The years between two dates using the actual/365 (fixed) day basis
#' convention.
#'
#' This calculates the years between two dates using the actual/365 (fixed)
#' day  basis convention. This convention counts the number of calendars
#' between the start and end dates and assumes a year consists of 365 days.
#' This is also known as the Act/365 fixed, A/365 fixed, A/365F and English
#' day basis convention.
#'
#' The year fraction is calculated as:
#' \deqn{\frac{Number of calendar days}{365}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
actual_365 <- function (date1, date2) {
        as.numeric(date2 - date1) / 365
}


#' The years between two dates using the Actual/Actual ISDA day basis
#' convention
#'
#' This calculates the years between two dates using the Actual/Actual ISDA
#' day basis convention. This convention counts the number of calendars
#' between the start and end dates. The definition of a year is contingent on
#' whether the year is a leap year or not. The year fraction is calculated as:
#' \deqn{
#'   \frac{d_1}{{dy}_1} + \frac{d_2}{{dy}_2} + y_2 - y_1 - 1
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{d_1} is the remaining days in the first date's year including
#'         both the start and end date
#'   \item \eqn{d_2} is the number of days to the second date from the start
#'         of that year
#'   \item \eqn{{dy}_1} is 366 if the first date is in a leap year. Otherwise
#'         it is 365
#'   \item \eqn{{dy}_2} is 366 if the second date is in a leap year. Otherwise
#'         it is 365
#' }
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
actual_actual_isda <- function (date1, date2)
{
        dib1 <- vector("numeric", NROW(date1))
        dib2 <- vector("numeric", NROW(date2))

        yy1 <- lubridate::year(date1)
        yy2 <- lubridate::year(date2)
        is_date1_ly <- lubridate::leap_year(date1)
        is_date2_ly <- lubridate::leap_year(date2)
        dib1[is_date1_ly] <- 366
        dib1[!is_date1_ly] <- 365
        dib2[is_date2_ly] <- 366
        dib2[!is_date2_ly] <- 365
        bony1 <- as.Date(ISOdate(yy1 + 1, 1, 1, 0, 0, 0))
        boy2 <- as.Date(ISOdate(yy2, 1, 1, 0, 0, 0))
        yy2 - yy1 - 1 + as.numeric(bony1 - date1) / dib1 +
                as.numeric(date2 - boy2) / dib2
}

#' The years between two dates for a given day basis convention
#'
#' This calculates the years between two dates using the given day
#' basis convention.
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative. The parameters will be repeated with recycling
#' such that each parameter's length is equal to maximum length of
#' any of the parameters.
#'
#' @param date1 A vector of dates. This will be coerced to a \code{\link{Date}}
#' class.
#' @param date2 A vector of dates. This will be coerced to a \code{\link{Date}}
#' class.
#' @param day_basis The basis on which the year fraction is calculated. See
#' [is_valid_day_basis()]
#' @param maturity_date a vector of dates representing the maturity date of
#' the instrument. Only used for 30E/360 ISDA day basis.
#' @return a numeric vector representing the number of years between
#' \code{date1} and \code{date2}.
#' @examples
#' require(lubridate)
#' year_frac(ymd("2010-03-31"), ymd("2012-03-31"), "30/360us") # 2
#' year_frac(ymd("2010-02-28"), ymd("2012-03-31"), "act/360")  # 2.116667
#' year_frac(ymd("2010-02-28"), ymd("2012-03-31"), "act/365")  # 2.087671
#' year_frac(ymd("2010-02-28"), ymd("2012-03-31"), "act/actisda")  # 2.086998
#' @references \url{http://en.wikipedia.org/wiki/Day_count_convention}
#' @family counter methods
#' @export

yearfrac <- function(StartDate, EndDate, DayCountConv, maturity_date = NULL)
{
        # Check that day basis is valid
        assertthat::assert_that(is_valid_day_basis(DayCountConv))

        # Convert inputs to Date (time stamp details irrelevant to these calcs)
        StartDate <- lubridate::as_date(StartDate)
        EndDate <- lubridate::as_date(EndDate)

        # Make sure inputs are vectors of same length. This will allow us to
        # vectorise the calculation
        max_n_dates <- max(NROW(StartDate), NROW(EndDate))
        yrs <- vector("numeric", max_n_dates)

        StartDate <- rep(StartDate, length.out = max_n_dates)
        EndDate <- rep(EndDate, length.out = max_n_dates)
        DayCountConv <- rep(DayCountConv, length.out = max_n_dates)
        if(!is.null(maturity_date)){
                maturity_date <- rep(maturity_date, length.out = max_n_dates)
        }

        # Prep work
        to_reverse <- StartDate > EndDate
        if (any(to_reverse, na.rm = TRUE)) {
                yrs[to_reverse] <- -year_frac(EndDate[to_reverse], StartDate[to_reverse],
                        DayCountConv[to_reverse], maturity_date[to_reverse])
        }

        # Get year fraction
        is_30360 <- toupper(DayCountConv) == "30/360"
        is_30360us <- toupper(DayCountConv) == "30/360US"
        is_30e360 <- toupper(DayCountConv) == "30E/360"
        is_30e360isda <- toupper(DayCountConv) == "30E/360ISDA"
        is_30ep360 <- toupper(DayCountConv) == "30E+/360"
        is_act360 <- toupper(DayCountConv) == "ACT/360"
        is_act365 <- toupper(DayCountConv) == "ACT/365"
        is_actactisda <- toupper(DayCountConv) == "ACT/ACTISDA"

        if (any(is_30360))
                yrs[is_30360] <- thirty_360(StartDate[is_30360], EndDate[is_30360])

        if (any(is_30360us))
                yrs[is_30360us] <- thirty_360_us(StartDate[is_30360us], EndDate[is_30360us])

        if (any(is_30e360))
                yrs[is_30e360] <- thirty_360_eu(StartDate[is_30e360], EndDate[is_30e360])

        if (any(is_30e360isda))
                yrs[is_30e360isda] <- thirty_360_eu_isda(StartDate[is_30e360isda],
                        EndDate[is_30e360isda], maturity_date[is_30e360isda])

        if (any(is_30ep360))
                yrs[is_30ep360] <- thirty_360_eu_plus(StartDate[is_30ep360], EndDate[is_30ep360])

        if (any(is_act360))
                yrs[is_act360] <- actual_360(StartDate[is_act360], EndDate[is_act360])

        if (any(is_act365))
                yrs[is_act365] <- actual_365(StartDate[is_act365], EndDate[is_act365])

        if (any(is_actactisda))
                yrs[is_actactisda] <- actual_actual_isda(StartDate[is_actactisda],
                        EndDate[is_actactisda])

        # Return value
        yrs
}

#' Day basis conventions
#'
#' Checks whether day basis conventions are valid. Supported day basis
#' conventions are documented in [year_frac()]
#'
#' @param day_basis A character vector of day basis conventions.
#' @return will return `TRUE` for `day_basis` elements that are any of the
#'   following: \code{30/360}, \code{30/360us}, \code{30e/360},
#'   \code{30e/360isda}, \code{30e+/360}, \code{act/360}, \code{act/365} and
#'   \code{act/actisda}. Otherwise will return `FALSE`
#' @export
#' @examples
#' is_valid_day_basis(c("act/360", "act/365f"))
#' @aliases daybasisconventions
#' @family counter methods

is_valid_day_basis <- function (day_basis) {
        all(
                toupper(day_basis) %in% c(
                        "30/360US",
                        "30E/360",
                        "30E/360ISDA",
                        "30E+/360",
                        "ACT/360",
                        "ACT/365",
                        "ACT/ACTISDA",
                        "30/360"
                )
        )
}

# Schedule ----------------------------------------------------------------

#' Generate a date schedule
#'
#' Generate a date schedule from \code{effective_date} to
#' \code{termination_date}. This code was derived from the Quantlib method
#' Schedule::Schedule. This can be used to generate the cash flow, fixing and
#' projection dates of an interest rate swap according to certain conventions.
#'
#' @param effective_date the date at which the schedule begins. For example, the
#'   effective date of a swap. This should be \code{\link{POSIXct}}.
#' @param termination_date the date at which the schedule ends. For example, the
#'   termination date of a swap. This should be \code{\link{POSIXct}}.
#' @param tenor the frequency of the events for which dates are generated. For
#'   example, \code{month(3)} reflects events that occur quarterly. Should be an
#'   atomic \code{\link{Period-class}} of length one
#' @param first_date date of first payment for example. This defaults to
#'   \code{effective_date} as is usually the case
#' @param last_date date of last payment for example. This defaults to
#'   \code{termination_date} as is usually the case
#' @param calendar a \code{\link{Calendar}}
#' @param bdc a string representing one of the following business day
#'   conventions: "u", "f", "mf", "p", "mp", "ms" (unadjusted, following,
#'   modified following, preceding, modified preceding and modified succeeding,
#'   resp.)
#' @param stub a string representing one of the following stub types:
#'   "short_front", "short_back", "long_front", "long_back".
#' @param eom_rule a logical value defining whether the end-to-end convention
#'   applies.
#' @return an \code{Interval} vector
#' @examples
#' library (lubridate)
#' effective_date <- ymd('20120103')
#' termination_date <- ymd('20121203')
#' tenor <- months(3)
#' stub <- 'short_front'
#' bdc <- 'mf'
#' calendar <- AUSYCalendar()
#' eom_rule <- FALSE
#' generate_schedule(effective_date, termination_date, tenor, calendar,
#'  bdc, stub, eom_rule)
#' @export
#' @family calendar methods

generate_schedule <- function (effective_date, termination_date, tenor,
        calendar = EmptyCalendar(), bdc = "u", stub = "short_front", eom_rule = FALSE,
        first_date = effective_date, last_date = termination_date, imm_dates = FALSE)
{
        # Input checking
        assertthat::assert_that(effective_date <= first_date,
                first_date < last_date,
                last_date <= termination_date,
                is_valid_stub(stub), is_atomic_period(tenor))

        # Adjust end date if getting IMM date sequence
        if (identical(imm_dates, TRUE)){
                last_date <- next_imm(last_date)
                tenor <- months(3)
                stub <- "short_front"
        }

        # Set things up
        yrs <- year_frac(first_date, last_date, "act/365")
        suppressMessages(pa <- trunc(lubridate::years(1) / tenor))
        # Add some wiggle room
        n <- trunc(yrs * pa) + pa

        # Direction of schedule generation is backward if stub is front. Else forward
        sgn <- if (grepl("front$", stub)) -1 else +1

        # Build sequence of periods
        if (identical(sgn, -1)) {
                seed <- last_date
                period_number <- seq.int(sgn * n, 0)
        } else {
                seed <- first_date
                period_number <- seq.int(0, sgn * n)
        }

        # Generate unadjusted dates.
        if (any(period_type(tenor) %in% c("month", "year"))) {
                operator <- lubridate::`%m+%`
        } else {
                operator <- `+`
        }
        res <- operator(seed, period_number * tenor)

        # Only want unadjusted dates after the first_date which is greater than or
        # equal to the effective_date and those before the last date which is less
        # than or equal to the termination date.
        res <- res[(res >= first_date) & (res <= last_date)]

        # Add the effective / termination dates if not already there. This will
        # create a stub if that is the case. Note the assertion
        # ensures that effective_date <= first_date < last_date <= termination_date
        # so we can concatenate effective / termination dates as below.
        is_stub_created <- FALSE
        if (!(effective_date %in% res)) {
                is_stub_created <- TRUE
                # This needs the definition of c in date-methods.R for TZ to be preserved
                res <- c(effective_date, res)
        }
        if (!(termination_date %in% res)) {
                is_stub_created <- TRUE
                # This needs the definition of c in date-methods.R for TZ to be preserved
                res <- c(res, termination_date)
        }

        # EOM adjustments if necessary
        is_seed_eom <- identical(seed, adjust(eom(seed), "p", calendar))
        if (all(eom_rule, is_seed_eom)) {
                # Apply adjustments between first and last dates only (exclusive of both)
                is_eom_required <- (res > first_date) & (res < last_date)
                res[is_eom_required] <- adjust(eom(res[is_eom_required]), 'p', calendar)
        }

        # Adjust dates according to business day convention
        res <- adjust(res, bdc, calendar)

        # Check for stubs and remove relevant date when the stub is a long type.
        if (all(identical(stub, "long_back"),
                any(is_stub_created, !identical(last_date, termination_date)))) {
                res <- res[-(NROW(res) - 1)]
        }
        if (all(identical(stub, "long_front"),
                any(is_stub_created, !identical(first_date, effective_date)))) {
                res <- res[-2]
        }

        # Return!
        lubridate::interval(utils::head(res, -1), utils::tail(res, -1))
}

next_imm <- function(Date) {
        m <- lubridate::month(Date)
        y <- lubridate::year(Date)
        d <- lubridate::day(Date)

        if ((m %% 3 == 0) & (d > 20)) {
                m <- ceiling(x = m / 3) * 3 + 3
                if (m > 12) {
                        m <- m - 12
                        y <- y + 1
                }
        } else m <- ceiling(x = m / 3) * 3

        imm_d <- 20

        m <- ceiling(x = m / 3) * 3

        imm_date <- as.Date(ISOdate(y,m,imm_d,0,0,0))

        return(imm_date)
}


# Epochs ------------------------------------------------------------------

#' Easter Monday day of year
#'
#' Determine the day of the year that the Western Easter Monday falls on.
#'
#' @param years a numeric vector of \code{years}
#' @return a numeric vector
#' @keywords internal

easter_monday <- function (years)
{
        assertthat::assert_that(all(years >= 1901), all(years <= 2199))
        # day_number is defined in QuantLib 1.3.0.
        day_number <- c(
                98,  90, 103,  95, 114, 106,  91, 111, 102, # 1901-1909
                87, 107,  99,  83, 103,  95, 115,  99,  91, 111,   # 1910-1919
                96,  87, 107,  92, 112, 103,  95, 108, 100,  91,   # 1920-1929
                111,  96,  88, 107,  92, 112, 104,  88, 108, 100,   # 1930-1939
                85, 104,  96, 116, 101,  92, 112,  97,  89, 108,   # 1940-1949
                100,  85, 105,  96, 109, 101,  93, 112,  97,  89,   # 1950-1959
                109,  93, 113, 105,  90, 109, 101,  86, 106,  97,   # 1960-1969
                89, 102,  94, 113, 105,  90, 110, 101,  86, 106,   # 1970-1979
                98, 110, 102,  94, 114,  98,  90, 110,  95,  86,   # 1980-1989
                106,  91, 111, 102,  94, 107,  99,  90, 103,  95,   # 1990-1999
                115, 106,  91, 111, 103,  87, 107,  99,  84, 103,   # 2000-2009
                95, 115, 100,  91, 111,  96,  88, 107,  92, 112,   # 2010-2019
                104,  95, 108, 100,  92, 111,  96,  88, 108,  92,   # 2020-2029
                112, 104,  89, 108, 100,  85, 105,  96, 116, 101,   # 2030-2039
                93, 112,  97,  89, 109, 100,  85, 105,  97, 109,   # 2040-2049
                101,  93, 113,  97,  89, 109,  94, 113, 105,  90,   # 2050-2059
                110, 101,  86, 106,  98,  89, 102,  94, 114, 105,   # 2060-2069
                90, 110, 102,  86, 106,  98, 111, 102,  94, 114,   # 2070-2079
                99,  90, 110,  95,  87, 106,  91, 111, 103,  94,   # 2080-2089
                107,  99,  91, 103,  95, 115, 107,  91, 111, 103,   # 2090-2099
                88, 108, 100,  85, 105,  96, 109, 101,  93, 112,   # 2100-2109
                97,  89, 109,  93, 113, 105,  90, 109, 101,  86,   # 2110-2119
                106,  97,  89, 102,  94, 113, 105,  90, 110, 101,   # 2120-2129
                86, 106,  98, 110, 102,  94, 114,  98,  90, 110,   # 2130-2139
                95,  86, 106,  91, 111, 102,  94, 107,  99,  90,   # 2140-2149
                103,  95, 115, 106,  91, 111, 103,  87, 107,  99,   # 2150-2159
                84, 103,  95, 115, 100,  91, 111,  96,  88, 107,   # 2160-2169
                92, 112, 104,  95, 108, 100,  92, 111,  96,  88,   # 2170-2179
                108,  92, 112, 104,  89, 108, 100,  85, 105,  96,   # 2180-2189
                116, 101,  93, 112,  97,  89, 109, 100,  85, 105)    # 2190-2199
        day_number[years - 1900]
}

#' March and September equinox
#'
#' Determine the date/time of March and September Equinoxes.
#' This implements Jean Meeus' algorithm (Astronomical Algorithms
#' 1st Ed, 1991, Chapter 26).
#'
#' @inheritParams easter_monday
#' @param tz time zone for which equinox instant should be returned (default:
#' \code{"UTC"})
#' @param want_dt a flag indicating whether the returned date should be in
#' dynamical time (default: \code{FALSE})
#' @param season defines the Equinox sought. Can be \code{mar} (default) or
#' \code{sep}.
#' @return the date-time (UTC) of the Equinox (drop sub-minute precision)
#' @keywords internal

equinox <- function (years, season = 'mar', tz = "UTC", want_dt = FALSE)
{
        # Check inputs
        assertthat::assert_that(season %in% c('mar', 'sep'),
                all(years >= 1000), all(years <= 3000))

        # Algorithm from Jean-Meeus Astronomical Algorithms, Chapter 26
        # Table 26.B coefficients
        coef_mar <- c(2451623.80984, 365242.37404,  0.05169, -0.00411, -0.00057)
        coef_jun <- c(2451716.56767, 365241.62603,  0.00325, 0.00888, -0.00030)
        coef_sep <- c(2451810.21715, 365242.01767, -0.11575,  0.00337,  0.00078)

        # Table 26.C constants.
        A <- c(485, 203, 199, 182, 156, 136, 77, 74, 70, 58, 52, 50, 45, 44, 29, 18,
                17, 16, 14, 12, 12, 12, 9, 8)
        B <- c(324.96, 337.23, 342.08, 27.85, 73.14, 171.52, 222.54, 296.72, 243.58,
                119.81, 297.17, 21.02, 247.54, 325.15, 60.93, 155.12, 288.79, 198.04,
                199.76, 95.39, 287.11, 320.81, 227.73, 15.45)
        C <- c(1934.136, 32964.467, 20.186, 445267.112, 45036.886, 22518.443,
                65928.934, 3034.906, 9037.513, 33718.147, 150.678, 2281.226,
                29929.562, 31555.956, 4443.417, 67555.328, 4562.452, 62894.029,
                31436.921, 14577.848, 31931.756, 34777.259, 1222.114, 16859.074)

        # Mean time
        y <- (years - 2000) / 1000
        M <- cbind(rep(1, NROW(y)), y, y ^ 2, y ^ 3, y ^ 4)
        if (identical(season, 'mar')) {
                jde0 <- as.vector(M %*% coef_mar)
        } else if (identical(season, "jun")) {
                jde0 <- as.vector(M %*% coef_jun)
        } else {
                jde0 <- as.vector(M %*% coef_sep)
        }
        # Correction
        tt <- (jde0 - 2451545) / 36525
        w <- 35999.373 * tt - 2.47
        delta_lambda <- 1 + 0.0334 * cos(rads(w)) + 0.0007 * cos(rads(2 * w))
        s <- vector("numeric", length(tt))
        for (i in seq_along(s)) {
                s[i] <- Reduce(sum, Map(function (a, b, c, t) a * cos(rads(b + c * t)),
                        A, B, C, tt[i]))
        }
        # julian datetime in dynamical time (i.e. not UTC)
        jde <- jde0 + 0.00001 * s / delta_lambda
        jde_to_gregorian(jde, tz, want_dt)
}

#' Determine Gregorian date from Julian day
#'
#' @param julian_day a numeric vector
#' @return a POSIXct vector of Gregorian dates at midnight UTC time.
#' @keywords internal

julian_day_to_gregorian <- function (julian_day)
{
        # Jean-Meeus Astronomical Algorithms, Chapter 7, pg 63
        z <- trunc(julian_day + 0.5)
        f <- julian_day + 0.5 - z
        alpha <- trunc((z - 1867216.25) / 36524.25)
        a <- rep(NA, NROW(julian_day))
        a[z < 2299161] <- z
        a[!(z < 2299161)] <- z + 1 + alpha - trunc(alpha / 4)
        b <- a + 1524
        c <- trunc((b - 122.1) / 365.25)
        d <- trunc(365.25 * c)
        e <- trunc((b - d) / 30.6001)
        dom <- b - d - trunc(30.6001 * e) + f
        f_dom <- dom - trunc(dom)
        m1 <- e - 1
        m2 <- rep(0, NROW(e))
        m2[e == 14 | e == 15] <-  (e - 13)[e == 14 | e == 15]
        m <- m1 * (e < 14) + m2 * (e >= 14)
        m1 <- c - 4716
        m2 <- rep(0, NROW(m))
        m2[m == 1 | m == 2] <- (c - 4715)[m == 1 | m == 2]
        y <- m1 * (m > 2) + m2 * (m <= 2)
        # reproduce same output as default values of lubridate::ymd()
        ISOdate(y, m, trunc(dom), hour = 0, tz = 'UTC') + f_dom * 24 * 60 * 60
}

jde_to_gregorian <- function (jde, tz = "UTC", want_dt = FALSE) {
        res <- julian_day_to_gregorian(jde)
        if (!want_dt) {
                # Meeus pg 71 has Delta T = TD - UT.
                res <- lubridate::floor_date(res - lubridate::seconds(delta_t(res)),
                        unit = "minute")

        }
        if (tz != "UTC") {
                return (lubridate::with_tz(res, tz = tz))
        } else {
                return (res)
        }
}

to_jd <- function (dates) {
        # Only gregorian calendar is supported.
        dates_utc <- lubridate::with_tz(dates, tzone = "UTC")
        y <- lubridate::year(dates_utc)
        m <- lubridate::month(dates_utc)
        dom <- lubridate::mday(dates_utc)
        if (!lubridate::is.Date(dates)) {
                f_dom <- as.numeric((dates_utc - ISOdate(y, m, dom, hour = 0, tz = "UTC")) / 24)
        } else {
                f_dom <- rep(0, length(dates))
        }
        dom <- dom + f_dom
        y[m <= 2] <- y[m <= 2] - 1
        m[m <= 2] <- m[m <= 2] + 12
        a <- trunc(y / 100)
        is_greg <- is_gregorian(dates)
        b <- vector("numeric", length(a))
        b[is_greg] <- 2 - a + trunc(a / 4)
        b[!is_greg] <- 0
        trunc(365.25 * (y + 4716)) + trunc(30.6001 * (m + 1)) + dom + b - 1524.5
}

is_gregorian <- function (dates) {
        cutoff <- lubridate::ymd(15821004)
        if (lubridate::is.POSIXt(dates)) {
                dates <- lubridate::as_date(dates)
        }
        dates >= cutoff
}

rads <- function (degs) {
        degs * pi / 180
}

delta_t <- function (dates) {
        assertthat::assert_that(lubridate::is.POSIXt(dates))
        dates <- lubridate::with_tz(dates, "UTC")
        # See: http://maia.usno.navy.mil/ser7/tai-utc.dat
        # Ten leap seconds before start of data contained in .leap.seconds
        # http://www.usno.navy.mil/USNO/earth-orientation/eo-info/general/date-time-def/date-and-time-definitions
        n_leap_seconds <- Map(function (x)
                lubridate::with_tz(.leap.seconds, "UTC") <= x, dates)
        n_leap_seconds <- vapply(n_leap_seconds, sum, integer(1))
        # Observed:
        # http://maia.usno.navy.mil/ser7/deltat.data
        # Quick comparison to observed suggests this code is correct to within a
        # second or two.
        32.184 + (10 + n_leap_seconds)
}

moon_phase_delta <- function (phase) {
        phase_map <- function (x) {
                switch(x, "new" = 0.0, "first" = 0.25, "full" = 0.50, "last" = 0.75)
        }
        vapply(phase, phase_map, numeric(1), USE.NAMES = FALSE)
}

mean_moon_phase <- function (k, phase) {
        # Chapter 47, Meeus
        assertthat::assert_that(
                all(k %% 1 == 0),
                assertthat::is.string(phase), phase %in% c("new", "first", "full", "last")
        )
        k <- k + moon_phase_delta(phase)
        tt <- k / 1236.85
        2451550.09765 +
                29.530588853 * k +
                0.0001337 * tt ^ 2 -
                0.000000150 * tt ^ 3 +
                0.00000000073 * tt ^ 4
}

true_moon_phase <- function (k, phase) {
        # Chapter 47, Meeus
        assertthat::assert_that(
                all(k %% 1 == 0),
                assertthat::is.string(phase), phase %in% c("new", "first", "full", "last")
        )
        jde <- mean_moon_phase(k, phase)
        k <- k + moon_phase_delta(phase)
        tt <- k / 1236.85
        ## Lunisolar corrections (47.4 - 47.7)
        ee <- 1 - 0.002516 * tt - 0.0000074 * tt ^ 2
        mm <- rads((2.5534 + 29.10535669 * k - 0.0000218 * tt ^ 2 -
                        0.00000011 * tt ^ 3) %% 360)
        mmd <- rads((201.5643 + 385.81693528 * k + 0.0107438 * tt ^ 2 +
                        0.00001239 * tt ^ 3 - 0.000000058 * tt ^ 4) %% 360)
        ff <- rads((160.7108 + 390.67050274 * k - 0.0016341 * tt ^ 2 -
                        0.00000227 * tt ^ 3 + 0.000000011 * tt ^ 4) %% 360)
        omega <- rads((124.7746 - 1.56375580 * k + 0.0020691 * tt ^ 2 +
                        0.00000215 * tt ^ 3) %% 360)
        new_moon_constant <-
                c(
                        -0.40720,
                        +0.17241,
                        +0.01608,
                        +0.01039,
                        +0.00739,
                        -0.00514,
                        +0.00208,
                        -0.00111,
                        -0.00057,
                        +0.00056,
                        -0.00042,
                        +0.00042,
                        +0.00038,
                        -0.00024,
                        -0.00017,
                        -0.00007,
                        +0.00004,
                        +0.00004,
                        +0.00003,
                        +0.00003,
                        -0.00003,
                        +0.00003,
                        -0.00002,
                        -0.00002,
                        +0.00002
                )
        full_moon_constant <-
                c(
                        -0.40614,
                        +0.17302,
                        +0.01614,
                        +0.01043,
                        +0.00734,
                        -0.00515,
                        +0.00209,
                        -0.00111,
                        -0.00057,
                        +0.00056,
                        -0.00042,
                        +0.00041,
                        +0.00038,
                        -0.00024,
                        -0.00017,
                        -0.00007,
                        +0.00004,
                        +0.00004,
                        +0.00003,
                        +0.00003,
                        -0.00003,
                        +0.00003,
                        -0.00002,
                        -0.00002,
                        +0.00002
                )
        qtr_phase_constant <-
                c(
                        -0.62801,
                        +0.17172,
                        -0.01183,
                        +0.00862,
                        +0.00804,
                        +0.00454,
                        +0.00204,
                        -0.00180,
                        -0.0007,
                        -0.0004,
                        -0.00034,
                        +0.00032,
                        +0.00032,
                        -0.00028,
                        +0.00027,
                        -0.00017,
                        -0.00005,
                        +0.00004,
                        -0.00004,
                        +0.00004,
                        +0.00003,
                        +0.00003,
                        +0.00002,
                        +0.00002,
                        -0.00002
                )
        one_n <- rep(1, length(ee))
        new_full_phase_coefficients_a <- matrix(c(one_n, ee, one_n, one_n, ee, ee,
                ee ^ 2, one_n, one_n, ee, one_n, ee, ee, ee, rep(one_n, 11)),
                ncol = 25)
        new_full_phase_coefficients_b <- sin(c(mmd, mm, 2 * mmd, 2 * ff, mmd - mm,
                mmd + mm, 2 * mm, mmd - 2 * ff, mmd + 2 * ff, 2 * mmd + mm, 3 * mmd,
                mm + 2 * ff, mm - 2 * ff, 2 * mmd - mm, omega, mmd + 2 * mm,
                2 * mmd - 2 * ff, 3 * mm, mmd + mm - 2 * ff, 2 * mmd + 2 * ff,
                mmd + mm + 2 * ff, mmd - mm + 2 * ff, mmd - mm - 2 * ff, 3 * mmd + mm,
                4 * mmd))
        new_full_phase_coefficients_b <-
                matrix(new_full_phase_coefficients_b, ncol = 25)
        new_full_phase_coefficients <- new_full_phase_coefficients_a *
                new_full_phase_coefficients_b
        qtr_phase_coefficients_a <- c(one_n, ee, ee, one_n, one_n, ee, ee ^ 2, one_n,
                one_n, one_n, ee, ee, ee, ee ^ 2, ee, rep(one_n, 10))
        qtr_phase_coefficients_a <- matrix(qtr_phase_coefficients_a, ncol = 25)
        qtr_phase_coefficients_b <- sin(c(mmd, mm, mmd + mm, 2 * mmd, 2 * ff,
                mmd - mm, 2 * mm, mmd - 2 * ff, mmd + 2 * ff, 3 * mmd, 2 * mmd - mm,
                mm + 2 * ff, mm - 2 * ff, mmd + 2 * mm, 2 * mmd + mm, omega,
                mmd - mm - 2 * ff, 2 * mmd + 2 * ff, mmd + mm + 2 * ff, mmd - 2 * mm,
                mmd + mm - 2 * ff, 3 * mm, 2 * mmd - 2 * ff, mmd - mm + 2 * ff,
                3 * mmd + mm))
        qtr_phase_coefficients_b <- matrix(qtr_phase_coefficients_b, ncol = 25)
        qtr_phase_coefficients <- qtr_phase_coefficients_a * qtr_phase_coefficients_b
        ww <- 0.00306 - 0.00038 * ee * cos(mm) + 0.00026 * cos(mmd) -
                0.00002 * cos(mmd - mm) + 0.00002 * cos(mmd + mm) + 0.00002 * cos(2 * ff)
        ## Planetary corrections
        planetary_constants <- c(325, 165, 164, 126, 110, 62, 60, 56, 47, 42, 40, 37,
                35, 23) / 1e6
        a1 <- rads((299.77 + 0.107408 * k - 0.009173 * tt ^ 2) %% 360)
        a2 <- rads((251.88 + 0.016321 * k) %% 360)
        a3 <- rads((251.83 + 26.651886 * k) %% 360)
        a4 <- rads((349.42 + 36.412478 * k) %% 360)
        a5 <- rads((84.66 + 18.206239 * k) %% 360)
        a6 <- rads((141.74 + 53.303771 * k) %% 360)
        a7 <- rads((207.14 + 2.453732 * k) %% 360)
        a8 <- rads((154.84 + 7.306860 * k) %% 360)
        a9 <- rads((34.52 + 27.261239 * k) %% 360)
        a10 <- rads((207.19 + 0.121824 * k) %% 360)
        a11 <- rads((291.34 + 1.844379 * k) %% 360)
        a12 <- rads((161.72 + 24.198154 * k) %% 360)
        a13 <- rads((239.56 + 25.513099 * k) %% 360)
        a14 <- rads((331.55 + 3.592518 * k) %% 360)
        planetary_coefficients <- sin(c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
                a12, a13, a14))
        planetary_coefficients <- matrix(planetary_coefficients, ncol = 14)
        if (phase == "new") {
                jde <- jde + new_full_phase_coefficients %*% new_moon_constant
        } else if (phase == "full") {
                jde <- jde + new_full_phase_coefficients %*% full_moon_constant
        } else if (phase == "first") {
                jde <- jde + qtr_phase_coefficients %*% qtr_phase_constant + ww
        } else {
                jde <- jde + qtr_phase_coefficients %*% qtr_phase_constant - ww
        }
        drop(jde + planetary_coefficients %*% planetary_constants)
}

next_moon_phase <- function (dates, phase, tz = "UTC", want_dt = FALSE) {
        assertthat::assert_that(
                assertthat::is.string(phase), phase %in% c("new", "first", "full", "last"),
                assertthat::is.flag(want_dt),
                assertthat::is.string(tz)
        )
        if (lubridate::is.Date(dates)) {
                dates <- as.POSIXct(dates)
        }
        jde0 <- to_jd(dates + lubridate::seconds(delta_t(dates)))
        yrs <- (lubridate::year(dates) + lubridate::yday(dates) / 365 - 2000)
        k <- floor(yrs * 12.3685)
        guess <- mean_moon_phase(k, phase)
        is_not_after <- jde0 > guess
        while (any(jde0 > guess)) {
                k[is_not_after] <- k[is_not_after] + 1
                guess[is_not_after] <- mean_moon_phase(k[is_not_after], phase)
        }
        res <- true_moon_phase(k, phase)
        jde_to_gregorian(res, tz, want_dt)
}

chinese_new_year <- function (years) {
        # https://en.wikipedia.org/wiki/Chinese_New_Year
        soy <- as.Date(paste(years, "01", "21", sep = "-"))
        next_moon_phase(soy, "new", tz = "Asia/Shanghai")
}


# Checkers ----------------------------------------------------------------


is_atomic_period <- function (period) {
        assertthat::assert_that(is(period, "Period"))
        sum(Map(methods::slot, period, methods::slotNames(period)) != 0) == 1
}

is_valid_bdc <- function (bdc) {
        all(bdc %in% c('u', 'f', 'mf', 'p', 'mp', 'ms'))
}

period_type <- function (period) {
        assertthat::assert_that(is_atomic_period(period))
        Find(function (x) methods::slot(period, x) != 0, methods::slotNames(period))
}

period_length <- function (period) {
        methods::slot(period, period_type(period))
}

#' Convert a numeric Tenor to a String
#'
#' @param tenor
#'
#' @return
#' @export
#'
#' @examples
tenor_to_string <- function(tenor){
        if (tenor == "1") {
                Tenor <- "m"
        }
        if (tenor == "3") {
                Tenor <- "q"
        }
        if (tenor == "6") {
                Tenor <- "sa"
        }
        if (tenor == "12") {
                Tenor <- "a"
        }
        return(Tenor)
}

