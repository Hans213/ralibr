#' Calibration of one-factor Hull-White parameters from Swaption volatilies
#'
#' @param valdate
#' @param dsc_cv
#' @param fwd_cv
#' @param vols
#'
#' @return
#' @export
#'
#' @examples
calibrate_hull_white_swaption <-
        function(valdate, cvDiscount, cvTenor, vols) {
                # Convert the vols
                swaption_data <-
                        convert_swaption_vols(
                                vols = vols,
                                cvTenor = cvTenor,
                                cvDiscount = cvDiscount,
                                valdate = valdate
                        )

                # Method 1: Calibrate 1 Alpha and 1 Sigma over the entire Cube
                N <- dim(vols)[1]
                K <- dim(vols)[2]-1

                param_calibrated <- list()
                param_initial <- c(0.004,0.02)

                param_calibrated <-
                        GenSA(
                                par = param_initial,
                                fn = fit_surface,
                                lower = c(0, 0),
                                upper = c(0.06, 0.06),
                                control = list(threshold.stop = 0.01, max.time = 10 * 60)
                        )




                return(param_calibrated)

                return(swaption_data)

        }

#' Title
#'
#' @param parameters
#' @param swaption_data
#'
#' @return
#' @export
#'
#' @examples
fit_surface <- function(parameters, swaption_data){
        # Extract frames from the list
        swaption_vols <- swaption_data$Vols
        swaption_prices <- swaption_data$Prices
        swaption_strikes <- swaption_data$StrikesATM
        swaption_annuities <- swaption_data$Annuities
        swaption_dates <- swaption_data$Dates

        # Dimensions
        N <- dim(swaption_vols)[1]
        K <- dim(swaption_vols)[2]-1

        # Pre-allocate matrix
        swaption_prices_calculated <- matrix(data = NA,nrow = N,ncol = K)

        # Calculate all prices
        for (i in 1:N){
                for(j in 1:K){
                        swaption_prices_calculated[i,j] <- payer_swaption_closed_form()
                }
        }

}

#' Title
#'
#' @param vols
#' @param cvTenor
#' @param cvDiscount
#' @param valdate
#'
#' @return
#'
#' @importFrom bizdays create.calendar
#' @importFrom bizdays offset
#'
#'
#' @examples
convert_swaption_vols <-
        function(vols,
                cvTenor,
                cvDiscount,
                valdate) {

                # Dimensions of the Swaption Vol Cube
                N <- dim(vols)[1]
                M <- dim(vols)[2]

                # Pre-allocate matrices
                ATMStrikes <- matrix(nrow = N, ncol = M)
                Annuity <- matrix(nrow = N, ncol = M)
                Prices <- matrix(nrow = N, ncol = M)

                # Swaption Tenor from the input (!)
                Tenor <-
                        substr(x = names(vols)[1],
                                start = 4,
                                stop = 4)

                # Convert Tenor to a string
                TenorString <- tenor_to_string(tenor = Tenor)

                # Pre-allocate list of Date combinations (Expiry-Tenors)
                Dates = list()

                # Parse valdate
                valdate <-
                        parse_date_internal(DateToParse = valdate, DateType = "European")

                # Create a calendar
                create.calendar("Actual", weekdays = c("saturday", "sunday"))

                # We loop over the rows and columns and convert the Vols
                for (i in 1:N) { # Loops over rows
                        for (j in 2:M) { # Loops over columns
                                # Name of the "Expiry - Tenor" Identifier
                                Name <-
                                        paste0(vols[i, 1],
                                                "-",
                                                names(vols)[j])

                                # Starting and Ending Date of the underlying Swap
                                StartDate <-
                                        roll_month(Date = valdate,
                                                Offset = 12 * vols[i, 1]) # Option Expiry in MONTHS
                                StartDate <-
                                        parse_date_internal(DateToParse = roll_weekday(Day = StartDate,
                                                BusDayConv = "mf"), # Business Day Convention
                                                DateType = "European")
                                StartDate <-
                                        offset(dates = StartDate,
                                                n = 2,
                                                cal = "Actual") # Spot lag of 2 days from Option expiry
                                EndDate <-
                                        parse_date_internal(
                                                DateToParse = roll_month(Date = StartDate,
                                                        Offset = 12 * as.numeric(names(vols)[j])),
                                                DateType = "European"
                                        )

                                Dates[[Name]] <-
                                        generate_dates(
                                                StartDate = StartDate,
                                                EndDate = EndDate,
                                                ValDate = valdate,
                                                CouponFreq = TenorString,
                                                BusDayConv = "mf",
                                                Output = "Frame"
                                        )


                                Output <-
                                        forward_swap_rate(
                                                StartDate = StartDate,
                                                EndDate = EndDate,
                                                ValDate = valdate,
                                                cvDiscount = cvDiscount,
                                                cvTenor = cvTenor,
                                                FloatingFreq = TenorString
                                        )

                                ATMStrikes[i, j] <- Output[[1]] # ATM Strike is the Forward Swap Rate
                                Annuity[i, j] <- Output[[2]]

                                # Calculate Swaption Price
                                Prices[i, j] <-
                                        value_swaption(
                                                FwdRate = Output[[1]],
                                                Strike = Output[[1]], # All ATM Swaptions
                                                Vol = vols[i, j],
                                                T = as.numeric(vols[i, 1]), # Options Expiry
                                                Annuity = Output[[2]],
                                                Notional = 1000000,
                                                Shift = 0,
                                                model = "BACH"
                                        )
                        }
                }

                # Format output
                ATMStrikes[, 1] <- vols[, 1]
                ATMStrikes <- as.data.frame(ATMStrikes)
                names(ATMStrikes) <- names(vols)
                Annuity[, 1] <- vols[, 1]
                Annuity <- as.data.frame(Annuity)
                names(Annuity) <- names(vols)
                Prices[, 1] <- vols[, 1]
                Prices <- as.data.frame(Prices)
                names(Prices) <- names(vols)

                Output <- list(vols, Prices, ATMStrikes, Annuity, Dates)
                names(Output) <- c("Vols", "Prices", "StrikesATM", "Annuities", "Dates")

                return(Output)
        }
