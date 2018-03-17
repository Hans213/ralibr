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
                K <- as.integer(dim(vols)[2] - 1)

                param_calibrated <- list()
                param_initial <- c(0.004, 0.02)

                # Create a matrix of Booleans to set subset to calibrate
                boolean_subset_matrix <- matrix(data = TRUE,nrow = N,ncol = K)

                swaption_data_subset_vector <-
                        lapply(
                                X = swaption_data,
                                FUN = function(x)
                                        x[boolean_subset_matrix]
                        )

                param_calibrated <-
                        GenSA::GenSA(
                                par = param_initial,
                                fn = fit_surface,
                                swaption_data_subset_vector,
                                lower = c(0, 0),
                                upper = c(0.06, 0.06),
                                control = list(threshold.stop = 0.01, max.time = 10 * 60)
                        )

                # Method 2: Co-terminals
                # Co-Terminal : matrix(data = rep(x = as.numeric(x = data$Vols[,1]),
                # 12),
                # ncol = 12) + as.numeric(colnames(x = data$Vols[,-1]))==30


                return(param_calibrated)

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

                # Parse valdate
                valdate <- parse_date_internal(DateToParse = valdate, DateType = "European")

                # Create a calendar
                bizdays::create.calendar("Actual", weekdays = c("saturday", "sunday"))

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
                                                DateToParse = roll_month(
                                                        Date = StartDate,
                                                        Offset = 12 * as.numeric(names(vols)[j])
                                                ),
                                                DateType = "European"
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

                                ATMStrikes[i, j] <-
                                        Output[[1]] # ATM Strike is the Forward Swap Rate

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

                Output <-
                        list(vols, Prices, ATMStrikes, Annuity)#, Dates2)
                names(Output) <-
                        c("Vols", "Prices", "StrikesATM", "Annuities")#, "Dates")

                return(Output)
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
fit_surface <- function(parameters, swaption_data) {

        # swaption_data will be a vector due to subsetting in calling function

        # Dimensions
        N <- dim(swaption_vols)[1]
        K <- dim(swaption_vols)[2] - 1

        # Pre-allocate matrix
        swaption_prices_calculated <-
                matrix(data = NA,
                        nrow = N,
                        ncol = K)

        # Calculate all prices
        for (i in 1:N) {
                for (j in 1:K) {
                        swaption_prices_calculated[i, j] <- gurrieri_payer_swaption_closed_form()
                }
        }

}

#' Title
#'
#' @param Strike
#' @param ValDate
#' @param TenorCurve
#' @param SwaptionDates
#' @param Tenor
#' @param Expiry
#' @param Notional
#' @param Alpha
#' @param S
#'
#' @return
#' @export
#'
#' @examples
gurrieri_payer_swaption_closed_form <-
        function(Strike,
                ValDate,
                TenorCurve,
                SwaptionDates,
                Tenor,
                Expiry,
                Notional = 1000000,
                Alpha,
                S) {
                ## Function calculates Swaption Price in closed form based on Gurrieri et al.
                ## Takes as input a list of all Swaption Dates
                ## Strike is the ATM Strike
                ## Alpha and S are the Hull-White parameters


                # First select relevant Dates
                Dates <-
                        as.data.frame(GetSwaptionDates(
                                Dates = SwaptionDates,
                                Tenor = Tenor,
                                Expiry = Expiry
                        )[[1]])

                # Dates (Mind the 2 day lag after expiry)
                SwapStart <- Dates[1, 1]
                OptionExpiry <-
                        offset(dates = SwapStart, # Changes to 'shift' when merging
                                n = -2,
                                cal = "Actual")
                SwapPayDates <- Dates[, 2]
                N <- length(SwapPayDates)
                SwapEnd <- SwapPayDates[N]

                # Maturities
                T.i <-
                        yearfrac(
                                DateBegin = SwapStart,
                                DateEnd = SwapPayDates,
                                DayCountConv = "act/360"
                        )
                t.i <-
                        yearfrac(
                                DateBegin = ValDate,
                                DateEnd = SwapPayDates,
                                DayCountConv = "act/360"
                        )
                t.0 <-
                        yearfrac(
                                DateBegin = ValDate,
                                DateEnd = OptionExpiry,
                                DayCountConv = "act/360"
                        )

                # ZCB (P Values)
                P0.t0 <-
                        interpolate(X = TenorCurve[,1], Y = TenorCurve[,2], x = SwapStart, method = "cs")
                P0.ti <-
                        interpolate(X = TenorCurve[,1], Y = TenorCurve[,2], x = as.numeric(TenorCurve[1, 1]) + t.i * 360,
                                method = "cs"
                        )

                # Coupon vector
                C <-
                        Strike * yearfrac(
                                DateBegin = Dates[, 1],
                                DateEnd = Dates[, 2],
                                DayCountConv = "act/360"
                        )
                C[N] <- C[N] + 1

                # B Value
                B <- (1 / Alpha) * (1 - exp(-Alpha * T.i))

                # Instant Fwd Rate
                Inst_Fwd <-
                        instant_forward(Curve = TenorCurve,
                                t = t.0,
                                delta = 0.001)


                # Variance
                V_r <- V_r(
                        s = 0,
                        t = t.0,
                        a = Alpha,
                        S = S
                )

                # A Value
                A <- log(P0.ti / P0.t0) + B * Inst_Fwd - 0.5 * B ^ 2 * V_r

                # Bisection search for r* (Solve instead?)
                threshold <- 0.0001
                max_iter <- 30
                rMin <- -0.9
                rMax <- 0.9
                diff <- 100
                i <- 0

                while ((abs(diff) > threshold) & (i < max_iter)) {
                        rTest <- 0.5 * (rMin + rMax)
                        diff <- 1 - sum(C * exp(A - rTest * B))
                        if (diff >= 0) {
                                rMax = rTest
                        }
                        if (diff < 0) {
                                rMin = rTest
                        }
                        i = i + 1
                }
                r_star <- rTest

                # Strike vector
                X.i <- exp(A - B * r_star)

                # Variance
                V_p <- (V_r * B ^ 2)


                gurrieri_payer_swaption_closed_form <-
                        sum(Notional * ZBP(
                                V = V_p,
                                P1 = P0.ti,
                                P2 = P0.t0,
                                X = X.i
                        ) * C)

                return(gurrieri_payer_swaption_closed_form)
        }

#' Function calculates Integral (George's function)
#'
#' @param s
#' @param t
#' @param a
#' @param S
#'
#' @return
#' @export
#'
#' @examples
V_r <- function(s, t, a, S) {
        # Fix Sigma$ stuff
        if (t == s) {
                V_r = 0
        }
        if (t < s) {
                s_1 <- S[, 1][which.max(t <= S[, 1])]
                s_n_1 <- S[, 1][max(which.max(s <= S[, 1]) - 1, 1)]
                s_n <- S[, 1][which.max(s <= S[, 1])]

                if (s_1 > s_n_1) {
                        V_r <-
                                -exp(-2 * a * t) * (1 / (2 * a)) * (S[, 2][match(s_1, S[, 1])]) ^ 2 * (exp(2 *
                                                a * s) - exp(2 * a * t))
                }

                if (s_1 <= s_n_1) {
                        Start_integral <-
                                -exp(-2 * a * t) * (1 / (2 * a)) * (S[, 2][match(s_1, S[, 1])]) ^ 2 * (exp(2 *
                                                a * s_1) - exp(2 * a * t))
                        End_integral <-
                                -exp(-2 * a * t) * (1 / (2 * a)) * (S[, 2][match(s_n, S[, 1])]) ^ 2 *
                                (exp(2 * a * s) - exp(2 * a * s_n_1))
                        V_r = Start_integral + End_integral
                        sequence <-
                                S[, 1][(s_1 <= S[, 1]) & (s_n_1 >= S[, 1])]
                        n <- length(sequence)
                        if (length(sequence) > 0) {
                                for (k in 1:(n - 1)) {
                                        s_k = sequence[k]
                                        s_kp1 = sequence[k + 1]
                                        V_r = V_r - exp(-2 * a *
                                                        t) * (1 / (2 * a)) * (S[, 2][match(s_kp1, S[, 1])]) ^ 2 * (exp(2 * a * s_kp1) -
                                                                        exp(2 * a * s_k))
                                }
                        }
                }
        }

        if (t > s) {
                s_1 <- S[, 1][which.max(s <= S[, 1])]
                s_n_1 <- S[, 1][max(which.max(t <= S[, 1]) - 1, 1)]
                s_n <- S[, 1][which.max(t <= S[, 1])]
                if (s_1 > s_n_1) {
                        V_r <-
                                exp(-2 * a * t) * (1 / (2 * a)) * (S[, 2][match(s_1, S[, 1])]) ^ 2 * (exp(2 *
                                                a * t) - exp(2 * a * s))
                }

                if (s_1 <= s_n_1) {
                        Start_integral <-
                                exp(-2 * a * t) * (1 / (2 * a)) * (S[, 2][match(s_1, S[, 1])]) ^ 2 * (exp(2 *
                                                a * s_1) - exp(2 * a * s))
                        End_integral <-
                                exp(-2 * a * t) * (1 / (2 * a)) * (S[, 2][match(s_n, S[, 1])]) ^ 2 * (exp(2 *
                                                a * t) - exp(2 * a * s_n_1))
                        V_r = Start_integral + End_integral
                        sequence <-
                                S[, 1][(s_1 <= S[, 1]) & (s_n_1 >= S[, 1])]
                        n <- length(sequence)
                        if (length(sequence) > 1) {
                                for (k in 1:(n - 1)) {
                                        s_k = sequence[k]
                                        s_kp1 = sequence[k + 1]
                                        V_r = V_r + exp(-2 * a *
                                                        t) * (1 / (2 * a)) * (S[, 2][match(s_kp1, S[, 1])]) ^ 2 * (exp(2 * a * s_kp1) -
                                                                        exp(2 * a * s_k))
                                }
                        }
                }
        }
        return(V_r)
}

#' ZBP Function
#'
#' @param V
#' @param P1
#' @param P2
#' @param X
#'
#' @return
#' @export
#'
#' @examples
ZBP <- function(V, P1, P2, X) {
        d1 <- -(1 / sqrt(V)) * log(P1 / P2 * X) + 0.5 * sqrt(V)
        d2 <- -(1 / sqrt(V)) * log(P1 / P2 * X) - 0.5 * sqrt(V)
        ZBP <-
                X * P2 * pnorm(q = d1,
                        mean = 0,
                        sd = 1) - P1 * pnorm(q = d2,
                                mean = 0,
                                sd = 1)
        return(ZBP)
}
