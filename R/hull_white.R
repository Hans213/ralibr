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
        function(cvDiscount, cvTenor, vols, method = "method_1") {

                # ValDate is from Curve
                valdate <- cvDiscount[1,1]

                if(valdate!=cvTenor[1,1]){stop("Curves do not have the same Valuation Date")}

                # Convert the vols
                # This is inefficient
                # 1) compared to the conversion we had earlier
                # 2) we convert the entire cube and will only use a subset based on the method
                swaption_data <-
                        convert_swaption_vols(
                                vols = vols,
                                cvTenor = cvTenor,
                                cvDiscount = cvDiscount,
                                valdate = valdate
                        )

                if(method=="method_1"){
                        output <- calibrate_method_1(swaption_data = swaption_data)
                }
                if(method=="method_2"){
                        output <- calibrate_method_2(swaption_data = swaption_data)
                }



                return(output)

        }
calibrate_method_2 <- function(swaption_data){

        # Method 2: Co-terminals
        boolean_subset_matrix <- matrix(data = rep(x = as.numeric(x = data$Vols[,1]), 12),ncol = 12) + as.numeric(colnames(x = data$Vols[,-1]))==30

        # Method 2: Calibrate 1 Alpha and 1 Sigma over CoTerminals
        N <- dim(swaption_data$Vols)[1]
        K <- dim(swaption_data$Vols)[2]

        param_calibrated <- list()
        param_initial <- c(0.003, 0.003)

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
                        par = param_initial, # This is alwasy a vector with S + Alpha so term structure creation should be in fit_swaptions
                        fn = fit_swaptions,
                        swaption_data_subset_vector,
                        lower = c(0.00001, 0.00001),
                        upper = c(0.06, 0.06),
                        control = list(maxit = 10000,
                                       threshold.stop = 0.03 * (N*K),
                                       max.time = 10 * 60)
                )

        parameters <- param_calibrated[[2]]
        prices_own <- gurrieri_payer_swaption_closed_form_apply(parameters = parameters,
                                                                swaption_data = swaption_data_subset_vector)
        dim(prices_own) <- c(N,K)
        output <- list(parameters,prices_own,swaption_data$Prices)
        return(output)
}

calibrate_method_1 <- function(swaption_data){

        # Method 1: Calibrate 1 Alpha and 1 Sigma over the entire Cube
        N <- dim(swaption_data$Vols)[1]
        K <- dim(swaption_data$Vols)[2]

        param_calibrated <- list()
        param_initial <- c(0.003, 0.003)

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
                        par = param_initial, # This is alwasy a vector with S + Alpha so term structure creation should be in fit_swaptions
                        fn = fit_swaptions,
                        swaption_data_subset_vector,
                        lower = c(0.00001, 0.00001),
                        upper = c(0.06, 0.06),
                        control = list(maxit = 10000,
                                       threshold.stop = 0.03 * (N*K),
                                       max.time = 10 * 60)
                )

        parameters <- param_calibrated[[2]]
        prices_own <- gurrieri_payer_swaption_closed_form_apply(parameters = parameters,
                                                                swaption_data = swaption_data_subset_vector)
        dim(prices_own) <- c(N,K)
        output <- list(parameters,prices_own,swaption_data$Prices)
        return(output)
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
#' @examples
convert_swaption_vols <-
        function(vols,
                cvTenor,
                cvDiscount,
                valdate) {

                # Dimensions of the Swaption Vol Cube
                N <- dim(vols)[1]
                K <- dim(vols)[2]

                # Pre-allocate
                Vols <- list()
                ATMStrikes <- list()
                Annuity <- list()
                Prices <- list()
                Expiries <- list()
                Tenors <- list()
                Dates <- list()
                ValDates <- list()
                Curves <- list()

                # Swaption Tenor from the input (!)
                Tenor <- as.numeric(substr(x = names(vols)[1], start = 4, stop = 4))

                # Parse valdate
                valdate <- parse_date_internal(DateToParse = valdate, DateType = "European")

                # Create a calendar
                euta <- EUTACalendar()

                cnt = 1
                for (j in 2:K) { # Loops over columns
                        for (i in 1:N) { # Loops over rows

                                Vols[[cnt]] <- vols[i, j]

                                StartDate <- shift(dates = shift(dates = valdate,
                                                                 period = months(12*vols[i,1]),
                                                                 bdc = "mf",
                                                                 calendar = euta),
                                                   period = lubridate::days(2),
                                                   bdc = "u",
                                                   calendar = euta)

                                EndDate <- shift(dates = StartDate,
                                                 period = months(12*as.numeric(names(vols)[j])),
                                                 bdc = "mf",
                                                 calendar = euta)

                                Output <- forward_swap_rate(StartDate = StartDate,
                                                            EndDate = EndDate,
                                                            ValDate = valdate,
                                                            cvDiscount = cvDiscount,
                                                            cvTenor = cvTenor,
                                                            FloatingFreq = Tenor
                                        )

                                ATMStrikes[[cnt]] <- Output[[1]] # ATM Strike is the Forward Swap Rate
                                Annuity[[cnt]]    <- Output[[2]]

                                # Calculate Swaption Price
                                Prices[[cnt]] <- value_swaption(
                                                        FwdRate = Output[[1]],
                                                        Strike = Output[[1]], # All ATM Swaptions
                                                        Vol = vols[i, j],
                                                        T = as.numeric(vols[i, 1]), # Options Expiry
                                                        Annuity = Output[[2]],
                                                        Notional = 1000000,
                                                        Shift = 0,
                                                        model = "BACH"
                                        )

                                Tenors[[cnt]]   <- as.numeric(colnames(vols)[j])
                                Expiries[[cnt]] <- vols[i, 1]
                                Dates[[cnt]]    <- generate_schedule(effective_date = StartDate,
                                                                 termination_date = EndDate,
                                                                 tenor = convert_frequency(input = Tenor,atomic = TRUE),
                                                                 calendar = euta,
                                                                 bdc = "mf")
                                Curves[[cnt]] <- cvTenor
                                ValDates[[cnt]] <- parse_date_internal(DateToParse = valdate, DateType = "European")

                                cnt = cnt + 1
                                #Test here
                        }
                }
                Dimension <- c(N,K-1)

                dim(Vols) <- Dimension
                dim(ATMStrikes) <- Dimension
                dim(Annuity) <- Dimension
                dim(Prices) <- Dimension
                dim(Tenors) <- Dimension
                dim(Expiries) <- Dimension
                dim(Curves) <- Dimension
                dim(Dates) <- Dimension
                dim(ValDates) <- Dimension

                Output <- list(Vols, Prices, ATMStrikes, Annuity, Expiries, Tenors, Curves, Dates, ValDates)#, Dates2)
                names(Output) <- c("Vols", "Prices", "StrikesATM", "Annuities", "Expiries", "Tenors", "Curves", "Dates", "ValDates")#, "Dates")

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
fit_swaptions <- function(parameters, swaption_data) {

        # swaption_data will be a list of vectors due to subsetting in calling function

        # Closed form swaption prices
        PSwaption <- gurrieri_payer_swaption_closed_form_apply(parameters = parameters,swaption_data = swaption_data)

        # Sum Squared Error
        # This is not really correct but it represents the sum of the percentage differences
        SSE <- sum((PSwaption/unlist(swaption_data$Prices)-1)^2)

        # Print some information
        cat("Alpha: ",sprintf("%.4f",parameters[[1]])," // Sigma: ",sprintf("%.4f",parameters[[2]])," // SSE: ", SSE,"\n")

        return(SSE)
}

gurrieri_payer_swaption_closed_form_apply <- function(parameters, swaption_data){

        # N
        N <- length(swaption_data$Prices)

        # Alpha
        Alpha <- parameters[1]

        # Term structure of Sigma
        MaxTerm = max(sapply(X = swaption_data$Tenors, FUN = max) + sapply(X = swaption_data$Expiries, FUN = max))
        Sigma <- dplyr::data_frame(term = MaxTerm, sigma = parameters[2])
        Sigma <- replicate(N,Sigma, simplify = FALSE)

        # Apply gurrieri closed form over vectors
        PSwaption <- mapply(FUN = gurrieri_payer_swaption_closed_form,
                            Strike = swaption_data$StrikesATM,
                            ValDate = swaption_data$ValDates,
                            TenorCurve = swaption_data$Curves,
                            SwaptionDates = swaption_data$Dates,
                            Tenor = swaption_data$Tenors,
                            Expiry = swaption_data$Expiries,
                            Notional = 1000000,
                            Alpha = Alpha,
                            S = Sigma)

        # return vector of calculated swaption prices given parameters
        return(PSwaption)
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

                # Calendar
                Calendar <- EUTACalendar()

                # First select relevant Dates
                Dates <- SwaptionDates
                SwapPayDates <- dplyr::pull(.data = Dates, end_dates)
                SwapStartDates <- dplyr::pull(.data = Dates, start_dates)

                # Dates (Mind the 2 day lag after expiry)
                SwapStart <- SwapStartDates[1]
                OptionExpiry <- shift(dates = SwapStart, period = lubridate::days(-2), bdc = "f", calendar = Calendar)
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
                C <- Strike * yearfrac(
                                DateBegin = SwapStartDates,
                                DateEnd = SwapPayDates,
                                DayCountConv = "act/360")
                C[N] <- C[N] + 1

                # B Value
                B <- (1 / Alpha) * (1 - exp(-Alpha * T.i))

                # Instant Fwd Rate
                Inst_Fwd <- instant_forward(Curve = TenorCurve,
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

        # Convert to vectors
        term <- dplyr::pull(.data = S, term)
        sigma <- dplyr::pull(.data = S, sigma)

        # Fix Sigma$ stuff
        if (t == s) {
                V_r = 0
        }
        if (t < s) {
                s_1 <- term[which.max(t <= term)]
                s_n_1 <- term[max(which.max(s <= term) - 1, 1)]
                s_n <- term[which.max(s <= term)]

                if (s_1 > s_n_1) {
                        V_r <-
                                -exp(-2 * a * t) * (1 / (2 * a)) * (sigma[match(s_1, term)]) ^ 2 * (exp(2 *
                                                a * s) - exp(2 * a * t))
                }

                if (s_1 <= s_n_1) {
                        Start_integral <-
                                -exp(-2 * a * t) * (1 / (2 * a)) * (sigma[match(s_1, term)]) ^ 2 * (exp(2 *
                                                a * s_1) - exp(2 * a * t))
                        End_integral <-
                                -exp(-2 * a * t) * (1 / (2 * a)) * (sigma[match(s_n, term)]) ^ 2 *
                                (exp(2 * a * s) - exp(2 * a * s_n_1))
                        V_r = Start_integral + End_integral
                        sequence <-
                                term[(s_1 <= term) & (s_n_1 >= term)]
                        n <- length(sequence)
                        if (length(sequence) > 0) {
                                for (k in 1:(n - 1)) {
                                        s_k = sequence[k]
                                        s_kp1 = sequence[k + 1]
                                        V_r = V_r - exp(-2 * a *
                                                        t) * (1 / (2 * a)) * (sigma[match(s_kp1, term)]) ^ 2 * (exp(2 * a * s_kp1) -
                                                                        exp(2 * a * s_k))
                                }
                        }
                }
        }

        if (t > s) {
                s_1 <- term[which.max(s <= term)]
                s_n_1 <- term[max(which.max(t <= term) - 1, 1)]
                s_n <- term[which.max(t <= term)]
                if (s_1 > s_n_1) {
                        V_r <-
                                exp(-2 * a * t) * (1 / (2 * a)) * (sigma[match(s_1, term)]) ^ 2 * (exp(2 *
                                                a * t) - exp(2 * a * s))
                }

                if (s_1 <= s_n_1) {
                        Start_integral <-
                                exp(-2 * a * t) * (1 / (2 * a)) * (sigma[match(s_1, term)]) ^ 2 * (exp(2 *
                                                a * s_1) - exp(2 * a * s))
                        End_integral <-
                                exp(-2 * a * t) * (1 / (2 * a)) * (sigma[match(s_n, term)]) ^ 2 * (exp(2 *
                                                a * t) - exp(2 * a * s_n_1))
                        V_r = Start_integral + End_integral
                        sequence <-
                                term[(s_1 <= term) & (s_n_1 >= term)]
                        n <- length(sequence)
                        if (length(sequence) > 1) {
                                for (k in 1:(n - 1)) {
                                        s_k = sequence[k]
                                        s_kp1 = sequence[k + 1]
                                        V_r = V_r + exp(-2 * a *
                                                        t) * (1 / (2 * a)) * (sigma[match(s_kp1, term)]) ^ 2 * (exp(2 * a * s_kp1) -
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
