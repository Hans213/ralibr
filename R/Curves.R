#' Bump a given curve
#'
#' @param Curve A curve (discount or zero coupon)
#' @param Bump The bump to apply
#' @param IsDiscounts optional (default = TRUE) - indicates if the second column are discount facors or zero rates
#'
#' @return A bumped curve
#'
#' @export
#'
#' @examples bump_curve(Curve)
#'
bump_curve <- function(Curve, Bump = 0.0001, IsDiscounts = TRUE){
        # Take a single Curve and bump the Zero rates with 1 bps.
        # The default setting assumes that column 2 contains Discount factors
        # Output the new Discount curve (with dates)

        N <- length(Curve[,1])
        StartDates <- rep(x = Curve[1,1], N)
        T <- yearfrac(DateBegin = StartDates, DateEnd = Curve[,1], DayCountConv = "act/360")
        if(IsDiscounts){
                ZeroRates <- discount_to_rate(DF = Curve[,2], T = T, Compounding = "CC")
                ZeroRatesBump <- ZeroRates + Bump
                CurveBump <- Curve;
                CurveBump[,2] <- rate_to_discount(Rate = ZeroRatesBump,T = T,Compounding = "CC")
        }else{
                CurveBump <- Curve
                CurveBump <- Curve[,2]+Bump
        }

        return(CurveBump)
}

#' Calculate forward rates
#'
#' @param StartDate
#' @param EndDate
#' @param frame
#' @param DayCount
#' @param compounding
#'
#' @return
#' @export
#'
#' @examples
forward_rate <- function(StartDate, EndDate, frame, DayCount = "act/360", compounding = "AC"){

        ## Function calculates a forward rate for given Start and End
        ## EndDate can be eighter a Date or a numeric T (Years)

        # Updated with parse_date_internal
        StartDate <- parse_date_internal(DateToParse = StartDate)
        EndDate   <- parse_date_internal(DateToParse = EndDate)
        #if(class(EndDate)=="numeric"){EndDate <- as.numeric(StartDate)+EndDate/360}

        # Discount factors for start and end date
        DF.Start <- interpolate(X = frame[,1], Y = frame[,2], x = StartDate, method = "cs")
        DF.End <- interpolate(X = frame[,1], Y = frame[,2], x = EndDate,method = "cs")

        # What is the Yearfrac between Start and End dates
        dT <- yearfrac(DateBegin = StartDate,DateEnd = EndDate,DayCountConv = DayCount)

        if(any(dT==0)){dT[dT==0]<-EndDate[dT==0]*360}

        # Fwd Rate given convention
        if(compounding=="CC"){
                forward_rate <- log((DF.Start/DF.End))/dT
        }else{
                forward_rate <- vector(mode = "numeric",length=length(dT))
                forward_rate[dT<1] <- (1/dT[dT<1])*(DF.Start[dT<1]/DF.End[dT<1] - 1)
                forward_rate[dT>=1] <- (DF.Start[dT>=1]/DF.End[dT>=1])^(1/dT[dT>=1])-1
        }

        return(forward_rate)
}

#' Title
#'
#' @param Curve
#' @param t
#' @param delta
#'
#' @return
#' @export
#'
#' @examples
instant_forward <- function(Curve, t, delta = 0.001) {

        Curve[, 1] <- (Curve[, 1] - Curve[1, 1]) / 360
        Curve[, 2] <- log(Curve[, 2])

        Fwd1 <-
                interpolate(X = Curve[,1],Y = Curve[,2],x = t,method = "cs")

        Fwd2 <-
                interpolate(X = Curve[,1],Y = Curve[,2],x = t + delta, method = "cs")

        Inst_Fwd <- -(Fwd2 - Fwd1) / delta
        return(Inst_Fwd)
}

#' Convert Discount Factors to Spot Rates
#'
#' @param DF A discount factor (numeric)
#' @param Compounding optional (default="CC") - The compounding frequency
#'
#' @return
#' @export
#'
#' @examples
discount_to_rate <- function(DF, T, Compounding = "CC"){

        if((toupper(Compounding)=="CC")|toupper(Compounding)=="Continuous"){
                ZeroRates <- -(1/T)*log(x = DF)
        }
        else if((toupper(Compounding)=="AC")|toupper(Compounding)=="Annual"){
                ZeroRates <- ((1/DF))^(1/T)-1
        }
        ZeroRates[is.na(ZeroRates)] <- 0
        return(ZeroRates)
}

#' Convert rates to discount factors
#'
#' @param Rate
#' @param Compounding
#'
#' @return
#' @export
#'
#' @examples
rate_to_discount <- function(Rate, T, Compounding = "CC"){

        if((toupper(Compounding)=="CC")|toupper(Compounding)=="Continuous"){
                Df <- exp(-Rate*T)
        }
        else if((toupper(Compounding)=="AC")|toupper(Compounding)=="Annual"){
                Df <- 1/(1+Rate)^T
        }
}



#' Interpolation
#'
#' @description Function useful for Interpolation.
#'    can be either "Dates" or "Numeric", allowing to
#'    easily Interpolate on discount curves.
#'    The interpolation can be done using linear or cubic-spline.
#'
#' @param X,Y Vectors giving the coordinates of the points to be interpolated.
#'    X can optionally be "Date" format,
#'    allowing to interpolate on a typical j
#'    discount curve structure.
#'
#' @param x A single point specifying where to interpolate on observations of X.
#'    Can be numeric, Date or character ("YYYY-MM-DD")
#'
#' @param method Either linear ("l") or cubicspline ("cs")
#'
#' @return The value corresponding to x of values in Y.
#'
#' @examples
#' Interpolate(X = c(1,3,5),Y = c(100,200,300),x = 2,method="l")
#' Interpolate(Curve$Date, Curve$Discount_Factors, "2025-03-03", "linear")
#'
#' @export

interpolate <- function(X, Y, x, method = "linear"){


        # No longer using characters as Input

        # if(class(x)=="character"){
        #         x <- ParseDate(DateToParse = x)
        # }

        # Convert the Vector X values in Column 1 to integers
        if (class(X)=="Date"){
                X <- as.numeric(X)
        }
        if (class(x)=="Date"){
                x <- as.numeric(x)
        }

        # Check if Point is inside the Vector X range
        if (any(x<min(X))) stop("Point < min(X)")

        # Interpolate given method
        if (method=="linear"|method=="lin"|method=="l"){
                if(any(x>max(X))){stop("x > max(X) - Use method = 'cs' for Extrapolation")}
                Interpolate <- approx(x = X,y = Y,xout = x)[[2]]
        }
        else if (method=="cs"|method=="cubicspline"|method=="spline"){
                if(any(x>max(X))){warning("x > max(X) - Extrapolating")} # Maybe better to interpolate flat past final ?
                Interpolate <- spline(x = X,y = Y,method = "natural",xout = x)[[2]]
        }
        else stop("Input 'method' is wrong")

        return(Interpolate)
}

#' Title
#'
#' @param StartDate
#' @param EndDate
#' @param ValDate
#' @param FloatingFreq
#' @param cvDiscount
#' @param cvTenor
#' @param FloatingDCC
#' @param FloatingBDC
#' @param FixedDCC
#' @param FixedBDC
#' @param FixedFreq
#'
#' @return
#'
#'
#' @examples
forward_swap_rate <-
        function(StartDate,
                EndDate,
                ValDate,
                cvDiscount,
                cvTenor,
                FloatingFreq = "q",
                FloatingDCC = "act/360",
                FloatingBDC = "mf",
                FixedFreq = "a",
                FixedDCC = "30/360",
                FixedBDC = "mf") {
                # Function calculates a Forward Swap Rate with given input

                ValDate <- parse_date_internal(DateToParse = ValDate, DateType = "European")

                FixedLeg <-
                        annuity_fixed_leg(
                                StartDate = StartDate,
                                EndDate = EndDate,
                                ValDate = ValDate,
                                cvDiscount = cvDiscount,
                                CouponFreq = FixedFreq,
                                BusDayConv = FixedBDC,
                                DayCount = FixedDCC
                        )

                FloatingLeg <-
                        annuity_floating_leg(
                                StartDate = StartDate,
                                EndDate = EndDate,
                                ValDate = ValDate,
                                cvDiscount = cvDiscount,
                                cvTenor = cvTenor,
                                CouponFreq = FloatingFreq,
                                BusDayConv = FloatingBDC,
                                DayCount = FloatingDCC
                        )

                # Fwd Swap Rate
                FwdSwapRate <- FloatingLeg / FixedLeg

                return(list(FwdSwapRate, FixedLeg, FloatingLeg))
        }

#' Title
#'
#' @param StartDate
#' @param EndDate
#' @param ValDate
#' @param CouponFreq
#' @param BusDayConv
#'
#' @return
#' @export
#'
#' @importFrom dplyr lead
#'
#' @examples
annuity_fixed_leg <-
        function(StartDate,
                EndDate,
                ValDate,
                cvDiscount,
                CouponFreq = "A",
                BusDayConv = "MF",
                DayCount = "30/360") {

                # Fixed leg Annuity
                DatesBegin <-
                        generate_dates(
                                StartDate = StartDate,
                                EndDate = EndDate,
                                CouponFreq = CouponFreq,
                                BusDayConv = BusDayConv,
                                ValDate = ValDate,
                                Output = "Frame"
                        )
                DatesEnd <- DatesBegin$EndDates
                DatesBegin <- DatesBegin$StartDates

                N = length(DatesBegin)

                DatesFrac <-
                        yearfrac(
                                DateBegin = DatesBegin,
                                DateEnd = DatesEnd,
                                DayCountConv = DayCount
                        )

                DiscountFactors <-
                        interpolate(
                                X = cvDiscount[, 1],
                                Y = cvDiscount[, 2],
                                x = DatesEnd,
                                method = "cs"
                        )

                FixedLeg <- sum(DiscountFactors * DatesFrac)

                return(FixedLeg)
        }

#' Title
#'
#' @param StartDate
#' @param EndDate
#' @param ValDate
#' @param cvDiscount
#' @param cvTenor
#' @param CouponFreq
#' @param BusDayConv
#' @param DayCount
#'
#' @return
#' @importFrom dplyr lead
#'
#' @examples
annuity_floating_leg <-
        function(StartDate,
                EndDate,
                ValDate,
                cvDiscount,
                cvTenor,
                CouponFreq = "Q",
                BusDayConv = "MF",
                DayCount = "act/360") {

                # Calculate floating leg annuity
                Dates <-
                        generate_dates(
                                StartDate = StartDate,
                                EndDate = EndDate,
                                CouponFreq = CouponFreq,
                                BusDayConv = BusDayConv,
                                ValDate = ValDate,
                                Output = "Frame"
                        )

                DatesEnd <- Dates$EndDates
                DatesBegin <- Dates$StartDates

                N <- length(DatesBegin)

                DatesFracFl <-
                        yearfrac(
                                DateBegin = DatesBegin,
                                DateEnd = DatesEnd,
                                DayCountConv = DayCount
                        )
                Fwds <-
                        forward_rate(
                                StartDate = DatesBegin,
                                EndDate = DatesEnd,
                                frame = cvTenor,
                                DayCount = DayCount
                        )

                DiscountFactorsFl <-
                        interpolate(
                                X = cvDiscount[, 1],
                                Y = cvDiscount[, 2],
                                x = DatesEnd,
                                method = "cs"
                        )

                FloatingLeg <- sum(Fwds * DatesFracFl * DiscountFactorsFl)

                return(FloatingLeg)
        }
