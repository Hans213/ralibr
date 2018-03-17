#' Value a fixed leg
#'
#' @param Cv Discount Curve
#' @param ExchNot Exchange of Notionals TRUE/FALSE
#' @param CCY Currency
#' @param Rate Coupon Rate
#' @param Freq Coupon Rate Frequency
#' @param DCC Day Count Convention
#' @param BDC Business Day Convention
#' @param Dir Direction PAY/SELL
#' @param ValD Valuation Date
#' @param EffD Effective Date
#' @param MatD Maturity Date
#' @param SecLast Second to last Payment Date
#' @param Not Notional
#' @param Out Output type
#'
#' @return Depends on Output type
#' @export
#'
#' @importFrom dplyr lead
#' @importFrom dplyr data_frame
#'
#' @examples
value_fixed_leg <- function(Cv,
        ValD,
        EffD,
        MatD,
        SecLast = NULL,
        Not = 10000000,
        ExchNot = FALSE,
        CCY = "EUR",
        Rate = 0.01,
        Freq = "A",
        DCC = "ACT/360",
        BDC = "MF",
        Dir = "PAY",
        Out = "DIRTY"){

        ## Valuation Function for a fixed leg of a standard swap
        ## Curve is a Nx2 matrix containing Dates and Discount Rates
        ## Effective Date and Maturity Date needs to be specified and SecondLast is optional
        ## Notional and Notional CCY
        ## CouponRate (In decimal form)
        ## Day Count Convention and Business Day Convention with defaults "ACT/360" and "Modified Following"
        ## Output specifies what we want the function to return (Dirty, Clean, DV01, Table, List)


        # Take ValDate as the first date in the Curve matrix (User needs to make sure this is correct)
        # ValDate <- parse_date_internal(Curve[1,1])
        ValD <- parse_date_internal(ValD)

        # Bump curve with the default 1 bp parallel increase
        CurveBumped <- bump_curve(Curve = Cv)
        CurveBumped[,1] <- parse_date_internal(CurveBumped[,1])

        Cv[,1] <- parse_date_internal(Cv[,1])


        # Generate the payment dates from Effective to Maturity, optionally including ValDate and SecondLast
        Dates <- generate_dates(StartDate = EffD,
                                    EndDate = MatD,
                                    SecondLast = SecLast,
                                    CouponFreq = Freq,
                                    BusDayConv = BDC,
                                    ValDate = ValD,
                                    Output = "Frame") # Output is in Date format !



        # Convert for internal use
        # Dates <- parse_date_internal(Dates)

        # Create two vectors (no conversion needed)
        DatesBegin <- Dates$StartDates
        DatesEnd <- Dates$EndDates

        N <- length(DatesBegin)

        # Roll and parse because roll_weekday is exported
        DatesPay <- parse_date_internal(roll_weekday(Day = DatesEnd, BusDayConv = BDC))

        # Yearfraction
        DatesFrac <- yearfrac(DateBegin = DatesBegin,DateEnd = DatesPay,DayCountConv = DCC)

        # Calculate the discount factor for the Payment End Dates
        DiscountFactors <- interpolate(X = Cv[,1],Y = Cv[,2], x = DatesEnd, method = "cs")
        DiscountFactorsBump <- interpolate(X = CurveBumped[,1], Y = CurveBumped[,2],x = DatesEnd, method = "cs")

        # Buy or Sell?
        if(toupper(Dir)=="PAY"){
                Sign <- rep(x = -1, N)
                Type <- rep(x = "Pay Fixed")
        }else{
                Sign <- rep(x = 1, N)
                Type <- rep(x = "Receive Fixed")
        }

        # Calculate actual coupon payments per period
        Rates               <- rep(x = Rate, N)
        Notionals           <- rep(x = Not,N)
        Cashflow            <- Sign * Notionals * Rates * DatesFrac

        # Do we add an exchange of principals on the end?
        if(ExchNot){
                Cashflow[N]   <- Cashflow[N] + Sign[N]*Not
        }

        # Discount cashflows with the discount factor
        CashflowDisc         <- Cashflow * DiscountFactors
        CashflowDiscBump     <- Cashflow * DiscountFactorsBump

        # Sum of discounted cashflows (= Dirty Values)
        ValueDirty           <- sum(CashflowDisc)
        ValueDirtyBump       <- sum(CashflowDiscBump)

        # Create the cashflow table
        Table <- data_frame(Type = Type,
                            CCY = rep(CCY,N),
                            PeriodBegin = DatesBegin,
                            PeriodEnd = DatesEnd,
                            PeriodPay = DatesPay,
                            YearFraction = DatesFrac,
                            Notional = Notionals,
                            Rate = Rates,
                            Cashflow = Cashflow,
                            DiscountFactor = DiscountFactors,
                            CashflowDisc = CashflowDisc)

        # Accrual from last Reset to ValDate ?
        AccrualTime <- yearfrac(DateBegin = DatesBegin[1], DateEnd = ValD, DayCountConv = DCC)
        Accrual     <- Not * Rate * AccrualTime * Sign[1]

        # Clean Values
        ValueClean <- ValueDirty - Accrual
        ValueCleanBump <- ValueDirtyBump - Accrual

        # DV01 (Based on Dirty)
        DV01 <- ValueDirtyBump-ValueDirty

        # What output type
        if(toupper(Out)=="CLEAN"){OutputValue <- ValueClean}
        else if(toupper(Out)=="DIRTY"){OutputValue <- ValueDirty}
        else if(toupper(Out)=="TABLE"){OutputValue <- Table}
        else if(toupper(Out)=="DV01"){OutputValue <- DV01}
        else if(toupper(Out)=="LIST1"){OutputValue <- list(ValueClean, DV01, Table)}
        else if(toupper(Out)=="LIST2"){OutputValue <- list(ValueDirty, DV01, Table)}

        # Output
        return(OutputValue)

}
attr( value_fixed_leg, "description" ) <- list("Value a fixed leg",
        Cv= '',
        ExchNot= '',
        CCY= '',
        Rate= '',
        Freq= '',
        DCC= '',
        BDC= '',
        Dir= '',
        ValD= '',
        EffD= '',
        MatD= '',
        SecLast= '',
        Not= '',
        Out= ''
);

#' Title
#'
#' @param CvDSC
#' @param CvTNR
#' @param ValD
#' @param EffD
#' @param MatD
#' @param SecLast
#' @param LstFix
#' @param Not
#' @param ExchNot
#' @param CCY
#' @param Freq
#' @param Spr
#' @param BDC
#' @param DCC
#' @param Dir
#' @param Out
#' @param Tenor
#'
#' @return
#' @export
#'
#' @importFrom dplyr lead
#' @importFrom dplyr data_frame

#' @examples
value_floating_leg <- function(
        CvDSC,
        CvTNR,
        ValD,
        EffD,
        MatD,
        SecLast = NULL,
        LstFix = 0,
        Not = 10000000,
        ExchNot = FALSE,
        CCY = "EUR",
        Tenor = "SA",
        Freq = "SA",
        Spr = 0,
        BDC = "MF",
        DCC = "30/360",
        Dir = "RECEIVE",
        Out = "DIRTY"){

        ## Valuation Function for a FLOATING leg of a standard swap
        ## Curve is a Nx2 matrix containing Dates and Discount Rates
        ## Effective Date and Maturity Date needs to be specified and SecondLast is optional
        ## Notional and Notional CCY
        ## Tenor curve and Spread (with Last Fixing which is required!)
        ## Day Count Convention and Business Day Convention with defaults "30/360" and "Modified Following"
        ## Output specifies what we want the function to return (Dirty, Clean, DV01, Table, List)

        # Take ValDate from the Curve
        # ValDate <- parse_date_internal(CurveDiscount[1,1])
        ValD <- parse_date_internal(ValD)


        # Bump our curves with 1 bps parallel increase
        CurveDiscountBumped <- bump_curve(Curve = CvDSC)
        CurveTenorBumped <- bump_curve(Curve = CvTNR)

        # Parse all Curves
        CurveDiscountBumped[,1] <- parse_date_internal(CurveDiscountBumped[,1])
        CurveTenorBumped[,1] <- parse_date_internal(CurveTenorBumped[,1])

        CvDSC[,1] <- parse_date_internal(CvDSC[,1])
        CvTNR[,1] <- parse_date_internal(CvTNR[,1])

        # Generate the Payment dates from Effective to Maturity, optionally including ValDate
        Dates <- generate_dates(StartDate = EffD,EndDate = MatD,
                SecondLast = SecLast,
                CouponFreq = Freq,
                BusDayConv = BDC,
                ValDate = ValD,
                Output = "Frame") # Output is in R dates

        # Create two vectors
        DatesEnd <- Dates$EndDates
        DatesBegin <- Dates$StartDates

        N <- length(DatesBegin)

        # Roll and convert because roll_weekday is exported
        DatesPay <- parse_date_internal(roll_weekday(Day = DatesEnd, BusDayConv = BDC))

        DatesFrac <- yearfrac(DateBegin = DatesBegin, DateEnd = DatesPay,DayCountConv = DCC)

        # What are the discount factors for the payment period end dates?
        DiscountFactors <- interpolate(X = CvDSC[,1],Y = CvDSC[,2], x = DatesEnd, method = "cs")
        DiscountFactorsBump <- interpolate(X = CurveDiscountBumped[,1],Y = CurveDiscountBumped[,2],x = DatesEnd, method = "cs")


        # Do we pay or receive this leg?
        if(toupper(Dir)=="PAY"){
                Sign <- rep(x = -1,N)
                Type <- rep(x = "Pay Floating",N)
        }else{
                Sign <- rep(x = 1, N)
                Type <- rep(x = "Receive Floating", N)
        }

        # Calculate the forward rates given the current discount factors and add to last fixing
        if(N>1){
                FwdRates <- c(LstFix,
                        forward_rate(StartDate = DatesBegin[-1],
                                EndDate = DatesEnd[-1],
                                frame = CvTNR,
                                DayCount = DCC))
                FwdRatesBumped <- c(LstFix,
                        forward_rate(StartDate = DatesBegin[-1],
                                EndDate = DatesEnd[-1],
                                frame = CurveTenorBumped,
                                DayCount = DCC))
        }else{
                FwdRates <- LstFix
                FwdRatesBumped <- LstFix
        }


        # Spread and Notionals ?
        Spreads <- rep(x = Spr,N)
        Notionals <- rep(x = Not,N)

        # Calculate the actual coupon payments per payment period
        Cashflow <- Notionals * (FwdRates+Spreads) * Sign * DatesFrac
        CashflowBump <- Notionals * (FwdRatesBumped+Spreads) * Sign * DatesFrac

        # Principal Exchange
        if(ExchNot){
                Cashflow[N]           <- Cashflow[N] + Sign[N]*Not
                CashflowBump[N]       <- CashflowBump[N] + Sign[N]*Not
        }

        # Value of the cashflows after discounting
        CashflowDisc            <- Cashflow * DiscountFactors
        CashflowBumpDisc        <- CashflowBump * DiscountFactorsBump

        # Sum of discounted cashflows (=Dirty Value)
        ValueDirty        <- sum(CashflowDisc)
        ValueDirtyBump  <- sum(CashflowBumpDisc)

        # The cashflow table with details
        Table <- data_frame(Type = Type,
                CCY = rep(CCY,N),
                PeriodBegin = DatesBegin,
                PeriodEnd = DatesEnd,
                PeriodPay = DatesPay,
                YearFraction = DatesFrac,
                Notional = Notionals,
                Rate = FwdRates + Spr,
                Cashflow = Cashflow,
                DiscountFactor = DiscountFactors,
                CashflowDisc = CashflowDisc)

        # Accrual
        AccrualTime <- yearfrac(DateBegin = DatesBegin[1], DateEnd = ValD, DayCountConv = DCC)
        Accrual <- Not * FwdRates[1] * AccrualTime * Sign[1]

        # Clean values
        ValueClean <- ValueDirty - Accrual
        ValueCleanBump <- ValueDirtyBump - Accrual

        # DV01 (Based on Dirty)
        DV01 <- ValueDirtyBump-ValueDirty

        # What output type
        if(toupper(Out)=="CLEAN"){OutputValue <- ValueClean}
        else if(toupper(Out)=="DIRTY"){OutputValue <- ValueDirty}
        else if(toupper(Out)=="TABLE"){OutputValue <- Table}
        else if(toupper(Out)=="DV01"){OutputValue <- DV01}
        else if(toupper(Out)=="LIST1"){OutputValue <- list(ValueClean, DV01, Table)}
        else if(toupper(Out)=="LIST2"){OutputValue <- list(ValueDirty, DV01, Table)}

        # Output
        return(OutputValue)
}


#' Title
#'
#' @param FwdRate
#' @param Strike
#' @param Vol
#' @param Annuity
#' @param Notional
#' @param Shift
#' @param model
#'
#' @return
#' @export
#'
#' @examples
value_swaption <-
        function(FwdRate,
                Strike,
                Vol,
                T,
                Annuity,
                Notional = 100000,
                Shift = 0,
                model = "BACH") {

                # If Strike is missing then use ATM Strike (Do we really want to do this?)
                if (missing(x = Strike)) {
                        Strike <- FwdRate
                }
                if (missing(Vol)) {
                        stop("Please provide a Volatility")
                }

                if (toupper(model) == "BACH") {
                        Vol <- Vol / 10000
                        Value <-
                                Notional * Annuity * bachelier(
                                        FwdRate = FwdRate,
                                        Strike = Strike,
                                        Vol = Vol,
                                        Maturity = T
                                )
                }
                else if (toupper(model) == "LOGN") {
                        Vol <- Vol / 100
                        Value <-
                                Notional * Annuity * lognormal(
                                        FwdRate = FwdRate,
                                        Strike = Strike,
                                        Vol = Vol,
                                        Maturity = T,
                                        Shift = Shift
                                )
                }
                return(Value)
        }

#' Title
#'
#' @param FwdRate
#' @param Strike
#' @param Vol
#' @param Maturity
#' @param Shift
#'
#' @return
#' @export
#'
#' @examples
lognormal <- function(FwdRate, Strike, Vol, Maturity, Shift = 0) {
        if (Maturity == 0) {
                Value = max(0, FwdRate - Strike)
        }
        if (Maturity > 0) {
                d1 <- (log((FwdRate + Shift) / (Strike + Shift)) + 0.5 * Vol ^ 2 * Maturity) / (Vol * sqrt(Maturity))
                d2 <- d1 - Vol * sqrt(Maturity)
                Value <- (FwdRate + Shift) * pnorm(d1, mean = 0, sd = 1) - (Strike + Shift) * pnorm(d2, mean = 0, sd = 1)
        }
        return(Value)
}

#' Title
#'
#' @param FwdRate
#' @param Strike
#' @param Vol
#' @param Maturity
#'
#' @return
#' @export
#'
#' @examples
bachelier <- function(FwdRate, Strike, Vol, Maturity){

        if(Maturity==0){
                Bach = max(0,FwdRate - Strike)
        }
        if(Maturity>0){
                d = (FwdRate - Strike)/(Vol*sqrt(Maturity))
                Bach = (FwdRate - Strike)*pnorm(d,mean=0,sd=1)+Vol*sqrt(Maturity)*dnorm(d,mean=0,sd=1)
        }
        return(Bach)
}
