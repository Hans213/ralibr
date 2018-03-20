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
#' @examples
#' require(lubridate)
#' value_fixed_leg(EUR_OIS_20171229,ymd(20170101),ymd(20220101))
value_fixed_leg <-
        function(Cv,
                 EffD,
                 MatD,
                 Stub = "short_front",
                 Not = 10000000, #Add amortization
                 ExchNot = FALSE,
                 Calendar = EUTACalendar(),
                 CCY = "EUR",
                 Rate = 0.01,
                 Freq = "A",
                 DCC = "ACT/360",
                 BDC = "MF",
                 Dir = "PAY"){

        ## Valuation Function for a fixed leg of a standard swap
        ## Curve is a Nx2 matrix containing Dates and Discount Rates
        ## Effective Date and Maturity Date needs to be specified and SecondLast is optional
        ## Notional and Notional CCY
        ## CouponRate (In decimal form)
        ## Day Count Convention and Business Day Convention with defaults "ACT/360" and "Modified Following"

        # Take ValDate as the first date in the Curve matrix (User needs to make sure this is correct)
        ValD <- parse_date_internal(Cv[1,1])

        # Bump curve with the default 1 bp parallel increase
        Cv_dv01 <- bump_curve(Curve = Cv)

        frame <-
                generate_schedule(
                        effective_date = EffD,
                        termination_date = MatD,
                        tenor = convert_frequency(input = Freq, atomic = TRUE),
                        reset = NULL,
                        calendar = Calendar,
                        bdc = BDC,
                        stub = Stub
                ) %>% mutate(year_fraction = yearfrac(
                        DateBegin = start_dates,
                        DateEnd = end_dates,
                        DayCountConv = DCC
                )) %>% mutate(discount_factor = interpolate(
                        X = Cv[, 1],
                        Y = Cv[, 2],
                        x = end_dates,
                        method = "cs"
                )) %>% mutate(discount_factor_dv01 = interpolate(
                        X = Cv_dv01[, 1],
                        Y = Cv_dv01[, 2],
                        x = end_dates,
                        method = "cs"
                )) %>% mutate(direction = if_else(toupper(Dir) == "PAY",-1, 1)) %>% mutate(fixed_rate = Rate) %>%
                mutate(notional = Not) #Add amortizations here

        N <- dim(frame)[1]

        if (ExchNot) {
                frame$notional_exch <- rep(Not, N)
        } else
                frame$notional_exch <- c(rep(0, N - 1), Not)

        frame <-
                frame %>% mutate(cash_flows = notional * direction * fixed_rate * year_fraction + notional_exch * direction) %>%
                mutate(cash_flows_disc = cash_flows * discount_factor) %>%
                mutate(cash_flows_disc_dv01 = cash_flows * discount_factor_dv01)

        dirty <- sum(frame$cash_flows_disc,na.rm = TRUE)
        dirty_dv01 <- sum(frame$cash_flows_disc_dv01,na.rm = TRUE)
        dv01 <- dirty_dv01 - dirty
        accrual <-
                yearfrac(
                        DateBegin = frame$start_dates[1],
                        DateEnd = ValD,
                        DayCountConv = DCC
                ) * frame$cash_flows[1]
        clean <- dirty - accrual


        return(list(currency = CCY, dirty = dirty, clean = clean, dv01 = dv01, table = frame))
}


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
#' @examples
#' value_floating_leg(CvDSC = EUR_OIS_20160930,CvTNR = EUR_6M_20160930,EffD = ymd(20120101),MatD = ymd(20210101))
value_floating_leg <- function(
        CvDSC,
        CvTNR,
        EffD,
        MatD,
        Stub = "short_front",
        LstFix = 0,
        Not = 10000000,
        ExchNot = FALSE,
        Calendar = EUTACalendar(),
        CCY = "EUR",
        Reset = "T-2",
        Freq = "SA",
        Spr = 0,
        BDC = "MF",
        DCC = "30/360",
        Dir = "RECEIVE"){

        ## Valuation Function for a FLOATING leg of a standard swap
        ## Curve is a Nx2 matrix containing Dates and Discount Rates
        ## Effective Date and Maturity Date needs to be specified and SecondLast is optional
        ## Notional and Notional CCY
        ## Tenor curve and Spread (with Last Fixing which is required!)
        ## Day Count Convention and Business Day Convention with defaults "30/360" and "Modified Following"

        # Take ValDate from the Curve
        ValD <- parse_date_internal(CvDSC[1,1])

        # Bump our curves with 1 bps parallel increase
        CvDSC_dv01 <- bump_curve(Curve = CvDSC)
        CvTNR_dv01 <- bump_curve(Curve = CvTNR)

        # Parse all Curves
        CvDSC_dv01[,1] <- parse_date_internal(CvDSC_dv01[,1])
        CvTNR_dv01[,1] <- parse_date_internal(CvTNR_dv01[,1])

        CvDSC[,1] <- parse_date_internal(CvDSC[,1])
        CvTNR[,1] <- parse_date_internal(CvTNR[,1])

        frame <-
                generate_schedule(
                        effective_date = EffD,
                        termination_date = MatD,
                        tenor = convert_frequency(input = Freq, atomic = TRUE),
                        reset = days(-2),
                        # Create conversion function from T, T-1, T-2
                        calendar = Calendar,
                        bdc = BDC,
                        stub = Stub
                ) %>% mutate(year_fraction = yearfrac(
                        DateBegin = start_dates,
                        DateEnd = end_dates,
                        DayCountConv = DCC
                )) %>% mutate(discount_factor = interpolate(
                        X = CvDSC[, 1],
                        Y = CvDSC[, 2],
                        x = end_dates,
                        method = "cs"
                )) %>% mutate(discount_factor_dv01 = interpolate(
                        X = CvDSC_dv01[, 1],
                        Y = CvDSC_dv01[, 2],
                        x = end_dates,
                        method = "cs"
                )) %>% mutate(direction = if_else(toupper(Dir) == "PAY", -1, 1)) %>% mutate(spread = Spr) %>% mutate(
                        floating_rate = forward_rate(
                                StartDate = reset_dates,
                                EndDate = end_dates,
                                frame = CvTNR,
                                DayCount = DCC
                        )
                ) %>% mutate(
                        floating_rate_dv01 = forward_rate(
                                StartDate = reset_dates,
                                EndDate = end_dates,
                                frame = CvTNR_dv01,
                                DayCount = DCC
                        )
                ) %>% mutate(notional = Not) #Add amortizations here

        # Add last fixing
        frame$floating_rate[which.min(is.na(frame$floating_rate))-1] <- LstFix
        frame$floating_rate_dv01[which.min(is.na(frame$floating_rate))-1] <- LstFix

        # Length
        N <- dim(frame)[1]

        # Exchange of Notionals
        if (ExchNot) {
                frame$notional_exch <- rep(Not, N)
        } else
                frame$notional_exch <- c(rep(0, N - 1), Not)

        # Final frame
        frame <-
                frame %>% mutate(cash_flows = notional * direction * (floating_rate + Spr) * year_fraction + notional_exch * direction) %>%
                mutate(cash_flows_dv01 = notional * direction * (floating_rate_dv01 + Spr) * year_fraction + notional_exch * direction) %>%
                mutate(cash_flows_disc = cash_flows * discount_factor) %>%
                mutate(cash_flows_disc_dv01 = cash_flows_dv01 * discount_factor_dv01)

        # Swap values
        dirty <- sum(frame$cash_flows_disc,na.rm = TRUE)
        dirty_dv01 <- sum(frame$cash_flows_disc_dv01,na.rm = TRUE)
        dv01 <- dirty_dv01 - dirty
        accrual <-
                yearfrac(
                        DateBegin = frame$start_dates[1],
                        DateEnd = ValD,
                        DayCountConv = DCC
                ) * frame$cash_flows[1]
        clean <- dirty - accrual

        return(list(currency = CCY, dirty = dirty, clean = clean, dv01 = dv01, table = frame))
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
        if (Maturity == 0) Value = max(0, FwdRate - Strike)
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

        if(Maturity==0) Bach = max(0,FwdRate - Strike)
        if(Maturity>0){
                d = (FwdRate - Strike)/(Vol*sqrt(Maturity))
                Bach = (FwdRate - Strike)*pnorm(d,mean=0,sd=1)+Vol*sqrt(Maturity)*dnorm(d,mean=0,sd=1)
        }
        return(Bach)
}
