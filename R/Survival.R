#' Survival probability from Hazard rates
#'
#' @param SurvivalDate Date for surival chance
#' @param ValuationDate Date of calculation
#' @param Maturities Hazard rate maturities
#' @param Hazards Hazard rates
#' @param Convention Day count convention
#'
#' @description Function takes a given date and Hazard rate Term Structure and calculates
#    the Survival Probability up to the SurvivalDate given the term structure of hazard rates
#    We use the assumptions of piecewise constant hazard rates

#' @return A survival percentage up to a given Date
#'
#' @export
#'
#'
#' @examples
survival <-
        function(SurvivalDate,
                ValuationDate,
                Maturities,
                Hazards,
                Convention = "act/360") {

                # Date inputs for Internal use
                SurvivalDate <- parse_date_internal(DateToParse = SurvivalDate, DateType = "European")
                ValuationDate <- parse_date_internal(DateToParse = ValuationDate, DateType = "European")
                Maturities <- parse_date_internal(DateToParse = Maturities, DateType = "European")

                # Extract EndDates that lie before the SurvivalDate
                EndDates <- c(Maturities[Maturities < SurvivalDate], SurvivalDate)

                # Calculate per period the StartDate
                StartDates <- dplyr::lag(x = EndDates)
                StartDates[1] <- ValuationDate

                # Put in frame
                Frame <- data_frame(EndDates = EndDates, StartDates = StartDates)

                # Calculate Yearfrac given the convention
                Frame$Frac <-
                        ralibr::yearfrac(
                                DateBegin = Frame$StartDates,
                                DateEnd = Frame$EndDates,
                                DayCountConv = Convention
                        )

                # Assign the hazards for these discrete periods
                Frame$Hazards <- Hazards[1:length(Frame$Frac)]

                if (any(is.na(x = Frame$Hazards))) {
                        Frame$Hazards[which(is.na(x = Frame$Hazards))] <-
                                Hazards[length(x = Hazards)]
                }

                # Cumulative Survival is "Q = exp(SumProd(T*Lambda))"
                survival <- exp(-sum(Frame$Frac * Frame$Hazards))

                return(survival)
        }
attr(survival, "description") <- list(
        "Calculate Survival probability from Hazard rates",
        SurvivalDate = "Survival probability date",
        ValuationDate = "The Hazard rate term structure valuation date",
        Maturities = "The Hazard rate term structure maturities",
        Hazards = "The Hazard rate values",
        Convention = "optional - Day count convention to use for yearfractions"
)


#' Bootstrap a CDS curve for hazard rates
#'
#' @usage
#'
#' @param OutputDates Date for output value
#' @param Maturity
#' @param Spreads
#' @param Curve
#' @param ValDate
#' @param Conv
#' @param Out
#' @param LGD Paramter for Loss-Given-Default
#'
#' @return
#'
#' @export
#'
bootstrap_cds <-
        function(Maturity,
                Spreads,
                Curve,
                ValDate,
                Conv = "act/360",
                OutputDates = NULL,
                LGD = 0.6,
                Out = "Hazard") {
                # Function loops over every Maturity in the MarketData matrix and bootstraps a Hazard rate implied by it

                # MarketData needs to be an Nx2 Matrix containing Maturities (integers) and Spreads (in bps) (numerics)
                # ZeroData needs to be a an Nx2 Matrix containing Dates (Dates) and Discount factors (numerics)

                # Put together the CDS frame
                MarketData <- data.frame(CDS.Maturities = Maturity, CDS.Quotes = Spreads)

                # Length
                N <- dim(MarketData)[1]

                # Convert input dates to R Date format for Internal use
                ValDate <- parse_date_internal(DateToParse = ValDate, DateType = "European")
                Curve[,1] <- parse_date_internal(DateToParse = Curve[,1], DateType = "European")


                ## Actual function
                # First Check Output string
                if (!any(c(
                        Out == "Hazard",
                        Out == "Survival",
                        Out == "Default"
                ))) {
                        stop("Wrong Output Type")
                }

                # Put ValuationDate in Date
                ValDate <- parse_date_internal(DateToParse = ValDate)

                # The idea is to solve the Hazards recursively
                MarketData <- cbind(MarketData, rep(x = 0.01, times = length(MarketData[, 1])))

                # Put in Frame
                MarketData <- as.tibble(x = MarketData)
                colnames(MarketData) <- c("CDS.Maturities", "CDS.Quotes","Hazards")

                # Tenors to Maturities (= Dates)
                #MarketData[,1] <- as.Date(x = mapply(FUN = shift, rep(list(ValDate), N), as.list(x = MarketData[, 1])) ,origin = "1970-01-01")
                MarketData <- dplyr::mutate(.data = MarketData, CDS.Dates = NA)
                for (i in 1:N){
                        MarketData$CDS.Dates[i] <- shift(dates = ValDate,period = months(12*Maturity[i]),bdc = "f",calendar = EUTACalendar())
                }
                MarketData$CDS.Dates <- as.Date(x = MarketData$CDS.Dates,origin = "1970-01-01")

                MarketData$CDS.Dates.IMM <-
                        as.Date(
                                x = sapply(X = MarketData$CDS.Dates, FUN = next_imm),
                                origin = "1970-01-01"
                        )

                MarketData$CDS.Dates.IMM <- adjust(dates = MarketData$CDS.Dates.IMM,bdc = "f",calendar = EUTACalendar())

                # If OutputDates = NULL then assign them the Maturities
                if (is.null(x = OutputDates)) {
                        OutputDates <- MarketData$CDS.Dates.IMM
                }


                # Initial Value for Hazard Rate in bootstrapping
                HazardStart <- 0.01

                # Loop over every point in the CDS Term Structure to bootstrap a hazard rate
                for (i in 1:N) {
                        result <- optim(
                                par = HazardStart,
                                fn = bootstrap_quote,
                                i = i,
                                ValDate = ValDate,
                                Quotes = MarketData,
                                Curve = Curve,
                                Convention = Conv,
                                LGD = LGD,
                                method = "Brent",
                                lower = 0,
                                upper = 0.3,
                                control = list(abstol = 1e-3, reltol = 1e-2)
                        )

                        MarketData$Hazards[i] <- result$par
                }


                # Select correct Output and return
                if (Out == "Hazard") {
                        # Currently for hazards just return them with the Original Dates
                        MarketData[, 1] <- date_to_excel(d1 = MarketData$CDS.Dates)
                        return(MarketData[, c(1, 3)])
                } else{
                        # For both options we need the Survival rates
                        Survivals <- sapply(
                                X = OutputDates,
                                FUN = survival,
                                ValuationDate = ValDate,
                                Maturities = MarketData$CDS.Dates,
                                Hazards = MarketData$Hazards,
                                Convention = Conv
                        )

                        OutputFrame <- data.frame(Maturities = date_to_excel(OutputDates))

                        if (Out == "Survival") {
                                OutputFrame$Survivals <- Survivals
                                return(OutputFrame)
                        }
                        if (Out == "Default") {
                                Defaults <- 1 - Survivals
                                OutputFrame$Defaults <- Defaults
                                return(OutputFrame)
                        }
                }
        }
attr(bootstrap_cds, "description") <- list(
        "Bootstrap a CDS curve",
        Maturity = "",
        Spreads = "",
        Curve = "",
        ValDate = "",
        Conv = "act/360",
        OutputDates = "NULL",
        LGD = "0.6",
        Out = "Hazard"
)


#' Title
#'
#' @param Hazard
#' @param i
#' @param ValDate
#' @param Quotes
#' @param Curve
#' @param Convention
#' @param LGD
#'
#' @return
#'
#' @importFrom dplyr mutate
#'
#' @examples
bootstrap_quote <-
        function(Hazard,
                i,
                ValDate,
                Quotes,
                Curve,
                Convention,
                LGD) {

                # First put the Hazard (Parameter to optimize) in the Quotes
                Quotes$Hazards[i] <- Hazard

                # Bootstrap the maturity given the previously calculated Hazards unchanged
                Frame <- generate_schedule(effective_date = ValDate,termination_date = Quotes$CDS.Dates.IMM[i],tenor = months(3),calendar = EUTACalendar(),imm_dates = TRUE)

                # Frac is act/360 as by ISDA convention
                Frame$Frac <- yearfrac(
                        DateBegin = Frame$start_dates,
                        DateEnd = Frame$end_dates,
                        DayCountConv = "act/360"
                )

                # First period is "Riskless" before ValDate
                Frame$Frac[1] <- yearfrac(
                        DateBegin = ValDate,
                        DateEnd = Frame$end_dates[1],
                        DayCountConv = "act/360"
                )

                # Frac2 using user-input Convention? (For Survival)
                Frame$Frac2 <- yearfrac(
                        DateBegin = Frame$start_dates,
                        DateEnd = Frame$end_dates,
                        DayCountConv = Convention
                )

                # First period is "Riskless" before ValDate
                Frame$Frac2[1] <- yearfrac(
                        DateBegin = ValDate,
                        DateEnd = Frame$end_dates[1],
                        DayCountConv = Convention
                )

                # CDS - running spread for contract duration
                Frame$Spread <- as.numeric(Quotes[i, 2]) / 10000

                Frame$EndDiscount <-
                        interpolate(
                                X = Curve[, 1],
                                Y = Curve[, 2],
                                x = Frame$end_dates,
                                method = "cs"
                        )

                Frame$Hazards <- sapply(
                        X = Frame$end_dates,
                        FUN = function(x, y)
                                return(Quotes$Hazards[which.max(y >= x)]),
                        y = Quotes$CDS.Dates.IMM
                )

                # We are looping over here -> Try to find an efficient method
                Frame$Survival = NA
                for (i in 1:length(Frame$start_dates)) {
                        Frame$Survival[i] <- exp(-sum(Frame$Frac2[1:i] * Frame$Hazards[1:i]))
                }

                Frame <- mutate(.data = Frame, CumulDefault = 1 - Survival) %>%
                        mutate(PeriodDefault = c(1 - Survival[1], diff(Survival) * -1)) %>%
                        mutate(Premium = Frac * Spread * Survival * EndDiscount) %>%
                        mutate(Accrued = Frac * Spread * EndDiscount * PeriodDefault) %>%
                        mutate(Protection = LGD * EndDiscount * PeriodDefault)

                # Return TestValue
                TestValue <- ((sum(Frame$Premium) + sum(Frame$Accrued)) * 10000 - sum(Frame$Protection) * 10000) ^ 2

                return(TestValue)
        }
