#' Read and calculate benchmarks
#'
#' @param file.rep outputs of read.rep
#' @param file.par outputs of read.par
#' @param catch.rep string, file name of catch.rep
#' @param nofish LOGICAL if BF0 and SBF0 be calculate
#' @param nofishp range of time step from the last time step to calculate BF0 and SBF0
#' @param lateyr year to calculate "*_latest" quantity
#' @param version string, "latex" or "original"
#' @param re.keep  vector of string indicating which benchmarks to be included
#' @param veryrecent range of time step from the last time step to calculate "very recent" quantity
#' @param verbose LOGICAL verbose or not
#'
#' @export
get.outcomes <- function (file.rep=read.rep("SKJ14/plot-12.par.rep"), file.par=read.par("SKJ14/12.par"),
                              catch.rep="SKJ14/catch.rep",
                              nofish=TRUE, nofishp=c(44,4), lateyr=2014, version="original",
                              re.keep=c("bo","sbo"),veryrecent=c(3,0),verbose=FALSE) {

    ## Updated 9/30/2014 3:03:02 PM SJH - corrected F0 time period calcs which started 1 quarter to soon
    ## Updated 11/07/2014 9:33:29 AM SJH - user defined latest year
    ## Updated 11/06/2014 SJH - replaced 'no fishing' biomass calcs to be based on user specified periods
    ## similar to the flag settings for teh MSY time
    ## Updated 20/07/2009 SJH - change 'latest' to the average over the last year
    ## Updated 21/06/09 SJH
    ## Changes
    ## 1. To allow for FRQ and REP files to have already been read in (can either be path or name of file
    ## if in quotes it is directing to a path
    ## 2. Set missing to NA rather than 0
    ## 3. Calculate Blatest.Bmsy
    ## 4. revise the order to be consistent betwen B and SB
    ## 5. Multiply MSY and YieldFcurr by rp$nRecs.yr
    ## 6. Reads in a catch.rep file and get some recent catch stuff
    ## 06/07/2018 MTV commented out any Bcurr or SBcurr due to the changes in calculating the new reference points time periods. SBrec calculates the average over the time period of the last 4 years including the last year
    if(verbose) cat("Starting get.outcomes\n")
    if(is.character(file.rep))
    {
        rp <- read.rep(file.rep)

    } else {
        rp <- file.rep

    }

    if(is.character(file.par))
    {
        pr <- read.par(file.par)

    } else {
        pr <- file.par
    }

    ## lining up time pointers - MSY period calcs and then unfished biomass, and latestv(final year)
    msytime <- c(pr$afl[148]-1, pr$afl[155])
    msytime <- rp$nTimes - msytime
    if(verbose)cat("msytime=",msytime,"; \n")
    msytime <- seq(from=msytime[1],to=msytime[2],by=1)
    if(verbose) cat("msytime=",msytime,"; \n")
    vrectime <- rp$nTimes - veryrecent
    if(verbose) cat("vrectime=",vrectime,"; \n")
    F0time <- rp$nTimes - nofishp
    F0time <- seq(F0time[1]+1,F0time[2],by=1)

    timeint <- rp$Year1 + (1:rp$nTimes)/rp$nRecs.yr - 1/(2*rp$nRecs.yr)
    latetime <- which(trunc(timeint) %in% lateyr)
    if(rp$nRecs.yr>1 | length(latetime)>1){ # I do not know if latetime will be multiple
        if (dim(rp$TotBiomass)[1] > 1) {
            Blatest  <- mean(rowSums(rp$TotBiomass[latetime,]))
            SBlatest <- mean(rowSums(rp$AdultBiomass[latetime,]))
            ## Was used before SC14
            ## Bcurr  <- mean(apply(rp$TotBiomass[msytime,],1,sum))
            ## SBcurr <- mean(apply(rp$AdultBiomass[msytime,],1,sum))
            SBrec <- mean(apply(rp$AdultBiomass[vrectime,],1,sum))
        } else {
            Blatest  <- mean(rp$TotBiomass[latetime])
            SBlatest <- mean(rp$AdultBiomass[latetime])
            ## Was used before SC14
            ## Bcurr  <- mean(rp$TotBiomass[msytime])
            ## SBcurr <- mean(rp$AdultBiomass[msytime])
            SBrec <- mean(rp$AdultBiomass[vrectime])
        }
  }else{
    if (dim(rp$TotBiomass)[1] > 1) {
      Blatest  <- sum(rp$TotBiomass[latetime,])
      SBlatest <- sum(rp$AdultBiomass[latetime,])
  #    cat("L65\n");browser()
      if(length(msytime)>1){
        ## Bcurr  <- mean(apply(rp$TotBiomass[msytime,],1,sum))
        ## SBcurr <- mean(apply(rp$AdultBiomass[msytime,],1,sum))
        SBrec <- mean(apply(rp$AdultBiomass[vrectime,],1,sum))
      }else{
        ## Bcurr  <- sum(rp$TotBiomass[msytime,])
        ## SBcurr <- sum(rp$AdultBiomass[msytime,])
        SBrec <- sum(rp$AdultBiomass[vrectime,])
      }
    } else {
      Blatest  <- sum(rp$TotBiomass[latetime])
      SBlatest <- sum(rp$AdultBiomass[latetime])
      ## Bcurr  <- mean(rp$TotBiomass[msytime])
      ## SBcurr <- mean(rp$AdultBiomass[msytime])
      SBrec <- mean(rp$AdultBiomass[vrectime])

    }

  }


    BF0 <- SBF0 <- Blatest.BF0 <- SBlatest.SBF0 <- NA #<- Bcurr.BF0 <- SBcurr.SBF0

    if(nofish) {  # Using the F=0 window times provided
        if (dim(rp$TotalBiomass.nofish)[1] > 1) {
            BF0  <- mean(rowSums(rp$TotalBiomass.nofish[F0time,]))
            SBF0 <- mean(rowSums(rp$AdultBiomass.nofish[F0time,]))

        } else {
            BF0  <- mean(rp$TotalBiomass.nofish[F0time])
            SBF0 <- mean(rp$AdultBiomass.nofish[F0time])

        }

        Blatest.BF0 <- Blatest/BF0
        SBlatest.SBF0 <- SBlatest/SBF0
        ## Bcurr.BF0 <- Bcurr/BF0
        ## SBcurr.SBF0 <- SBcurr/SBF0
        SBrec.SBF0 <- SBrec/SBF0
        SBrec.SBmsy <- SBrec/rp$SBmsy
    }

    ## Bcurr.B0 <- Bcurr/rp$B0
    ## Bcurr.BFcurr <- Bcurr/rp$BFcurr
    ## Bcurr.Bmsy <- Bcurr/rp$Bmsy
    Blatest.B0 <- Blatest/rp$B0
    Blatest.Bmsy <- Blatest/rp$Bmsy

    ## SBcurr.SB0 <- SBcurr/rp$SB0
    SBlatest.SB0 <- SBlatest/rp$SB0
    ## SBcurr.SBFcurr <- SBcurr/rp$SBFcurr
    ## SBcurr.SBmsy <- SBcurr/rp$SBmsy
    SBlatest.SBmsy <- SBlatest/rp$SBmsy
    BFcurr.B0 <- rp$BFcurr/rp$B0
    SBFcurr.SB0 <- rp$SBFcurr/rp$SB0
    Bmsy.B0 <- rp$Bmsy/rp$B0
    SBmsy.SB0 <- rp$SBmsy/rp$SB0
    BFcurr.Bmsy <- rp$BFcurr/rp$Bmsy
    SBFcurr.SBmsy <- rp$SBFcurr/rp$SBmsy
    YFcurr.MSY <- as.numeric(rp$YFcurr/rp$MSY)

    ## read in total catches by time period from the catc.rep file
    catches <- scan(catch.rep, skip=1, nmax=rp$nTimes)
    ## Sum the catches in the last year - taking into account the model time step
    Clatest  <- sum(catches[(rp$nTimes-(rp$nRecs.yr-1)):rp$nTimes])
    ## Sum the catches over the MSY-calc period - taking into account the model time step
    Ccurr  <- sum(catches[msytime[1]:msytime[length(msytime)]])/ ((msytime[length(msytime)]-msytime[1]+1)/rp$nRecs.yr)

    ## Compare to MSY
    Ccurr.MSY <- Ccurr/(rp$MSY*rp$nRecs.yr)
    Clatest.MSY <- Clatest/(rp$MSY*rp$nRecs.yr)

    resout <- list(
        Ccurr = Ccurr,
        Clatest = Clatest,
        YFcurr = rp$YFcurr*rp$nRecs.yr,
        MSY = rp$MSY*rp$nRecs.yr,
        YFcurr.MSY = YFcurr.MSY,
        Ccurr.MSY =  Ccurr.MSY,
        Clatest.MSY = Clatest.MSY,

        Fmsy = rp$Fmsy,
        Fmult = rp$Fmult,
        Fcurr.Fmsy = 1/rp$Fmult,

        B0 = rp$B0,
        Bmsy = rp$Bmsy,
        Bmsy.B0 = Bmsy.B0,
        ## Bcurr = Bcurr,
        Blatest = Blatest,
        BFcurr = rp$BFcurr,
        BF0 = BF0,

        SB0 = rp$SB0,
        SBmsy = rp$SBmsy,
        SBmsy.SB0 = SBmsy.SB0,
        ## SBcurr = SBcurr,
        SBrec = SBrec,
        SBlatest = SBlatest,
        SBFcurr = rp$SBFcurr,
        SBF0 = SBF0,
        SBmsy.SBF0 = rp$SBmsy/SBF0,

        ## Bcurr.B0 = Bcurr.B0,
        Blatest.B0 = Blatest.B0,
        BFcurr.B0 = BFcurr.B0,

        ## Bcurr.Bmsy = Bcurr.Bmsy,
        Blatest.Bmsy = Blatest.Bmsy,
        BFcurr.Bmsy = BFcurr.Bmsy,

        ## Bcurr.BF0 = Bcurr.BF0,
        Blatest.BF0 = Blatest.BF0,

        ## SBcurr.SB0 = SBcurr.SB0,
        SBlatest.SB0 = SBlatest.SB0,
        SBFcurr.SB0 = SBFcurr.SB0,
        ## SBcurr.SBmsy = SBcurr.SBmsy,
        SBlatest.SBmsy = SBlatest.SBmsy,
        SBFcurr.SBmsy = SBFcurr.SBmsy,
        ## SBcurr.SBF0 = SBcurr.SBF0,
        SBrec.SBF0= SBrec.SBF0,
        SBrec.SBmsy= SBrec.SBmsy,
        SBlatest.SBF0 = SBlatest.SBF0,

        steep = rp$steep,

        obj = pr$obj,
        npars = pr$npars,
        gradient = pr$gradient,
        Mmin = min(rp$MatAge),
        Lmin = pr$Lmin, Lmax = pr$Lmax, K = pr$K
    )

    latout <- list(
        clatest = Clatest,
        yfcurr = rp$YFcurr*rp$nRecs.yr,
        msy = rp$MSY*rp$nRecs.yr,
        clatestmsy = Clatest.MSY,

        fmsy = rp$Fmsy,
        fmult = rp$Fmult,
        fref = 1/rp$Fmult,

        bo = rp$B0,
        bmsy = rp$Bmsy,
        ## bcurr = Bcurr,
        blatest = Blatest,
        bfo = BF0,

        sbo = rp$SB0,
        sbmsy = rp$SBmsy,
        sbmsysbo = SBmsy.SB0,
        ## sbcurr = SBcurr,
        sbcurr = SBrec,
        sblatest = SBlatest,
        sbfo = SBF0,
        sbmsysbfo =  rp$SBmsy/SBF0,

        blbmsy = Blatest.Bmsy,
        bcbmsy = BFcurr.Bmsy,

        ## bcbfo = Bcurr.BF0,
        blbfo = Blatest.BF0,

        ## These were the recent/current reference points before SC13
        ## sbcsbfo = SBcurr.SBF0,
        ## sbcsbmsy = SBcurr.SBmsy,
        ## These are the recent reference points for SC14
        sbcsbfo = SBrec.SBF0,
        sbcsbmsy = SBrec.SBmsy,
        sblsbmsy = SBlatest.SBmsy,
        sblsbo = SBlatest.SB0,
        sblsbfo = SBlatest.SBF0
    )

    latout <- latout[match(re.keep, names(latout))]
    latout <- unlist(latout)
    if(verbose)cat("Finished get.outcomes\n")
    if(version == "original")  return(resout)
    if(version == "latex") return(latout)
}


