 get.outcomes.test <-
function (file.rep,file.par,catch.rep,nofish=T)
{
# By Simon D Hoyle 2008
## Updated 21/06/09 SJH
# Changes
# 1. To allow for FRQ and REP files to have already been read in (can either be path or name of file
# if in quotes it is directing to a path
# 2. Set missing to NA rather than 0
# 3. Calculate Blatest.Bmsy
# 4. revise the order to be consistent betwen B and SB
# 5. Multiply MSY and YieldFcurr by rp$nRecs.yr
# 6. Reads in a catch.rep file and get some recent catch stuff

# Simple approach doesn't seem to work. 
  #rp <- ifelse(is.character(file.rep),read.rep(file.rep),file.rep)
  #pr <- ifelse(is.character(file.par),read.par(file.par),file.par)

# So ....
  if(is.character(file.rep))
  {
  rp <- read.rep(file.rep)
  }
  else
  {
  rp <- file.rep
  }

  if(is.character(file.par))
  {
  pr <- read.par(file.par)
  }
  else
  {
  pr <- file.par
  }

  #browser()

  msytime <- c(pr$afl[148]-1, pr$afl[155])
  msytime <- rp$nTimes - msytime
  timeint <- rp$Year1 + (1:rp$nTimes)/rp$nRecs.yr - 1/(2*rp$nRecs.yr) 
  if (dim(rp$TotBiomass)[1] > 1) {
    Blatest  <- sum(rp$TotBiomass[rp$nTimes,])
    SBlatest <- sum(rp$AdultBiomass[rp$nTimes,])
    Bcurr  <- mean(apply(rp$TotBiomass[msytime[1]:msytime[2],],1,sum))
    SBcurr <- mean(apply(rp$AdultBiomass[msytime[1]:msytime[2],],1,sum))
    } else {
    Bcurr  <- mean(rp$TotBiomass[msytime[1]:msytime[2]])
    SBcurr <- mean(rp$AdultBiomass[msytime[1]:msytime[2]])
    Blatest  <- rp$TotBiomass[rp$nTimes]
    SBlatest <- rp$AdultBiomass[rp$nTimes]
    }
  BF0byyr <- BcurrF0 <- SBF0byyr <- SBcurrF0 <- BlatestF0 <- SBlatestF0 <- Blatest.BlatestF0 <- SBcurr.SBcurrF0 <- SBlatest.SBlatestF0 <- Bcurr.BcurrF0 <- NA
# SJH 21/06/09
#  BF0byyr <- BcurrF0 <- SBF0byyr <- SBcurrF0 <- BlatestF0 <- SBlatestF0 <- Blatest.BlatestF0 <- SBcurr.SBcurrF0 <- SBlatest.SBlatestF0 <- Bcurr.BcurrF0 <- 0
  if(nofish==T) {
    if (dim(rp$TotalBiomass.nofish)[1] > 1) {
      BcurrF0  <- mean(apply(rp$TotalBiomass.nofish[msytime[1]:msytime[2],],1,sum))
      SBcurrF0 <- mean(apply(rp$AdultBiomass.nofish[msytime[1]:msytime[2],],1,sum))
      BlatestF0  <- sum(rp$TotalBiomass.nofish[rp$nTimes,])
      SBlatestF0 <- sum(rp$AdultBiomass.nofish[rp$nTimes,])
      } else {
      BcurrF0  <- mean(rp$TotalBiomass.nofish[msytime[1]:msytime[2]])
      SBcurrF0 <- mean(rp$AdultBiomass.nofish[msytime[1]:msytime[2]])
      BlatestF0  <- rp$TotalBiomass.nofish[rp$nTimes]
      SBlatestF0 <- rp$AdultBiomass.nofish[rp$nTimes]
      }
    Blatest.BlatestF0 <- Blatest/BlatestF0
    SBcurr.SBcurrF0 <- SBcurr/SBcurrF0
    SBlatest.SBlatestF0 <- SBlatest/SBlatestF0
    Bcurr.BcurrF0 <- Bcurr/BcurrF0
    }
  Bcurr.B0 <- Bcurr/rp$B0
  Bcurr.BFcurr <- Bcurr/rp$BFcurr
  Bcurr.Bmsy <- Bcurr/rp$Bmsy
# SJH 21/06/09
  Blatest.B0 <- Blatest/rp$B0
  Blatest.Bmsy <- Blatest/rp$Bmsy
#

  SBcurr.SB0 <- SBcurr/rp$SB0
  SBlatest.SB0 <- SBlatest/rp$SB0
  SBcurr.SBFcurr <- SBcurr/rp$SBFcurr
  SBcurr.SBmsy <- SBcurr/rp$SBmsy
  SBlatest.SBmsy <- SBlatest/rp$SBmsy
  BFcurr.B0 <- rp$BFcurr/rp$B0
  SBFcurr.SB0 <- rp$SBFcurr/rp$SB0
  Bmsy.B0 <- rp$Bmsy/rp$B0
  SBmsy.SB0 <- rp$SBmsy/rp$SB0
  BFcurr.Bmsy <- rp$BFcurr/rp$Bmsy
  SBFcurr.SBmsy <- rp$SBFcurr/rp$SBmsy
  YFcurr.MSY <- as.numeric(rp$YFcurr/rp$MSY)



# SJH 21/06/09 - adding in the catch stuff from catch.rep
  # red in total catches by time period from the catc.rep file
  catches <- scan(catch.rep, skip=1, nmax=rp$nTimes)
  # Sum the catches in the last year - taking into account the model time step
  Clatest  <- sum(catches[(rp$nTimes-(rp$nRecs.yr-1)):rp$nTimes])
  # Sum the catches over the MSY-calc period - taking into account the model time step
  Ccurr  <- sum(catches[msytime[1]:msytime[2]])/ ((msytime[2]-msytime[1]+1)/rp$nRecs.yr)
#browser()
# Compare to MSY
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
        Bcurr = Bcurr, 
        Blatest = Blatest, 
        BFcurr = rp$BFcurr, 
        BcurrF0 = BcurrF0, 
        BlatestF0 = BlatestF0,

        SB0 = rp$SB0, 
        SBmsy = rp$SBmsy, 
        SBmsy.SB0 = SBmsy.SB0, 
        SBcurr = SBcurr, 
        SBlatest = SBlatest, 
        SBFcurr = rp$SBFcurr, 
        SBcurrF0 = SBcurrF0, 
        SBlatestF0 = SBlatestF0, 

        Bcurr.B0 = Bcurr.B0, 
        Blatest.B0 = Blatest.B0,
        BFcurr.B0 = BFcurr.B0, 

        Bcurr.Bmsy = Bcurr.Bmsy, 
        Blatest.Bmsy = Blatest.Bmsy, 
        BFcurr.Bmsy = BFcurr.Bmsy, 

        Bcurr.BcurrF0 = Bcurr.BcurrF0, 
        Blatest.BlatestF0 = Blatest.BlatestF0, 
        # Is this really that useful?
        #Bcurr.BFcurr = Bcurr.BFcurr, 

        SBcurr.SB0 = SBcurr.SB0, 
        SBlatest.SB0 = SBlatest.SB0, 
        SBFcurr.SB0 = SBFcurr.SB0, 
        SBcurr.SBmsy = SBcurr.SBmsy, 
        SBlatest.SBmsy = SBlatest.SBmsy, 
        SBFcurr.SBmsy = SBFcurr.SBmsy, 
        SBcurr.SBcurrF0 = SBcurr.SBcurrF0, 
        SBlatest.SBlatestF0 = SBlatest.SBlatestF0,
        #SBcurr.SBFcurr = SBcurr.SBFcurr, 
        

        steep = rp$steep, 

        obj = pr$obj,
        npars = pr$npars,
        gradient = pr$gradient,
        Mmin = min(rp$MatAge),
        Lmin = pr$Lmin, Lmax = pr$Lmax, K = pr$K 
        )
  return(resout)
}
