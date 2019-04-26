rep.file <- "I:/assessments/Pop dy modeling/MFCL/R functions/testing/200201plot.rep"

read_nmd.rep <- function(rep.file) {
  # Simon Hoyle June 2008
  # NMD June 2011 - flexibility for tag reporting rates structure
  # SDH October 2011 - adapt for projections ##
  # NMD November 2011 - small fix for input of NexpbyYrFsh
  # NMD February 2014 - make flexible for multi-species input of F_year_age
  # NMD June 2015 - fix for function datfromstr to include the first item
#
  datfromstr2 <-function (datstring) 
  {
      return(as.numeric(unlist(strsplit(datstring, split = "[[:blank:]]+"))))
  }
  #  
  a <- readLines(rep.file)
  # find startpoint
  pos1 <- grep("Observed spawning Biomass",a) ;  top <- a[1:pos1]
  # find endppoint
  pos2 <- grep("F at MSY",a) ; rem <- a[(pos1+1):length(a)]
  # Check if multi-species input of F_year_age
  tmp <- grep("# Species",a)
  f_multspp <- FALSE
  if(length(tmp)!=0) f_multspp <- TRUE; rm(tmp);

  # load data
  pos1 <- grep("# Number of time periods",a) ; nTimes <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Year 1",a) ; Year1 <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Number of regions",a) ; nReg <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Number of age classes",a) ; nAges <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Number of recruitments per year",a) ; nRecs.yr <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Number of fisheries",a) ; nFisheries <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Number of realizations per fishery",a) ; nRlz.fsh <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Region for each fishery",a) ; Region.fsh <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-c(1)])
  pos1 <- grep("# Time of each realization by fishery",a) ; Rlz.t.fsh <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh))
    for (i in 1:nFisheries) { Rlz.t.fsh[i,1:nRlz.fsh[i]] <- as.numeric(unlist(strsplit(a[pos1+i],split="[[:blank:]]+"))[-1]) }
  pos1 <- grep("# Mean lengths at age",a) ; mean.LatAge <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# SD of length at age",a) ; sd.LatAge <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Mean weights at age",a) ; mean.WatAge <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Natural mortality at age",a) ; MatAge <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Selectivity by age class ",a) ; SelAtAge <- t(sapply(a[(pos1+1):(pos1+nFisheries)],datfromstr,USE.NAMES =F))
  pos1 <- grep("# Catchability by realization ",a) ; qAtAge <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh)) ;
    for(i in 1:nFisheries) { qAtAge[i,1:nRlz.fsh[i]] <- as.numeric(unlist(strsplit(a[pos1+i],split="[[:blank:]]+"))[-1]) }
  pos1 <- grep("# Catchability\\+effort dev\\. by realization ",a) ; qEdevAtAge <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh)) ;
    for(i in 1:nFisheries) { qEdevAtAge[i,1:nRlz.fsh[i]] <- as.numeric(unlist(strsplit(a[pos1+i],split="[[:blank:]]+"))[-1]) }
  pos1 <- grep("# Fishing mortality by age class \\(across\\) and year \\(down\\)",a) ; 
  if(f_multspp){
    FbyAgeYr <- t(sapply(a[(pos1+2):(pos1+nTimes+1)],datfromstr,USE.NAMES =F))
  } else {
    FbyAgeYr <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
  }  

  pos1 <- grep("# Fishing mortality by age class \\(across\\), year \\(down\\) and region \\(block\\)",a) ; FatYrAgeReg <- array(dim=c(nTimes,nAges,nReg)) ;
    for(j in 1:nReg) {
      pos1 <- pos1 + 1
      for(i in 1:nTimes) {
        pos1 <- pos1 + 1
        FatYrAgeReg[i,,j] <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1]) } }

#  browser()
# SDH 2011/10/24 added 4 lines so the code works when extra comment text is added for projections
# NMD 2015/09/03 added 4 lines so the code works for later version of plot.rep
  posyr <- rep(1,nTimes)
  if(length(grep("#   Projected",a, ignore.case = T)) > 0) {
    tmp <- c("# Exploitable population biomass by fishery \\(down\\) and by year-season  \\(across\\)")
    tmp2 <- c("# Exploitable population biomass by fishery \\(across\\) and year \\(down\\)")
    if(length(grep(tmp,a))>0 & length(grep(tmp2,a))>0){   #latest version of plot.rep file
      projyrs <- grep(tmp2,a) - max(grep("#   Projected",a, ignore.case = T)) - 1
    } else if(length(grep(tmp,a))>0 & length(grep(tmp2,a))==0){   #later version of plot.rep file
      projyrs <- grep(tmp,a) - max(grep("#   Projected",a, ignore.case = T)) - 1
    } else { # older version
      projyrs <- grep(tmp2,a) - max(grep("#   Projected",a, ignore.case = T)) - 1
    }
    posyr[nTimes-projyrs+1] <- 2
  }
  pos1 <- grep("# Population Number by age \\(across\\), year \\(down\\) and region",a, ignore.case = T) ; NatYrAgeReg <- array(dim=c(nTimes,nAges,nReg)) ;
  for(j in 1:nReg) {
    pos1 <- pos1 + 1
    for(i in 1:nTimes) {
      pos1 <- pos1 + posyr[i] # SDH 2011/10/24 changed from  'pos1 <- pos1 + 1'
      NatYrAgeReg[i,,j] <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1]) 
    }
  }
  pos1 <- grep("# Exploitable population by fishery \\(across\\) and year \\(down\\)",a)
  strtag <- 0
  if(length(pos1)==1)  {   # old version
    pos1 <- grep("# Exploitable population by fishery \\(across\\) and year \\(down\\)",a) 
  } else {   # new version
    pos1 <- grep("# Exploitable population biomass by fishery \\(across\\) and year \\(down\\)",a)
    if(length(pos1)==1)  {   # new version
      pos1 <- grep("# Exploitable population biomass by fishery \\(across\\) and year \\(down\\)",a)
    } else {   # latest version
      pos1 <- grep("# Exploitable population biomass by fishery \\(down\\) and by year-season  \\(across\\)",a)
      strtag <- 1
    }
  }
  if(!strtag) {
    NexpbyYrFsh <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
  } else {
    NexpbyYrFsh <- t(sapply(a[(pos1+1):(pos1+nFisheries)],datfromstr2,USE.NAMES =F))
  }
  pos1 <- grep("# Exploitable population in same units as catch by fishery \\(across\\) and year \\(down\\)",a) ;
  strtag <- 0
  if(length(pos1)==1)  {   # old version
    pos1 <- grep("# Exploitable population in same units as catch by fishery \\(across\\) and year \\(down\\)",a) ;
  } else {   # new version
    pos1 <- grep("# Exploitable population in same units as catch by fishery \\(down\\) and year-season \\(across\\)",a)
    strtag <- 1
  }
  if(length(pos1)!=0) {
    if(!strtag){
      ExPopCUnitsbyYrFsh <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
    } else if(strtag==1) {
      ExPopCUnitsbyYrFsh <- t(sapply(a[(pos1+1):(pos1+nFisheries)],datfromstr2,USE.NAMES =F))    
# NMD correction to use datfromstr2 as there is no leading space
    }
  } else ExPopCUnitsbyYrFsh <- NA
  pos1 <- grep("# Absolute biomass by region \\(across\\) and year \\(down\\)",a) ; Recruitment <- t(sapply(a[(pos1+2):(pos1+1+nTimes)],datfromstr,USE.NAMES =F))
  pos1 <- grep("# Total biomass$",a) ; TotBiomass <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
  pos1 <- grep("# Adult biomass$",a) ; AdultBiomass <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
  pos1 <- grep("# Relative biomass by region \\(across\\) and year \\(down\\)$",a) ; RelBiomass <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))

  pos1 <- grep("# Observed catch by fishery \\(down\\) and time \\(across\\)",a) ; ObsCatch <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh)) ;
    for(i in 1:nFisheries) { ObsCatch[i,1:nRlz.fsh[i]] <- datfromstr(a[pos1+i]) }

  pos1 <- grep("# Predicted catch by fishery \\(down\\) and time \\(across\\)",a) ; PredCatch <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh)) ;
    for(i in 1:nFisheries) { PredCatch[i,1:nRlz.fsh[i]] <- datfromstr(a[pos1+i]) }

  pos1 <- grep("# Observed CPUE by fishery \\(down\\) and time \\(across\\)",a) ; ObsCPUE <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh)) ;
    for(i in 1:nFisheries) { ObsCPUE[i,1:nRlz.fsh[i]] <- datfromstr(a[pos1+i]) }

  pos1 <- grep("# Predicted CPUE by fishery \\(down\\) and time \\(across\\)",a) ; PredCPUE <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh)) ;
    for(i in 1:nFisheries) { PredCPUE[i,1:nRlz.fsh[i]] <- datfromstr(a[pos1+i]) }

  pos1 <- grep("# Yield analysis option",a) ; YieldOption <- as.numeric(a[pos1+1])
  if(YieldOption==1)
  {
    chk <- grep("# BH autocorrelation",a)  # check for recent version
    if(length(chk) > 0){
      if(length(chk) == 1){
        nlns <- 4
      } else if(length(chk) == 2){
        nlns <- 5
      }
    } else {
      nlns <- 3
    }
    SRR <- as.numeric(unlist(strsplit(a[pos1+nlns],split="[[:blank:]]+"))[c(4,7,10,13)])
   }

  pos1 <- grep("# Observed spawning Biomass",a) ; Obs.SB <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Observed recruitment",a) ; Obs.R <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Spawning Biomass",a) ; Pred.SB <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Predicted recruitment",a) ; Pred.R <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("steepness =",a) ; steep <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[10])
  pos1 <- grep("# MSY",a) ; MSY <- as.numeric(a[pos1+1])
  pos1 <- grep("# F multiplier at MSY",a) ; Fmult <- as.numeric(a[pos1+1])
  pos1 <- grep("# F at MSY",a); Fmsy <- as.numeric(a[pos1+1])
  pos1 <- grep("# Total biomass at MSY",a) ; Bmsy <- as.numeric(a[pos1+1])
  pos1 <- grep("# Adult biomass at MSY",a) ; SBmsy <- as.numeric(a[pos1+1])
  pos1 <- grep("# current Total Biomass to Total biomass at MSY",a) ; B.Bmsy <- as.numeric(a[pos1+1])
  pos1 <- grep("# current Adult Biomass to Adult Biomass at MSY",a) ; SB.SBmsy <- as.numeric(a[pos1+1])
  pos1 <- grep("# Effort multiplier",a)[1] ; Effmult <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Equilibrium yield",a) ; Eq.yield <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
#  browser()
  YFcurr <- Eq.yield[Effmult==1]
  if(length(YFcurr)==0) YFcurr <- 0
  pos1 <- grep("# Equilibrium adult biomass",a) ; Eq.SB <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  SBFcurr <- Eq.SB[Effmult==1]
  SB0 <- Eq.SB[Effmult==0]
  if(length(SBFcurr)==0) SBFcurr <- 0
  pos1 <- grep("# Equilibrium total biomass",a) ; Eq.B <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  BFcurr <- Eq.B[Effmult==1]
  if(length(BFcurr)==0) BFcurr <- 0
  B0 <- Eq.B[Effmult==0]
  pos1 <- grep("# Adult biomass over adult biomass at MSY",a) ; Eq.SB.SBmsy <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Total biomass over total biomass at MSY",a) ; Eq.B.Bmsy <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Aggregate F over F at MSY",a) ; Eq.F.Fmsy <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Aggregate F$",a) ; Eq.aggF <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Effort multiplier",a)[2] ; YPR.effmult <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Yield per recruit",a)[2] ; YPR <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  TagGrps <- RepRates <- nTagRetPds <- TagRetPds <- ObsTagReturns <- PredTagReturns <- MaxLiberty <- ObsvPredbyLib <- 0
  pos1 <- grep("# Grouping indicator \\(0",a) ;
#  browser()
  if (length(pos1)>0) {
    TagGrps <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    if (max(TagGrps)==0) { TagGrps <- 1:length(TagGrps) }
    pos1 <- grep("# Reporting rates by fishery \\(no time",a) ;
    xxx <- grep("# Reporting rates by by fishery by tag group \\(no time",a)
    if(length(pos1)>0) {
      RepRates <- as.numeric(a[(pos1+1):(pos1+nFisheries)])
    }
    if(length(xxx)>0) {
      pos1 <- xxx
      rm(xxx)
      RepRates <- sapply(a[(pos1+1):(pos1+nFisheries)],datfromstr,USE.NAMES =F)
    }
    pos1 <- grep("# No. of time periods associated with tag returns",a) ; nTagRetPds <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# Time periods associated with ",a) ; TagRetPds <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# Observed tag returns by time period",a) ; ObsTagReturns <- sapply(a[(pos1+1):(pos1+max(TagGrps))],datfromstr,USE.NAMES =F)
    pos1 <- grep("# Predicted tag returns by time period",a); PredTagReturns <- sapply(a[(pos1+1):(pos1+max(TagGrps))],datfromstr,USE.NAMES =F)
    pos1 <- grep("# Maximum time at liberty",a) ; MaxLiberty <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# Observed vs predicted tag returns by time at liberty",a); ObsvPredbyLib <- t(sapply(a[(pos1+1):(pos1+MaxLiberty)],datfromstr,USE.NAMES =F))
    }
  if (nReg>1) {
    pos1 <- grep("# Movement analysis",a);
    nMPars <- grep("# Region 2",a[pos1:length(a)])[1] - 3;
   MoveRates <- array(dim=c(nMPars,nReg,nReg)) ;
   for (i in 1:nReg) {
      MoveRates[,,i] <- t(sapply(a[(pos1+2):(pos1+nMPars+1)],datfromstr,USE.NAMES =F))
      pos1 <- pos1 + nMPars + 1
      }
   } else { MoveRates <- NA }
  pos1 <- grep("# length-sample components of likelihood by fishery",a) ; lenLiks <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# weight-sample components of likelihood by fishery",a) ; wtLiks <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])

  pos1 <- grep("# Total biomass in absence of fishing",a) ;
  if(length(pos1)>0) {
    TotalBiomass.nofish <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
    pos1 <- grep("# Adult biomass in absence of fishing",a) ; AdultBiomass.nofish <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
    pos1 <- grep("# Exploitable populations in absence of fishing",a) ; ExplBiomass.nofish <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
    pos1 <- grep("# Predicted catch for interaction analysis",a) ; PredCatch.interact <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh)) ;
      for(i in 1:nFisheries) { PredCatch.interact[i,1:nRlz.fsh[i]] <- datfromstr(a[pos1+i]) }
    } else { TotalBiomass.nofish <- AdultBiomass.nofish <- ExplBiomass.nofish  <- PredCatch.interact <- NULL }

  rep.obj <- list(nTimes=nTimes,Year1=Year1,nReg=nReg,nAges=nAges,nRecs.yr=nRecs.yr,
            nFisheries=nFisheries,nRlz.fsh=nRlz.fsh,Region.fsh=Region.fsh,Rlz.t.fsh=Rlz.t.fsh,mean.LatAge=mean.LatAge,
            sd.LatAge=sd.LatAge,mean.WatAge=mean.WatAge,
            MatAge=MatAge,SelAtAge=SelAtAge,qAtAge=qAtAge,qEdevAtAge=qEdevAtAge,FbyAgeYr=FbyAgeYr,FatYrAgeReg=FatYrAgeReg,
            NatYrAgeReg=NatYrAgeReg,NexpbyYrFsh=NexpbyYrFsh,ExPopCUnitsbyYrFsh=ExPopCUnitsbyYrFsh,
            Recruitment=Recruitment,TotBiomass=TotBiomass,AdultBiomass=AdultBiomass,
            RelBiomass=RelBiomass,ObsCatch=ObsCatch,PredCatch=PredCatch,ObsCPUE=ObsCPUE,PredCPUE=PredCPUE,
            YieldOption=YieldOption,SRR=SRR,
            Obs.SB=Obs.SB, Obs.R=Obs.R, Pred.SB=Pred.SB, Pred.R=Pred.R,
            steep = steep,
            MSY=MSY, Fmult=Fmult,
            Fmsy=Fmsy, Bmsy=Bmsy, SBmsy=SBmsy, B.Bmsy=B.Bmsy, SB.SBmsy=SB.SBmsy,
            Effmult=Effmult, Eq.yield=Eq.yield, YFcurr=YFcurr, Eq.SB=Eq.SB, SBFcurr=SBFcurr, SB0=SB0, Eq.B=Eq.B, B0=B0, BFcurr=BFcurr, Eq.SB.SBmsy=Eq.SB.SBmsy,
            Eq.B.Bmsy=Eq.B.Bmsy, Eq.F.Fmsy=Eq.F.Fmsy, Eq.aggF=Eq.aggF, YPR.effmult=YPR.effmult, YPR=YPR,
            TagGrps=TagGrps,RepRates=RepRates,nTagRetPds=nTagRetPds,TagRetPds=TagRetPds,
            ObsTagReturns=ObsTagReturns,PredTagReturns=PredTagReturns,MaxLiberty=MaxLiberty,ObsvPredbyLib=ObsvPredbyLib,
            MoveRates=MoveRates,
            lenLiks=lenLiks,wtLiks=wtLiks,
            TotalBiomass.nofish=TotalBiomass.nofish,AdultBiomass.nofish=AdultBiomass.nofish,ExplBiomass.nofish=ExplBiomass.nofish,
            PredCatch.interact=PredCatch.interact)
  return(rep.obj)
}
