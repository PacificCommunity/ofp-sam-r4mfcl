rep.file <- "P:/skulls/sppsex/Testing/2012-04-29/msm_sngl_eval/plot-junk5.par.rep"
rep2.file <- "P:/skulls/sppsex/cutbet_replct/age28_bet/plot-12.par.rep"


read.mult.rep <- function(rep.file) {
# NMD 18 Nov 2011 - took read.rep and modified for reading in multi-spp case
  a <- readLines(rep.file)
#
# Identify format
  pos1 <- grep("# MULTIFAN-CL Viewer",a)  
  tmp <- a[(pos1+1)]
  frmt <- floor(as.numeric(substring(tmp,3,5)))
#          frmt = 2 is the old format (without multi-spp capability)
#          frmt = 3 is the new format (with multi-spp capability)
#
  # load data
  pos1 <- grep("# Number of time periods",a) ; nTimes <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Year 1",a) ; Year1 <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Number of regions",a) ; nReg <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])

  # Identify number of species
  if(frmt > 2){
    pos1 <- grep("# Number of species",a)
    nSpp <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    if(nSpp > 1){
      ms_flag <- 1
      nspp <- nSpp
    }
  } else {
    nSpp <- 1     # always 1 species for the old format
    ms_flag <- 0
    nspp <- nSpp
  }
  pos1 <- grep("# Number of age classes",a) ; 
  if(nSpp == 1 | frmt < 3){
    nAges <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  } else {
    nAges <- list()
    for(i in 1:nSpp){
      nAges[[i]] <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])[i]
    }
  }
  if(frmt > 2){
    pos1 <- grep("# Regions species pointer",a) ; 
    reg.Spp <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  } else {
    reg.Spp <- rep(1, times=nReg)
  }
  pos1 <- grep("# Number of recruitments per year",a) ; nRecs.yr <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Number of fisheries",a) ; nFisheries <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Number of realizations per fishery",a) ; nRlz.fsh <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Region for each fishery",a) ; Region.fsh <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-c(1)])
  pos1 <- grep("# Time of each realization by fishery",a) ; Rlz.t.fsh <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh))
  for (i in 1:nFisheries) { Rlz.t.fsh[i,1:nRlz.fsh[i]] <- as.numeric(unlist(strsplit(a[pos1+i],split="[[:blank:]]+"))[-1]) }

  if(nSpp == 1 | frmt < 3){
    pos1 <- grep("# Mean lengths at age",a) ; mean.LatAge <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# SD of length at age",a) ; sd.LatAge <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# Mean weights at age",a) ; mean.WatAge <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# Natural mortality at age",a) ; MatAge <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  } else {
    pos1 <- grep("# Mean lengths at age",a)
    pos2 <- grep("# SD of length at age",a)
    pos3 <- grep("# Mean weights at age",a)
    pos4 <- grep("# Natural mortality at age",a)
    if(length(pos1) != length(pos2) | length(pos2) != length(pos3) | length(pos1) != length(pos3) | length(pos1) != nSpp){
      print(" Error in read in of Lengths and Weights by age class ")
      stop()
    }
    mean.LatAge <- list()
    sd.LatAge <-  list()
    mean.WatAge <-  list()
    MatAge <-  list()
    for(i in 1:nSpp){
      mean.LatAge[[i]] <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      sd.LatAge[[i]] <- as.numeric(unlist(strsplit(a[pos2[i]+1],split="[[:blank:]]+"))[-1])
      mean.WatAge[[i]] <- as.numeric(unlist(strsplit(a[pos3[i]+1],split="[[:blank:]]+"))[-1])
      MatAge[[i]] <- as.numeric(unlist(strsplit(a[pos4+i],split="[[:blank:]]+"))[-1])
    }
  }
  pos1 <- grep("# Selectivity by age class ",a)
  pos2 <- grep("# length bin mid-points",a)
  SelAtAge <- t(sapply(a[(pos1+1):(pos2-1)],datfromstr,USE.NAMES =F))
  pos1 <- grep("# Catchability by realization ",a) ; qAtAge <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh)) ;
    for(i in 1:nFisheries) { qAtAge[i,1:nRlz.fsh[i]] <- as.numeric(unlist(strsplit(a[pos1+i],split="[[:blank:]]+"))[-1]) }
  pos1 <- grep("# Catchability\\+effort dev\\. by realization ",a) ; qEdevAtAge <- matrix(nrow=nFisheries,ncol=max(nRlz.fsh)) ;
    for(i in 1:nFisheries) { qEdevAtAge[i,1:nRlz.fsh[i]] <- as.numeric(unlist(strsplit(a[pos1+i],split="[[:blank:]]+"))[-1]) }
  FbyAgeYr <- list()
  if(frmt > 2){
    if(nSpp > 1){
      for(i in 1:nSpp){
        pos1 <- grep("# Fishing mortality by age class \\(across\\) and year \\(down\\)",a) 
        pos2 <- grep(paste("# Species ",i,sep=""),a)
        if(!length(pos2) > 0 | !length(pos1) > 0){
          print(" Error in read in of Fishing mortality by age class ")
          stop()
        } else if((pos2 - pos1) != 1 & i==1){
          print(" Error in read in of Fishing mortality by age class ")
          stop()
        } else if((pos2 - pos1) != (i + ((i-1)*nTimes))) {
          print(" Error in read in of Fishing mortality by age class ")
          stop()
        }            
        FbyAgeYr[[i]] <- t(sapply(a[(pos2+1):(pos2+nTimes)],datfromstr,USE.NAMES =F))
      }
    } else {
      pos1 <- grep("# Fishing mortality by age class \\(across\\) and year \\(down\\)",a)
      FbyAgeYr <- t(sapply(a[(pos1+2):(pos1+1+nTimes)],datfromstr,USE.NAMES =F))
    }
  } else {
    pos1 <- grep("# Fishing mortality by age class \\(across\\) and year \\(down\\)",a)
    FbyAgeYr <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
  }
  pos1 <- grep("# Fishing mortality by age class \\(across\\), year \\(down\\) and region \\(block\\)",a) ; 
  if(nSpp > 1){
    tmpAges <- nAges[[1]]
  } else {
    tmpAges <- nAges
  }
  FatYrAgeReg <- array(dim=c(nTimes,tmpAges,nReg)) ;
    for(j in 1:nReg) {
      pos1 <- pos1 + 1
      for(i in 1:nTimes) {
        pos1 <- pos1 + 1
        FatYrAgeReg[i,,j] <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1]) } }

# SDH 2011/10/24 added 4 lines so the code works when extra comment text is added for projections
  posyr <- rep(1,nTimes)
  if(length(grep("#   Projected",a, ignore.case = T)) > 0) {
    projyrs <- grep("# Exploitable population biomass by fishery \\(across\\) and year \\(down\\)",a) - max(grep("#   Projected",a, ignore.case = T)) - 1
    posyr[nTimes-projyrs+1] <- 2
    }
  pos1 <- grep("# Population Number by age \\(across\\), year \\(down\\) and region",a, ignore.case = T) ; NatYrAgeReg <- array(dim=c(nTimes,tmpAges,nReg)) ;
    for(j in 1:nReg) {
      pos1 <- pos1 + 1
      for(i in 1:nTimes) {
        pos1 <- pos1 + posyr[i] # SDH 2011/10/24 changed from  'pos1 <- pos1 + 1'
        NatYrAgeReg[i,,j] <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1]) }
      }

  pos1 <- grep("# Exploitable population by fishery \\(across\\) and year \\(down\\)",a)
  strtag <- 0
  if(length(pos1)==1)  {
    pos1 <- grep("# Exploitable population by fishery \\(across\\) and year \\(down\\)",a) 
  } else {
    pos1 <- grep("# Exploitable population biomass by fishery \\(across\\) and year \\(down\\)",a)
    if(length(pos1)==1)  {
      pos1 <- grep("# Exploitable population biomass by fishery \\(across\\) and year \\(down\\)",a)
    } else {
      pos1 <- grep("# Exploitable population biomass by fishery \\(down\\) and by year-season  \\(across\\)",a)
      strtag <- 1
    }
  }
  if(!strtag) {
    NexpbyYrFsh <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
  } else {
    NexpbyYrFsh <- t(sapply(a[(pos1+1):(pos1+nFisheries)],datfromstr,USE.NAMES =F))
  }

  pos1 <- grep("# Exploitable population in same units as catch by fishery \\(across\\) and year \\(down\\)",a) ;
  strtag <- 0
  if(length(pos1)==1)  {
    pos1 <- grep("# Exploitable population in same units as catch by fishery \\(across\\) and year \\(down\\)",a) ;
  } else {
    pos1 <- grep("# Exploitable population in same units as catch by fishery \\(down\\) and year-season \\(across\\)",a)
    strtag <- 1
  }
  if(length(pos1)!=0) {
    if(!strtag){
      ExPopCUnitsbyYrFsh <- t(sapply(a[(pos1+1):(pos1+nTimes)],datfromstr,USE.NAMES =F))
    } else if(strtag==1) {
      ExPopCUnitsbyYrFsh <- t(sapply(a[(pos1+1):(pos1+nFisheries)],datfromstr,USE.NAMES =F))    
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
#---------------------------------------------------------------------#
# Following only occurs if yield has been calculated #
  if(YieldOption > 0){
#
# Check number of species reported: nSpp for multi-species model but 1 for multi-sex
  pos0 <- grep("# Beverton-Holt stock-recruitment relationship",a)
  if(length(pos0) > 1) {
    ms_flag <- 1
    nspp <- length(pos0)
  } else {
    ms_flag <- 0
    if(nSpp > 1) print("Multi-sex yield report")
    nspp <- 1
  }
  if(ms_flag==1){
    yield_res <- list()
    for(i in 1:nspp){
      yield_res[[i]] <- list()
    }
  }    
  if(ms_flag != 1){
    if(YieldOption==1)
    {
      SRR <- as.numeric(unlist(strsplit(a[pos1+3],split="[[:blank:]]+"))[c(4,7,10)])
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
    ptr <- Effmult[c(1:length(Eq.SB))]
    ptr[is.na(ptr)] <- 10
    SBFcurr <- Eq.SB[ptr==1]
    SB0 <- Eq.SB[ptr==0]
    if(length(SBFcurr)==0) SBFcurr <- 0
    pos1 <- grep("# Equilibrium total biomass",a) ; Eq.B <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    ptr <- Effmult[c(1:length(Eq.B))]
    ptr[is.na(ptr)] <- 10
    BFcurr <- Eq.B[Effmult==1]
    if(length(BFcurr)==0) BFcurr <- 0
    B0 <- Eq.B[ptr==0]
    pos1 <- grep("# Adult biomass over adult biomass at MSY",a) ; Eq.SB.SBmsy <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# Total biomass over total biomass at MSY",a) ; Eq.B.Bmsy <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# Aggregate F over F at MSY",a) ; Eq.F.Fmsy <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# Aggregate F$",a) ; Eq.aggF <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# Effort multiplier",a)[2] ; YPR.effmult <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# Yield per recruit",a)[2] ; YPR <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  } else {
    for(i in 1:nspp){
      pos0 <- grep("# Beverton-Holt stock-recruitment relationship",a)
      if(YieldOption==1)
      {
        yield_res[[i]]$SRR <- as.numeric(unlist(strsplit(a[pos0[i]+1],split="[[:blank:]]+"))[c(4,7,10)])
      }
      pos1 <- grep("# Observed spawning Biomass",a) ; yield_res[[i]]$Obs.SB <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      pos1 <- grep("# Observed recruitment",a) ; yield_res[[i]]$Obs.R <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      pos1 <- grep("# Spawning Biomass",a) ; yield_res[[i]]$Pred.SB <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      pos1 <- grep("# Predicted recruitment",a) ; yield_res[[i]]$Pred.R <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      pos1 <- grep("steepness =",a) ; yield_res[[i]]$steep <- as.numeric(unlist(strsplit(a[pos1[i]],split="[[:blank:]]+"))[10])
      pos1 <- grep("# MSY",a) ; yield_res[[i]]$MSY <- as.numeric(a[pos1[i]+1])
      pos1 <- grep("# F multiplier at MSY",a) ; yield_res[[i]]$Fmult <- as.numeric(a[pos1[i]+1])
      pos1 <- grep("# F at MSY",a); yield_res[[i]]$Fmsy <- as.numeric(a[pos1[i]+1])
      pos1 <- grep("# Total biomass at MSY",a) ; yield_res[[i]]$Bmsy <- as.numeric(a[pos1[i]+1])
      pos1 <- grep("# Adult biomass at MSY",a) ; yield_res[[i]]$SBmsy <- as.numeric(a[pos1[i]+1])
      pos1 <- grep("# current Total Biomass to Total biomass at MSY",a) ; yield_res[[i]]$B.Bmsy <- as.numeric(a[pos1[i]+1])
      pos1 <- grep("# current Adult Biomass to Adult Biomass at MSY",a) ; yield_res[[i]]$SB.SBmsy <- as.numeric(a[pos1[i]+1])
      pos1 <- grep("# Effort multiplier",a) ; yield_res[[i]]$Effmult <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      pos1 <- grep("# Equilibrium yield",a) ; yield_res[[i]]$Eq.yield <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      yield_res[[i]]$YFcurr <- yield_res[[i]]$Eq.yield[yield_res[[i]]$Effmult==1]
      if(length(yield_res[[i]]$YFcurr)==0) yield_res[[i]]$YFcurr <- 0
      pos1 <- grep("# Equilibrium adult biomass",a) ; yield_res[[i]]$Eq.SB <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      yield_res[[i]]$SBFcurr <- yield_res[[i]]$Eq.SB[yield_res[[i]]$Effmult==1]
      yield_res[[i]]$SB0 <- yield_res[[i]]$Eq.SB[yield_res[[i]]$Effmult==0]
      if(length(yield_res[[i]]$SBFcurr)==0) yield_res[[i]]$SBFcurr <- 0
      pos1 <- grep("# Equilibrium total biomass",a) ; yield_res[[i]]$Eq.B <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      yield_res[[i]]$BFcurr <- yield_res[[i]]$Eq.B[yield_res[[i]]$Effmult==1]
      if(length(yield_res[[i]]$BFcurr)==0) yield_res[[i]]$BFcurr <- 0
      yield_res[[i]]$B0 <- yield_res[[i]]$Eq.B[yield_res[[i]]$Effmult==0]
      pos1 <- grep("# Adult biomass over adult biomass at MSY",a) ; yield_res[[i]]$Eq.SB.SBmsy <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      pos1 <- grep("# Total biomass over total biomass at MSY",a) ; yield_res[[i]]$Eq.B.Bmsy <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      pos1 <- grep("# Aggregate F over F at MSY",a) ; yield_res[[i]]$Eq.F.Fmsy <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      pos1 <- grep("# Aggregate F$",a) ; yield_res[[i]]$Eq.aggF <- as.numeric(unlist(strsplit(a[pos1[i]+1],split="[[:blank:]]+"))[-1])
      pos0 <- grep("# Yield per recruit report",a)
      if(length(pos0) > 0){
        pos1 <- grep("# Effort multiplier",a[pos0:length(a)])[1] ; yield_res[[i]]$YPR.effmult <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
        pos1 <- grep("# Yield per recruit",a[pos0:length(a)])[1] ; yield_res[[i]]$YPR <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
      } else {
        YPR.effmult <- NA
        YPR <- NA
      }
    }
  }
#
  } else {
# end of yield calculations
    ms_flag <- 0
    SRR <- Obs.SB <- Obs.R <- Pred.SB <- Pred.R <- steep <- MSY <- Fmult <- NA
    Fmsy <- Bmsy <- SBmsy <- B.Bmsy <- SB.SBmsy <- Effmult <- Eq.yield <- NA
    YFcurr <- Eq.SB <- SBFcurr <- SB0 <- Eq.B <- B0 <- BFcurr <- Eq.SB.SBmsy <- NA
    Eq.B.Bmsy <- Eq.F.Fmsy <- Eq.aggF <- YPR.effmult <- YPR <- 0
  }
#---------------------------------------------------------------------#
#
  TagGrps <- RepRates <- nTagRetPds <- TagRetPds <- ObsTagReturns <- PredTagReturns <- MaxLiberty <- ObsvPredbyLib <- 0
  pos1 <- grep("# Grouping indicator \\(0",a) ;
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


  if(ms_flag == 1){    # multi-species yield results
    rep.obj <- list(nTimes=nTimes,Year1=Year1,nReg=nReg,nAges=nAges,reg.Spp=reg.Spp,nRecs.yr=nRecs.yr,
              nFisheries=nFisheries,nRlz.fsh=nRlz.fsh,Region.fsh=Region.fsh,Rlz.t.fsh=Rlz.t.fsh,mean.LatAge=mean.LatAge,
              sd.LatAge=sd.LatAge,mean.WatAge=mean.WatAge,
              MatAge=MatAge,SelAtAge=SelAtAge,qAtAge=qAtAge,qEdevAtAge=qEdevAtAge,FbyAgeYr=FbyAgeYr,FatYrAgeReg=FatYrAgeReg,
              NatYrAgeReg=NatYrAgeReg,NexpbyYrFsh=NexpbyYrFsh,ExPopCUnitsbyYrFsh=ExPopCUnitsbyYrFsh,
              Recruitment=Recruitment,TotBiomass=TotBiomass,AdultBiomass=AdultBiomass,
              RelBiomass=RelBiomass,ObsCatch=ObsCatch,PredCatch=PredCatch,ObsCPUE=ObsCPUE,PredCPUE=PredCPUE,
              Num_spp=nspp,
              YieldOption=YieldOption,
              Yield_res=yield_res,            
              YPR.effmult=YPR.effmult, YPR=YPR,
              TagGrps=TagGrps,RepRates=RepRates,nTagRetPds=nTagRetPds,TagRetPds=TagRetPds,
              ObsTagReturns=ObsTagReturns,PredTagReturns=PredTagReturns,MaxLiberty=MaxLiberty,ObsvPredbyLib=ObsvPredbyLib,
              MoveRates=MoveRates,
              lenLiks=lenLiks,wtLiks=wtLiks,
              TotalBiomass.nofish=TotalBiomass.nofish,AdultBiomass.nofish=AdultBiomass.nofish,ExplBiomass.nofish=ExplBiomass.nofish,
              PredCatch.interact=PredCatch.interact)
  } else {             # single-species yield data
    rep.obj <- list(nTimes=nTimes,Year1=Year1,nReg=nReg,nAges=nAges,reg.Spp=reg.Spp,nRecs.yr=nRecs.yr,
              nFisheries=nFisheries,nRlz.fsh=nRlz.fsh,Region.fsh=Region.fsh,Rlz.t.fsh=Rlz.t.fsh,mean.LatAge=mean.LatAge,
              sd.LatAge=sd.LatAge,mean.WatAge=mean.WatAge,
              MatAge=MatAge,SelAtAge=SelAtAge,qAtAge=qAtAge,qEdevAtAge=qEdevAtAge,FbyAgeYr=FbyAgeYr,FatYrAgeReg=FatYrAgeReg,
              NatYrAgeReg=NatYrAgeReg,NexpbyYrFsh=NexpbyYrFsh,ExPopCUnitsbyYrFsh=ExPopCUnitsbyYrFsh,
              Recruitment=Recruitment,TotBiomass=TotBiomass,AdultBiomass=AdultBiomass,
              RelBiomass=RelBiomass,ObsCatch=ObsCatch,PredCatch=PredCatch,ObsCPUE=ObsCPUE,PredCPUE=PredCPUE,
              Num_spp=nspp,
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
  }

  return(rep.obj)
  }

