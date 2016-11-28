##################################
#
# calc.vulnB
#
# calculate vulnerable biomass from the .rep file outputs
#
# 01/10/2015
#
##################################



calc.vulnB <- function(rep, wgt.units='tonnes'){
  
  N   <- rep$NatYrAgeReg
  sel <- rep$SelAtAge
  reg <- rep$Region.fsh
  
  wt <- switch(wgt.units, tonnes=rep$mean.WatAge/1000,
                          kg    =rep$mean.WatAge)
  
  #dimension checks - all should have same number of ages
  if(dim(N)[2]!=length(wt) | dim(sel)[2]!=length(wt))
    stop("Error - age dimensions of N at age Wt at age and Sel at age not consistent")
  
  vulnB <- NULL
  for(fsh in 1:length(rep$Region.fsh)){
    vulnB <- cbind(vulnB, apply(sweep(N[,,reg[fsh]],2,sel[fsh,]*wt,'*'),1,sum))
  }
  return(vulnB)
}


#calc.vulnB(read.rep("Q:/skj/2014/assessment/RefCase/plot-11.par.rep"), wgt.units="tonnes")
