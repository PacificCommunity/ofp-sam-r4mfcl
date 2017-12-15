 reconstruct.frq.ce <-
function(CPUE.file="X:/yft/2009/Data Preparation/CPUE/indices/yft_JPstd_R1.txt",
data=out.data,fishery=1)
{
# Replaces the nominal effort in the original .FRQ file with stanadrdised effort
# based on the CPUE index
# SJH 2/6/2009
# read in the CPUE file
cpue <- read.table(CPUE.file,sep=",",header=T,row.names=NULL)
# extract c/e data for that fishery
xxx <- data[data[,4]==fishery,1:6]

# Get a time unit comparable to the CPUE file to allow using match
dec.time <- xxx[,1]+((xxx[,2]-0.5)/12)
match.index <- match(cpue[,1],dec.time)
# divide catch by CPUE index to get standardised effort
xxx[match.index,6] <- round(xxx[match.index,5]/cpue[,3],0)
qtimes <- seq(0.125,0.875,by=0.25)

      #replace effort and get catch (if CPUE exists)
      for(j in 1:length(qtimes))
      {
      # Get time periods
      dud <- 2008:2007 + qtimes[j]
      # replace effort with 2007 estimate first
      xxx[dec.time==dud[1],6] <- xxx[dec.time==dud[2],6]
      # check if CPUE exists
      dud1 <- match(dud[1],cpue[,1],nomatch=NA)
      # if no CPUE then set catch to -1 else catch=effort*cpue
      xxx[dec.time==dud[1],5] <- ifelse(is.na(dud1),-1,round(xxx[dec.time==dud[1],6]*cpue[dud1,3],0))
      }

return(xxx)
}
