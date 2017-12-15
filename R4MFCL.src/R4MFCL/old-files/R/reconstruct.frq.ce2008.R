 reconstruct.frq.ce2008 <-
function(data=out.data,fishery=1)
{
# Replaces the 2008 effort with 2007 and set catch to -1
# SJH 2/6/2009
# extract c/e data for that fishery
xxx <- data[data[,4]==fishery,1:6]

# Get a time unit comparable to the CPUE file to allow using match
# hangover from previous function, but okay
dec.time <- xxx[,1]+((xxx[,2]-0.5)/12)
qtimes <- seq(0.125,0.875,by=0.25)

      #replace effort and set catch to -1
      for(j in 1:length(qtimes))
      {
      # Get time periods
      dud <- 2008:2007 + qtimes[j]
      # replace effort with 2007 estimate first
      xxx[dec.time==dud[1],6] <- xxx[dec.time==dud[2],6]
      xxx[dec.time==dud[1],5] <- -1
      }
return(xxx)
}
