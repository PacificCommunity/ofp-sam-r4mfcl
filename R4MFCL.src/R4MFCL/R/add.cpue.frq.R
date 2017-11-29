 add.cpue.frq <-
function(CPUE.file="P:/yft/2009/Data Preparation/CPUE/indices/yft_JPstd_R1.txt",
data,fishery=1,add.cv="T")
{
# Replaces the nominal effort in the original .FRQ file with stanadrdised effort
# based on the CPUE index
# SJH 2/6/2009
# originally named reconstruct.frq.ce but renamed 29/06/09
# flexible to handle either sort of frq file and you have the choice to include the
# cv (need to check with Adam to put in the write units)
# puts in -1 for effort first to make sure we account for missing values of CPUE (no JPN catch or effort)


# read in the CPUE file
cpue <- read.table(CPUE.file,sep=",",header=T,row.names=NULL)
# extract c/e data for that fishery (doesn't matter what version of Frq file)
xxx <- data[data[,4]==fishery,1:7]
# set effort to missing first
xxx[,6] <- -1

# Get a time unit comparable to the CPUE file to allow using match
dec.time <- xxx[,1]+((xxx[,2]-0.5)/12)
match.index <- match(cpue[,1],dec.time)
# divide catch by CPUE index to get standardised effort
xxx[match.index,6] <- xxx[match.index,5]/cpue[,3]
eff.w <- 1/(2*(cpue[,4]^2))
# add CV if desired
if(add.cv=="T") {xxx[match.index,7] <- eff.w }
return(xxx)
}
