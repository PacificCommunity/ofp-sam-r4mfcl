plot.obspredcpue.betyft <- function(plotrep=baserep,frqfile=basefrq,
fisheries=c(1,2,4,7,10,12),parfile=basepar,fleetlabs=BET_fleets$fnames,XLIM=c(1950,2011))
{
###########################################################################
# Adam's code turned into a function
# 14/7/2011
## plot LL CPUE obs vs expected

## expected is the observed but with effort modified by the effort devs
#browser()
#time steps
tmp <- readLines(plotrep)

line1 <- grep("# Number of time periods", tmp)
tstep <- scan(plotrep, nlines=1, skip = line1)
#first year
line1 <- grep("# Year 1", tmp)
year1 <- scan(plotrep, nlines=1, skip = line1)
#number regions
line1 <- grep("# Number of regions", tmp)
nregion <- scan(plotrep, nlines=1, skip = line1)
#number of age classes
line1 <- grep("# Number of age classes", tmp)
nage <- scan(plotrep, nlines=1, skip = line1)
#number of fisheries
line1 <- grep("# Number of fisheries", tmp)
nfish <- scan(plotrep, nlines=1, skip = line1)
##fishery incidents
line1 <- grep("# Number of realizations", tmp)
fish1 <- scan(plotrep, nlines=1, skip = line1)
##fishery incidents times
line1 <- grep("# Time of each ", tmp)
fish2 <- scan(plotrep, nlines=nfish, skip = line1)


# effort deviation coefficients
line1 <- grep("# effort deviation coefficients", readLines(parfile))[1]
edevs <- readLines(parfile)[(line1+1):(line1+nfish)]


frq <- read.frq(frqfile)
#reg <- 1
par(mfrow=c(5,2), mar=c(2,4,1,1))

for(i in fisheries){
time <- frq$mat[,1][frq$mat[,4]==i] +  frq$mat[,2][frq$mat[,4]==i]/12
catch <- frq$mat[,5][frq$mat[,4]==i]
effort <- frq$mat[,6][frq$mat[,4]==i]
effort <- ifelse(effort == -1, NA, effort)
normeffort <- effort/mean(effort, na.rm = T)
cpue.obs <- catch/(normeffort)
#edevs2 <- as.numeric(unlist(strsplit(edevs[i], " "))[-1])
edevs2 <- as.numeric(unlist(strsplit(edevs[i], split="[[:blank:]]+"))[-1])   #from Nick
cpue.pred <- catch/(normeffort * exp(edevs2))

years <- year1 + seq(1,tstep, 1)/4 - 0.125
ylabel <- ifelse(i == fisheries[5], "CPUE/1000", "")

plot(time, cpue.obs/1000, ylim=c(0,1.15*max(cpue.obs/1000,cpue.pred/1000, na.rm=T)), type="n", lwd=2, xlim=XLIM, xlab="", ylab=ylabel, las=1)
points(time, cpue.obs/1000, pch=19, col=grey(0.5))
lines(time, cpue.pred/1000, lwd=2, col="black")
#mtext(side=3, line=0.7, fleetlabs[i], cex=0.7)
text(quantile(XLIM,0.5),  1.1*max(cpue.obs/1000,cpue.pred/1000, na.rm=T), fleetlabs[i])
}
legend("topright", legend=c("Observed", "Predicted"), pch=c(19,NA), lty=c(NA,1), lwd=2, bty="n", col=c(grey(0.5), "black"))
}