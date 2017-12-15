plot.length.resid.bet <- function(fitfile,fishery,nage=40,circscale=1,fleetlabs=BET_fleets$fnames)
{
##gets residuals from length.fit file
windows(12,8)
par(mfrow=c(1,1))
filename <- fitfile
#number of fisheries
nfish <- scan(filename, nlines=1, skip=2)
#records per fishery
nfish2 <- scan(filename, nlines=1, skip=3)
#length intervals
lendat <- scan(filename, nlines=1, skip=1)
lenint <- lendat[2] + 1:lendat[1] * lendat[3] - lendat[3]
#number of ages
#nage <- 40

###specify fishery of interest
#fishery <- 5

##time step for fishery matrix
tstep <- matrix(NA, nfish2[fishery], 3)
##set up obs. and pred. matrices
obslf <- matrix(NA, nfish2[fishery], lendat[1])
predlf <- matrix(NA, nfish2[fishery], lendat[1])

##read in
loadfit <- readLines(filename)
##locate first line
line1 <- grep(paste("# fishery", fishery), loadfit)
#deal with multiples
line1 <- ifelse(fishery == 1, line1[1], line1)
line1 <- ifelse(fishery == 2, line1[1], line1)

count <- line1
count2 <- nage + 7
#browser()
for(i in 1:nfish2[fishery]){
        tstep[i,] <- as.numeric(unlist(strsplit(loadfit[count+1]," ")))
        obslf[i,] <- as.numeric(unlist(strsplit(loadfit[count+4]," "))[-1])
        predlf[i,] <- as.numeric(unlist(strsplit(loadfit[count+5]," "))[-1])
        count <- count + count2
}


#residuals: rows = time; col = length interval
resid <- obslf - predlf
resid2 <- abs(resid)
maxres <- max(resid2)
#positive resids
resid3 <- ifelse(resid > 0, resid, NA)
#negative resids
resid4 <- abs(ifelse(resid < 0, resid, NA))

tstep2 <- tstep[,1] + tstep[,2]/12

#diameter of circle is proportional to residual
plot(1,1, xlim=c(1952, 2010), ylim=range(lenint), type="n", ylab="Length cm", xlab="",las=1)
for(i in 1:nfish2[fishery]){
#positive residuals
symbols(rep(tstep2[i], lendat[1]), lenint, circles = circscale * sqrt(resid3[i,]/maxres), fg="blue", add=T, inches=FALSE)
#negative residuals
symbols(rep(tstep2[i], lendat[1]), lenint, circles = circscale * sqrt(resid4[i,]/maxres), fg="red", add=T, inches=FALSE)
}


mtext(side=3, line = 0.5, fleetlabs[fishery])
}
