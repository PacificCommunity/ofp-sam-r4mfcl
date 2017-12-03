#' @export
plot_weight.temporal <- function(frq=basefrq, tmp.rep=read.rep(baserep), ini=baseini, fleetlabs=spp_fleets$fnames, YLIM=c(0,200),
                                Nrows=5, Ncols=3, annual=FALSE,fish.selblock=4)
{
  line1 <- grep("# Datasets ", readLines(frq))
  a <- count.fields(frq, skip=line1+1)
  b <- c(0,cumsum(a))
  dat <- scan(frq, skip=line1+1, comment.char="#")
  
  jnk <- read.frq(frq)

## as per frq file
  lfint <- jnk$dl$lfint
  lfirst <- jnk$dl$lffirst
  lint <- jnk$dl$lfwidth
  wtint <- jnk$dl$wfint
  wtfirst <- jnk$dl$wffirst
  wint <- jnk$dl$wfwidth
  nfish <- jnk$struct$nf

  mat <- matrix(0, length(a), wtint+4)
  for (i in 1:length(a)){
#no size data
    if(a[i] == 9){next()}
#only length data
    if(a[i] == lfint+8){next()}
#only weight data
    if(a[i] == wtint+8){
      mat[i,1:4] <- dat[(b[i]+1):(b[i]+4)]
      mat[i,5:(wtint+4)] <- dat[(b[i]+9):(b[i]+8+wtint)]
      }
#lf & weight data
    if(a[i] == lfint + wtint+7){
      mat[i,1:4] <- dat[(b[i]+1):(b[i]+4)]
      mat[i,5:(wtint+4)] <- dat[(b[i]+9+lfint-1):(b[i]+8+wtint+lfint-1)]
    }
  }

#no weight data
  sumwt <- apply(mat[,5:(wtint+4)], 1, sum)
  mat <- mat[sumwt > 0,]
  rm(sumwt)

  alb <- as.data.frame(mat)

##weight data
  lf <- aggregate.data.frame(alb[,5:(wtint+4)], list(alb[,4]), sum)
  lf2 <- apply(lf[,2:(wtint+1)], 1, sum)
  for(i in 1:dim(lf)[1]){
    lf[i,2:(wtint+1)] <- lf[i,2:(wtint+1)]/lf2[i]
  }
  lf0<-lf
##need to check this
  wint <- wtfirst + (1:wtint)*wint - wint
#cat("L53\n");browser()
#############################################################################
##compare mean weight predicted vs. observed
###observed
  lf <- aggregate.data.frame(alb[,5:(wtint+4)], list(alb[,1], alb[,4]), sum)
##summary stats
  stm <- matrix(NA, dim(lf)[1], 7)
  stm[,1] <- as.numeric(as.character(lf[,1]))
  stm[,2] <- as.numeric(as.character(lf[,2]))
#number of fish
  stm[,3] <- apply(lf[,-c(1,2)], 1, sum)
##calculate median weight
##need to check this
#  wint <- wtfirst+1:wtint-1
 wtint <- jnk$dl$wfint
  wtfirst <- jnk$dl$wffirst
  wint <- jnk$dl$wfwidth
  nfish <- jnk$struct$nf
  wint <- wtfirst + (1:wtint)*wint - wint
  for (j in 1:dim(lf)[1]){
    b <- 0
    for (i in 1:wtint){
      if(lf[j,i+2] > 0){
        b <- c(b, rep(wint[i],lf[j,i+2]))
      }
    }
    stm[j,4] <- median(b[-1])
    stm[j,5] <- quantile(b[-1], 0.25)
    stm[j,6] <- quantile(b[-1], 0.75)
    stm[j,7] <- mean(b[-1])
  }
#
####get predicted size composition
##as per plot.rep
#tmp.rep <- read.rep("P:/bigeye/2009/Model-runs/stepwise/run10/plot-11x.par.rep")
  tmp.jnk <- ini$LW
  lwa <- tmp.jnk[1]
  lwb <- tmp.jnk[2]
  
  nyears <- tmp.rep$nTimes
  year1 <- tmp.rep$Year1
  nages <- tmp.rep$nAges
  nfishery <- tmp.rep$nFisheries
##fishery region
  region <- tmp.rep$Region.fsh
  agemat <- tmp.rep$NatYrAgeReg
  selectivity <- tmp.rep$SelAtAge
##weight at age
  wt <- matrix(tmp.rep$mean.WatAge, 1, nages)
##length at age
  lth <- matrix(tmp.rep$mean.LatAge, 1, nages)
##sd at length
  lthsd <- matrix(tmp.rep$sd.LatAge, 1, nages)
  
##fisheries with weight data
  stm2 <- stm[stm[,3] > 30,]
  fishery <- unique(stm2[,2])
  if(annual){
    years <- year1+(1:nyears) - 1
  } else {
    years <- year1+(1:nyears)/4 - 0.25  
  }
#plot
  par(mfrow=c(Nrows,Ncols), mar=c(2,2,2,1), omi=c(0.2,0.3,0,0))
  for(k in 1:length(fishery)){
##get mean, median, q25,q75 for weight
    k.sel<-ifelse(k<=4,k,k+1)  # This is hard wired for swo with selex YT
    fish2 <- t(matrix(t(agemat[,,region[fishery[k]]]) * selectivity[k.sel,1:nages],
               nages, nyears))
    fish2 <- round(fish2/100,0)
    fishmat <- matrix(NA, nyears, 4)
    for(j in 1:nyears){
      b <- 0
      for (i in 1:nages){
        if(fish2[j,i] > 0){
          b <- c(b, rnorm(fish2[j,i], lth[i], lthsd[i]))
        }
      }
#convert length to weight
      b <- lwa*b^lwb
      fishmat[j,1] <-  median(b[-1], na.rm=T)
      fishmat[j,2] <- quantile(b[-1], 0.25, na.rm=T)
      fishmat[j,3] <- quantile(b[-1], 0.75, na.rm=T)
      fishmat[j,4] <- mean(b[-1], na.rm=T)
    }

    stm3 <- stm2[stm2[,2] == fishery[k],]

    if(is.null(nrow(stm3))) # if only one obs!
    {
      plot(1, 1, pch=16, xlim=range(years), ylim=YLIM, col="red", type="n", ylab="", xlab="",las=1)
      polygon(c(years, rev(years)), c(fishmat[,2],rev(fishmat[,3])), col="light grey", border=NA)
#plot median length
      lines(years, fishmat[,1], lwd=2, col="slate grey")
      points(stm3[1], stm3[4], pch=16, cex=0.75, col="red")
      segments(stm3[1],stm3[5],stm3[1],stm3[6], col="red", lty=1)
      mtext(side=3, fleetlabs[fishery[k]], line=0.2, cex=0.7)
    } else {
      plot(1,1, pch=16, xlim=range(years), ylim=YLIM, col="red", type="n", ylab="", xlab="",las=1)
      polygon(c(years, rev(years)), c(fishmat[,2],rev(fishmat[,3])), col="light grey", border=NA)
#plot median length
      lines(years, fishmat[,1], lwd=2, col="slate grey")
      points(stm3[,1], stm3[,4], pch=16, cex=0.75, col="red")
      segments(stm3[,1],stm3[,5],stm3[,1],stm3[,6], col="red", lty=1)
      mtext(side=3, fleetlabs[fishery[k]], line=0.2, cex=0.7)
    }


    if(k==(Nrows*2) | k==(Nrows*4) | k==length(fishery)){
      mtext(side=2, "Fish weight (kg)", outer=T, line=1)
    }
  }
}

