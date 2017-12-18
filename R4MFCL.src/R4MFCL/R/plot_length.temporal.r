#' Plots of time trends of quantile of length composition data
#' @param frq outputs of read.frq
#' @param tmp.rep outputs of read.rep
#' @param fleetlabs vector of string names of fisheries
#' @param YLIM YLIM
#' @param Nrows number of rows of plot in one page
#' @param Ncols number of columns of plot in one page
#' @param annual LOGICAL if summarize annually, default=FALSE 
#' @importFrom graphics lines mtext polygon points segments plot
#' @export
plot_length.temporal <- function(frq, 
                                 tmp.rep, 
                                 fleetlabs, YLIM=c(0,200),
                                Nrows=5, Ncols=3, annual=FALSE)
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
  
  if(is.na(YLIM[1])){
    tmp <- lfirst + (lfint * lint) - lint
    tmp <- ceiling(tmp/100) * 100
    YLIM <- c(0,tmp)
    rm(tmp)
  }
##get length data from frq
  mat <- matrix(0, length(a), lfint+4)
  for (i in 1:length(a)){
#no size data
    if(a[i] == 9){next()}
# length data only
    if(a[i] == 9+lfint-1 | a[i] == 9+lfint+wtint-2){
      mat[i,1:4] <- dat[(b[i]+1):(b[i]+4)]
      mat[i,5:(lfint+4)] <- dat[(b[i]+8):(b[i]+7+lfint)]
    }
  }
#browser()
#no length data
  sumlf <- apply(mat[,5:(lfint+4)], 1, sum)
  mat <- mat[sumlf > 0,]
  rm(sumlf)
  
  alb <- as.data.frame(mat)
  lf <- aggregate.data.frame(alb[,5:(lfint+4)], list(alb[,4]), sum)
  lf2 <- apply(lf[,2:(lfint+1)], 1, sum)
  for(i in 1:dim(lf)[1]){
    lf[i,2:(lfint+1)] <- lf[i,2:(lfint+1)]/lf2[i]
  }
#need to check this
  lint <- lfirst + (1:lfint) * lint - lint

#############################################################################
##compare observed and predicted length
  lf <- aggregate.data.frame(alb[,5:(lfint+4)], list(alb[,1], alb[,4]), sum)
##summary stats
  stmlf <- matrix(NA, dim(lf)[1], 7)
  stmlf[,1] <- as.numeric(as.character(lf[,1]))
  stmlf[,2] <- as.numeric(as.character(lf[,2]))
#number of fish
  stmlf[,3] <- apply(lf[,-c(1,2)], 1, sum)
##calculate median weight
  for (j in 1:dim(lf)[1]){
    b <- 0
    for (i in 1:lfint){
      if(lf[j,i+2] > 0){
        b <- c(b, rep(lint[i],lf[j,i+2]))
      }
    }
    stmlf[j,4] <- median(b[-1])
    stmlf[j,5] <- quantile(b[-1], 0.25)
    stmlf[j,6] <- quantile(b[-1], 0.75)
    stmlf[j,7] <- mean(b[-1])
  }

##drop years with limited data
  stmlf2 <- stmlf[stmlf[,3] > 10,]

####get predicted size composition
##as per plot.rep
#tmp.rep <- read.rep("P:/bigeye/2009/Model-runs/stepwise/run10/plot-11x.par.rep")

 
  nyears <- tmp.rep$nTimes
  year1 <- tmp.rep$Year1
  nages <- tmp.rep$nAges
  nfishery <- tmp.rep$nFisheries
##fishery region
  region <- tmp.rep$Region.fsh
  agemat <- tmp.rep$NatYrAgeReg
  selectivity <- tmp.rep$SelAtAge
##length at age
  lth <- matrix(tmp.rep$mean.LatAge, 1, nages)
##sd at length
  lthsd <- matrix(tmp.rep$sd.LatAge, 1, nages)

##fisheries with lenght data
  fishery <- unique(stmlf2[,2])
  if(annual){
    years <- year1+(1:nyears) - 1
  } else {
    years <- year1+(1:nyears)/4 - 0.25  
  }
  
#plot
  par(mfrow=c(Nrows,Ncols), mar=c(2,2,2,1), omi=c(0.2,0.3,0,0))
#  cat("L106\n");browser()
  for(k in 1:length(fishery)){
    ##get mean, median, q25,q75 for length
    k.sel<-ifelse(k<=4,k,k+1)  # This is hard wired for swo with selex YT
    fish2 <- t(matrix(t(agemat[,,region[fishery[k]]]) * selectivity[k.sel,1:nages], nages,  nyears))
    fish2 <- round(fish2/100,0)
    fishmat <- matrix(NA, nyears, 4)
#    cat("L113, k=",k,"\n") #;browser()
    for(j in 1:nyears){
      b <- 0
      for (i in 1:nages){
        if(fish2[j,i] > 0){
          b <- c(b, rnorm(fish2[j,i], lth[i], lthsd[i]))
        }
      }
      fishmat[j,1] <-  median(b[-1], na.rm=T)
      fishmat[j,2] <- quantile(b[-1], 0.25, na.rm=T)
      fishmat[j,3] <- quantile(b[-1], 0.75, na.rm=T)
      fishmat[j,4] <- mean(b[-1], na.rm=T)
    }
    stm3 <- stmlf2[stmlf2[,2] == fishery[k],]
    #cat("L125\n");browser()
    if(is.null(nrow(stm3))) # if only one obs!
    {
      plot(stmlf2[1], stmlf2[4], pch=16, xlim=range(years), ylim=YLIM, col="red", type="n", ylab="", xlab="",las=1)
      polygon(c(years, rev(years)), c(fishmat[,2],rev(fishmat[,3])), col="light grey", border=NA)
      #plot median length
      lines(years, fishmat[,1], lwd=2, col="slate grey")
      points(stm3[1], stm3[4], pch=16, cex=0.75, col="red")
      segments(stm3[1],stm3[5],stm3[1],stm3[6], col="red", lty=1)
      mtext(side=3, fleetlabs[fishery[k]], line=0.2)
    } else {
      plot(stmlf2[,1], stmlf2[,4], pch=16, xlim=range(years), ylim=YLIM, col="red", type="n", ylab="", xlab="",las=1)
      polygon(c(years, rev(years)), c(fishmat[,2],rev(fishmat[,3])), col="light grey", border=NA)
      #plot median length
      lines(years, fishmat[,1], lwd=2, col="slate grey")
      points(stm3[,1], stm3[,4], pch=16, cex=0.75, col="red")
      segments(stm3[,1],stm3[,5],stm3[,1],stm3[,6], col="red", lty=1)
      mtext(side=3, fleetlabs[fishery[k]], line=0.2)
    }
  
    if(k==(Nrows*2) | k==(Nrows*4) | k==length(fishery)){
      mtext(side=2, "Fish length (cm)", outer=T, line=1)
    }
  }
}
