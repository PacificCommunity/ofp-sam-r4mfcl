plot.samples.times.skj <- function(frq=basefrq,plotrepfile=baserep,fleets=fleetlabs)
{
# Adapting AL's script to be a function
# SJH 13/7/2011

##get length data from frq
jnk <- read.frq(frq)
line1 <- grep("# Datasets ", readLines(frq))
a <- count.fields(frq, skip=line1+3)
b <- c(0,cumsum(a))
dat <- scan(frq, skip=line1+3, comment.char="#")
## key params from the frq file
lfint <- jnk$dl$lfint
lfirst <- jnk$dl$lffirst
lint <- jnk$dl$lfwidth
nfish <- jnk$struct$nf
# Get final column of fisheries data
lstcol <- 8

#browser()
## length data
mat <- matrix(0, length(a), lfint+4)
for (i in 1:length(a)){
#no size data
  if(a[i] == lstcol){next}

# length data only
  if(a[i] == lstcol+lfint-1){    # Length of row is only LF or WF data only and LF_data 1st col is not -1: only LF data
      mat[i,1:4] <- dat[(b[i]+1):(b[i]+4)]
      mat[i,5:(lfint+4)] <- dat[(b[i]+(lstcol-1)):(b[i]+(lstcol-2)+lfint)]
  }
}
#no length data
sumlf <- apply(mat[,5:dim(mat)[2]], 1, sum)
mat <- mat[sumlf > 0,]
rm(sumlf)

lens <- as.data.frame(mat)
  names(lens) <- c("yr","mo","wk","flt",1:lfint)
  ns <- apply(lens[,-(1:7)],1,sum)
 # lens <- defactor(aggregate(ns,list(yr=lens$yr,flt=lens$flt),sum))
  lens <- aggregate(ns,list(yr=lens$yr,flt=lens$flt),sum)
  lens$yr <- as.numeric(as.character(lens$yr))
  lens$flt <- as.numeric(as.character(lens$flt))
  
  par(mar=c(4,8,2,2))
  
  plot(c(1970,2012),c(1,nfish+0.5),type='n',axes=F,xlab="",ylab="")

  ##-- scale by fleet ---
  mxsamp <- aggregate(lens$x,list(flt=lens$flt),max)
  #mxsamp$flt <- as.numeric(as.character(mxsamp.wt$flt))
  mxsamp$flt <- as.numeric(as.character(mxsamp$flt))
 #print(mxsamp.wt)
 #fleets with size data
  fltdata <- mxsamp$flt
  print(fltdata)
  mxsamp <- mxsamp[order(mxsamp$flt),]
  #maximum bar 1000 fish or greater
#  lens$x2 <- ifelse(lens$x > 4000, 4000, lens$x)     # original
#  mxsamp$x2 <- ifelse(mxsamp$x > 4000, 4000, mxsamp$x)   # original
  lens$x2 <- lens$x
  mxsamp$x2 <- mxsamp$x
  ##  plot with polygons
  ##    note: the row of NA's is there to separate the polygons
  x <- y <- matrix(NA,nrow=5,ncol=nrow(lens))
  x[1,] <- lens$yr-.7
  x[2,] <- x[1,]
  x[3,] <- lens$yr
  x[4,] <- x[3,]
  y[1,] <- lens$flt
  y[2,] <- lens$flt+.6*lens$x2/mxsamp$x2[match(lens$flt, fltdata)]
#   y[2,] <- lens$flt+.6*lens$x2/4000                 # original 
#  y[2,] <- ifelse(lens$flt > 1000, 0.6, y[2,])
  y[3,] <- y[2,]
  y[4,] <- y[1,]
#  polygon(x,y,border=NULL,col="LightSlateGrey")
  polygon(x,y,border=NULL,col="grey")

  line1 <- grep("# Number of fisheries", readLines(plotrepfile))
#number of fisheries
  nfish <- scan(plotrepfile, skip=line1, nlines=1, comment.char="#")
#number of times per fishery
  ftimes <- scan(plotrepfile, skip=line1+2, nlines=1, comment.char="#")
#times when fishery occurs
  ftimes2 <- scan(plotrepfile, skip=line1+6, nlines=nfish, comment.char="#")
  a <- rep(1:nfish, ftimes)
  a <- split(ftimes2, a)
  ftimes <- sapply(a,function(x)c(min(x),max(x)))
  nf <- nfish
  segments(x0=ftimes[1,],x1=ftimes[2,],y0=1:nf,y1=1:nf)
  print(paste("Max bar equivalent to",max(lens$x),"samples."))
  axis(1,cex.axis=.7)
#  labs <- scan("labels.tmp",what="",sep=":",quiet=T)
  labs <- fleets
  axis(2,at=1:nfish,labels=labs,las=1,cex.axis=.7)
  #axis(4,at=fltdata,labels=round(mxsamp$x,0),las=1,cex.axis=.7)
 # mtext("Max",line=1,side=4,at=25.6,las=1,cex=.7)
  box()
}