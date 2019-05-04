plot.size.data.available <- function(frq=basefrq, plotrepfile=baserep, fleets=BET_fleets$fnames, weights=TRUE, Ylab.space=10, plotlims=c(1950,2014))
{
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
    wtint <- jnk$dl$wfint
    wtfirst <- jnk$dl$wffirst
    wint <- jnk$dl$wfwidth
    nfish <- jnk$struct$nf
# Get final column of fisheries data
    lstcol <- ifelse(jnk$struct$te > 4,9,8)

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

    #if(weights){
    # weight data only
      if(a[i] == lstcol+wtint-1){next}

    #lf & weight data
      if(a[i] == (lfint + wtint+(lstcol-2))){                     # Length and weight data in columns
        mat[i,1:4] <- dat[(b[i]+1):(b[i]+4)]
        mat[i,5:(lfint+4)] <- dat[(b[i]+(lstcol-1)):(b[i]+(lstcol-2)+lfint)]
      }
    }#}
    #no length data
    sumlf <- apply(mat[,5:dim(mat)[2]], 1, sum)
    mat <- mat[sumlf > 0,]
    rm(sumlf)

    #browser()
    lens <- as.data.frame(mat)
      names(lens) <- c("yr","mo","wk","flt",1:lfint)
      ns <- apply(lens[,-(1:7)],1,sum)
     # lens <- defactor(aggregate(ns,list(yr=lens$yr,flt=lens$flt),sum))
      lens <- aggregate(ns,list(yr=lens$yr,flt=lens$flt),sum)
      lens$yr <- as.numeric(as.character(lens$yr))
      lens$flt <- as.numeric(as.character(lens$flt))

    if(weights){
    ## get weight data
    mat <- matrix(0, length(a), wtint+4)
    for (i in 1:length(a)){
    #no size data
      if(a[i] == lstcol){next()}
    #only length data
      if(a[i] == lfint+(lstcol-1) & dat[(b[i]+(lstcol-1))] != -1){next()}
     #only weight data
      if(a[i] == wtint+(lstcol-1) & dat[(b[i]+(lstcol))] != -1){
       mat[i,1:4] <- dat[(b[i]+1):(b[i]+4)]
       mat[i,5:(wtint+4)] <- dat[(b[i]+lstcol):(b[i]+(lstcol-1)+wtint)]
       }
     #lf & weight data
      if(a[i] == lfint + wtint+(lstcol-2)){
        mat[i,1:4] <- dat[(b[i]+1):(b[i]+4)]
        mat[i,5:(wtint+4)] <- dat[(b[i]+lstcol+lfint-1):(b[i]+(lstcol-1)+wtint+lfint-1)]
      }
    }}

     #browser()

      #no wt data
      sumwt <- apply(mat[,5:(wtint+4)], 1, sum)
      mat <- mat[sumwt > 0,]
      rm(sumwt)

    if(weights){
    wts <- as.data.frame(mat)
        names(wts) <- c("yr","mo","wk","flt",1:wtint)
        ns <- apply(wts[,-(1:7)],1,sum)
       # wts <- defactor(aggregate(ns,list(yr=wts$yr,flt=wts$flt),sum))
        wts <- aggregate(ns,list(yr=wts$yr,flt=wts$flt),sum)
        wts$yr <- as.numeric(as.character(wts$yr))
      wts$flt <- as.numeric(as.character(wts$flt))
    }
      opar <- par(mar=c(3,Ylab.space,1,0.5))
      on.exit(par(opar))

      plot(plotlims, c(1,nfish+0.5),type='n',axes=F,xlab="",ylab="")

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
     lens$x2 <- ifelse(lens$x > 1000, 1000, lens$x)     # original
     mxsamp$x2 <- ifelse(mxsamp$x > 1000, 1000, mxsamp$x)   # original
      ## lens$x2 <- lens$x
      ## mxsamp$x2 <- mxsamp$x
      ##  plot with polygons
      ##    note: the row of NA's is there to separate the polygons
      x <- y <- matrix(NA,nrow=5,ncol=nrow(lens))
      x[1,] <- lens$yr-.4
      x[2,] <- x[1,]
      x[3,] <- lens$yr
      x[4,] <- x[3,]
      y[1,] <- lens$flt
      #y[2,] <- lens$flt+.6*lens$x2/mxsamp$x2[match(lens$flt, fltdata)]
      y[2,] <- lens$flt+.6*lens$x2/1000                 # original
      y[2,] <- ifelse(lens$flt > 1000, 0.6, y[2,])
      y[3,] <- y[2,]
      y[4,] <- y[1,]
    #  polygon(x,y,border=NULL,col="LightSlateGrey")
      polygon(x,y,border=NULL,col="grey")


      ##-- scale by fleet ---
      if(weights) {
        mxsamp.wt <- aggregate(wts$x,list(flt=wts$flt),max)
      mxsamp.wt$flt <- as.numeric(as.character(mxsamp.wt$flt))
     #print(mxsamp.wt)
     #fleets with size data
      fltdata <- mxsamp.wt$flt
      print(fltdata)
      mxsamp.wt <- mxsamp.wt[order(mxsamp.wt$flt),]
      #maximum bar 1000 fish or greater
    #  wts$x2 <- ifelse(wts$x > 4000, 4000, wts$x)                                    # original
    #  mxsamp.wt$x2 <- ifelse(mxsamp.wt$x > 4000, 4000, mxsamp.wt$x)                  # original
      wts$x2 <-  wts$x
      mxsamp.wt$x2 <-  mxsamp.wt$x
      ##  plot with polygons
      ##    note: the row of NA's is there to separate the polygons
      x <- y <- matrix(NA,nrow=5,ncol=nrow(wts))
      x[1,] <- wts$yr
      x[2,] <- x[1,]
      x[3,] <- wts$yr+.4
      x[4,] <- x[3,]
      y[1,] <- wts$flt
      y[2,] <- wts$flt+.6*wts$x2/mxsamp.wt$x2[match(wts$flt, fltdata)]
    #  y[2,] <- wts$flt+.6*wts$x2/4000                          #original
    #  y[2,] <- ifelse(wts$flt > 1000, 0.6, y[2,])
      y[3,] <- y[2,]
      y[4,] <- y[1,]
      polygon(x,y,border=NULL,col="red")
    }
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
