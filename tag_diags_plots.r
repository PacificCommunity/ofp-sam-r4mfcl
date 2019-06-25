# Script that runs the tag diagnostics function and generates the plots
#
################################################################################
################################################################################
##                          Tagging diagnostics
#
# Do a single evaluation -switch 2 1 1 1 1 187 1
# to generate the temporary_tag_report

  source("C:/Nick/yft/2014/write-up/working/functions/get.tag.structure.R")

  setupdir <- c("C:/Nick/yft/2014/assessment/Setup")
  setwd(setupdir)
  rundir <- "../Model_runs/sens_run33_tagmix_2qtr"

  ppath <- function(x,y){paste(x,y,sep="/")}

  frqfile <- read.frq(ppath(rundir,"yft.frq"))
  tagfile <- read.tag(ppath(rundir,"yft.tag"))
  parfile <- read.par(ppath(rundir,"12.par"))
# Some basic summaries
#    - by fishery of recapture and year
rec.fsh.yr <- tapply(tagfile$rel.recov$n,list(tagfile$rel.recov$yr, tagfile$rel.recov$fsh),sum)
rec.fsh.yr[is.na(rec.fsh.yr[,])] <- 0

#    - by release program and fishery of recapture
tagfile$rel.recov$prg <- tagfile$tagprog[tagfile$rel.recov$grp]
rec.prg.fsh <- tapply(tagfile$rel.recov$n,list(tagfile$rel.recov$prg, tagfile$rel.recov$fsh),sum)
rec.prg.fsh[is.na(rec.prg.fsh[,])] <- 0

  tagz <- get.tag.structure(tagrepfile=ppath(rundir,"temporary_tag_report"),
                tagfile=tagfile,year1=1952)
  plotfol <- c("C:/Nick/yft/2014/write-up/working/tag_diagnostics/")
  flnm <- paste(plotfol,"tagz.obj",sep="")
  save(tagz,file=flnm)

# load(flnm)

  ## plot pred and obs recov by rel and recov region
  tgage <- parfile$afl[96] + 1
  windows(width=11, height=8)
  par(mfrow=c(9,9),mar=c(1.7,1.7,1.7,1.7))
  for(i in 1:9) for(j in 1:9) {
    tt <- with(tagz$auxdat,which(rreg==i & creg==j))
    rrp <- rro <- numeric(tgage)       # One more than the number age classes in the tagged population  aflag(96)
    for(k in tt) {
      rrp[tagz$auxdat$t2rec[k]] <- with(tagz$auxdat,rrp[t2rec[k]] + prec[k])
      rro[tagz$auxdat$t2rec[k]] <- with(tagz$auxdat,rro[t2rec[k]] + orec[k])
    }
    options(warn=-1)
#    print(rrp)
#    print(rro)

    if(sum(rrp,na.rm=TRUE) == 0 | sum(rro,na.rm=TRUE) == 0){
      plot(c(1:tgage),c(1:tgage),type='n',log="y",
           xlab="months at large",ylab="returns")
      title(paste("from",i,"to",j))
    } else {
      plot(rep(1:tgage,2),c(rrp,rro),type='n',log="y",
           xlab="months at large",ylab="returns")
      lines(1:tgage,rrp)
      points(1:tgage,rro)
      options(warn=0)
      title(paste("from",i,"to",j))
    }
  }
  prefx <- substring(rundir,15,nchar(rundir))
  flnm <- paste(plotfol,prefx,"_tagmv_region",sep="")
  savePlot(flnm,type="png")
  graphics.off()

  ## plot rec by time
  tt <- with(tagz$auxdat,aggregate(list(orec=orec,prec=prec),
                                   by=list(ctime=ctime),sum))
  sel <- with(tagz$auxdat,which(ctime==rtime))
  tt2 <- with(tagz$auxdat[sel,],aggregate(list(orec=orec,prec=prec),
                                   by=list(ctime=ctime),sum))
  par(mfrow=c(1,1))
  with(tt,prepplot(ctime,c(orec,prec)))
  with(tt,lines(ctime,prec,lwd=3))
  with(tt,points(ctime,orec,lwd=3))
  with(tt2,points(ctime,orec,col='red'))
  flnm <- paste(plotfol,prefx,"_tagfit_time_inclmix",sep="")
  savePlot(flnm,type="png")
  graphics.off()

  ##  plot tags not caught in mixing periods
  mixprd <- parfile$tfl[1,1] / 4 # time periods as proportion of 1 year
#  sel <- with(tagz$auxdat,which(ctime!=rtime))
# Modification follows to allow flexibility in length of the mixing period
  sel <- with(tagz$auxdat,which((ctime-rtime)>mixprd))
  tt <- with(tagz$auxdat[sel,],aggregate(list(orec=orec,prec=prec),by=list(ctime=ctime),sum))
  par()
  with(tt,prepplot(ctime,c(orec,prec)))
  with(tt,lines(ctime,prec,lwd=2))
  with(tt,points(ctime,orec))
  flnm <- paste(plotfol,prefx,"_tagfit_time_exclmix_1",sep="")
  savePlot(flnm,type="png")
  graphics.off()

  ## plot on log scale
  with(tt,plot(ctime,orec,ylim=c(.1,2200),log="y"))
  with(tt,lines(ctime,prec))
  flnm <- paste(plotfol,prefx,"_tagfit_time_exclmix_1_logscale",sep="")
  savePlot(flnm,type="png")
  graphics.off()

  ##  obs and pred caps by tag programme
# Correct the name of the CS programme
  tagz$auxdat$prog[tagz$auxdat$prog=="m CS"] <- "CS"
#  sel <- with(tagz$auxdat,which(ctime!=rtime))
  sel <- with(tagz$auxdat,which((ctime-rtime)>mixprd))
  tt <- with(tagz$auxdat[sel,],aggregate(list(orec=orec),
                                        by=list(ctime=ctime,pg=prog),
                                        sum))
  prec <- with(tagz$auxdat[sel,],aggregate(list(orec=orec,prec=prec),
                                           by=list(ctime=ctime),sum))
  pgn <- c(1:length(unique(tagz$auxdat$prog)))
  names(pgn) <- unique(tagz$auxdat$prog)
  par(mfrow=c(1,1),mar=c(6,6,3,3))
  with(tt,prepplot(ctime,orec,ylim=c(.1,2200),log="y"))
  with(tt,mtext("log(Recaptures)",side = 2,line=3,cex=1.5))
  with(tt,points(ctime,orec,col=pgn[pg]+1,pch=20,cex=1.5))
  with(prec,lines(ctime,prec,lwd=2))
  with(prec,points(ctime,orec,cex=1.5))
  ymax <- max(c(prec$orec,prec$prec))*2
  legend("topleft",legend=names(pgn),pch=c(20,20,20),cex=1.5,col= c(pgn)+1)
  flnm <- paste(plotfol,prefx,"_tagfit_prog_exmix_1",sep="")
  savePlot(flnm,type="png")
  graphics.off()

easylayout <- function(n) {
  ncolpl <- ceiling(sqrt(n))
  nrowpl <- ceiling(n/ncolpl)
  par(mfcol=c(nrowpl,ncolpl))
}

  ## do plots by fishery

  fisherycodes <- YFT_fleets$fnames
  tt <- with(tagz$auxdat,
             aggregate(list(prec=prec,orec=orec),
                       by=list(fry=fry,ctime=ctime),sum))
  easylayout(max(tt$fry))
  par(mar=c(3,2,2,1))
  options(warn=-1)
  for(i in sort(unique(tt$fry))) {
    with(tt[tt$fry%in%i,],prepplot(ctime,orec,ylim=c(.1,2200),log="y"))
    with(tt[tt$fry%in%i,],lines(ctime,prec))
    with(tt[tt$fry%in%i,],points(ctime,orec))
    title(main=paste("fleet ",i,", ",fisherycodes[i],sep=""),font.main=1)
  }
  options(warn=0)
  flnm <- paste(plotfol,prefx,"_tagfit_fshry_exmix_1",sep="")
  savePlot(flnm,type="png")
  graphics.off()

