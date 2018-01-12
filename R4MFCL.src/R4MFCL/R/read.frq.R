#' Read frq file
#'
#' @param frq.file file name of frq file
#' @param frq.title frq.title
#' @param ntop ntop
#' @param fishdefs fishdefs
#' @importFrom utils count.fields
#' @export
read.frq <-
function(frq.file,frq.title="",ntop=0,fishdefs=NA,remove0frq=TRUE) {
##===================================================================================
## by Simon Hoyle June 2008
## SDH 2/2/09 change for multi-region
## SDH 27/6/09 change for frq version 6
## SDH 13/7/09 Changed version 6 approach to work for any number of seasons, regions
## NMD 7/7/10 Changed to include Data flags - as is required for modifying for projections
##  PK June 2011 - fixed a warning
## YT  23/02/17  Adapted to frq versions 8 and 9
##===================================================================================
## load the top -------------------
    op <- options(warn=-1)
    ff <- readLines(frq.file)
    options(op)
    ff <- gsub("\032","",ff)   # to remove mysterious ^z
    writeLines(ff,"erasethisfile")
    if (frq.title=="") frq.title <- ff[1]
    a <- scan("erasethisfile", nlines=200, comment.char="#")
#    file.remove("erasethisfile")
    if (ntop>0) top <- ff[2:ntop] else top <- "#"
    nreg <- a[1]; nf <- a[2]; gendiff <- a[3]; ntg <- a[4]; yr1 <- a[5];nsp<-ta<- a[6];tb<- a[7];tc<- a[8];td<- a[9];version<-te<- a[10]
 ##  tc = number of seasons in a year;    te = version number of frq file, 4 lacks the season-region-flags that are present in version 6
 ##  nsp =ta = number of species (or sex)
    if(te>=8){ # Multi species/sex model
      NtagGrpBySp<-a[10+1:nsp]
      first<-10+nsp+1;last<-10+nsp+nsp*nreg
      spReg<-matrix(data=a[first:last],nrow=nsp,ncol=nreg)
      first<-last+1
      last <- first+nreg-1
    }else{
      first <- 11
      last <- first+nreg-1
      nsp<-1
      spReg<-NULL
    }
    relreg <- a[first:last];
    first <- last+1; last <- first+ nf - 1
    fishreg <- a[first:last]
  #  browser()
    if (nreg>1) {
      first <- last + 1; last <- first + nreg * (nreg-1) / 2*nsp -1
      incidence <- a[first:last]
    } else { incidence <- NA }
    first <- last+1; last <- first+ nf - 1
    ctype <- a[first:last]  # First of data flags
    dflags <- matrix(0,nrow=5,ncol=length(ctype))
    dflags[1,] <- ctype
    for(i in 2:5){
      first <- last+1; last <- last + nf
      dflags[i,] <- a[first:last]
    }
    first <- last+1
    if (version>=6) {
      last <- first + tc*nreg*nsp -1
      seas_reg_flags <- matrix(a[first:last],nrow=tc*nsp)
      first <- last+1
    }
    mpy <- a[first];
    first <- first+1; last <- first+ mpy-1
    mweeks <- a[first:last]
    first <- last+1; last <- first+8
    dl <- as.list(a[first:last])
    a.old<-a ## For debugging
    first <- last + 1
    if (version>=6) {
      last<-first+1
      age_inds <- a[first:last]; first <- last + 1
    }
      ## how long is the top? -------------------
    #lasttop_old <- grep(dl[1],readLines(frq.file,n=200))
  #  browser()
    lasttop_old <- grep(dl[1],readLines("erasethisfile",n=200))
    if (version>=6) {
 #     lasttop <- grep(age_inds[1],
          #readLines(frq.file,n=200)[(lasttop_old[1]+1):200])
 #         readLines("erasethisfile",n=200)[(lasttop_old[1]+1):200])
     lasttop<-grep(a[first],readLines("erasethisfile",n=200)[(lasttop_old[1]+1):200])[[1]]-1
     ## YT 2017-04-17 to allow frq file having addtional 2 columns fo age data located in the same line of header of frq data
    } else { lasttop <- 0 }
    lasttop <- lasttop_old + min(lasttop)
       ## Load up data -- ------------------------
    #a <- count.fields(frq.file, skip=lasttop)
    a <- count.fields("erasethisfile", skip=lasttop)
    b <- c(0,cumsum(a))
    #dat <- scan(frq.file, skip=lasttop, comment.char="#")
    dat <- scan("erasethisfile", skip=lasttop, comment.char="#")
    file.remove("erasethisfile")
    mat <- matrix(0, length(a), max(a))
    if(version>=6 & version <8){
      colnames(mat) <- c("year","qtr","week","fishery","catch","effort","se",8:max(a))
    } else if(version ==8 ){
      colnames(mat) <- c("year","qtr","week","fishery",paste0("Sex",1:nsp),paste0("AggData",1:nsp),"catch","effort","se",(8+nsp*2):max(a))
    } else if(version ==9 ){
      colnames(mat) <- c("year","qtr","week","fishery",paste0("Sex",1:nsp),paste0("AggCatch",1:nsp),paste0("AggLF",1:nsp),paste0("AggWF",1:nsp),"catch","effort","se",(8+nsp*4):max(a))
    } else {
      colnames(mat) <- c("year","qtr","week","fishery","catch","effort",7:max(a))
    }
    for (i in 1:length(a)){
      mat[i,1:a[i]] <- dat[(b[i]+1):b[i+1]]
    }
       ## check the number of fisheries  -------------------
    true_nf <- length(unique(mat[,4]))
       ##browser()
    if (nf!=true_nf) warning("nf = ",nf," and there are ",true_nf," fisheries")
       ## set up definitions  -------------------
    if(is.na(fishdefs)) fishdefs <- data.frame(cbind(fishery=sort(unique(mat[,4])),gear=NA,nations=NA,areas=NA))
       ## load into data frames  -------------------
    names(dl)=c("dsets","lfint","lffirst","lfwidth","lffactor","wfint","wffirst","wfwidth","wffactor")
    #if(dl[["dsets"]] != nrow(mat)) warning("Warning: dl$dsets is not equal to the number of catch records")
    dl[["dsets"]] <- nrow(mat)
    fish <- data.frame(fishreg=fishreg,ctype=ctype,fishdefs)
    if(version>=6) { reg <- list(relreg=relreg,incidence=incidence,seas_reg_flags=seas_reg_flags) } else
    { reg <- list(relreg=relreg,incidence=incidence) }
    if(version>=6) { struct <- list(nreg=nreg,nf=nf,gendiff=gendiff,ntg=ntg,yr1=yr1,ta=ta,tb=tb,tc=tc,td=td,te=te,age_inds=age_inds,nsp=nsp) } else {
      struct <- list(nreg=nreg,nf=nf,gendiff=gendiff,ntg=ntg,yr1=yr1,ta=ta,tb=tb,tc=tc,td=td,te=te) }
    struct[["nf"]] <- length(unique(mat[,4]))
    rtn <- list(frq.title=frq.title,top=top,fish=fish,reg=reg,struct=struct,dflags=dflags,mpy=mpy,mweeks=mweeks,dl=dl,mat=mat,version=te)
    return(rtn)
  }

