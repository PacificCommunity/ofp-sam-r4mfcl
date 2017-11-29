#' @importFrom utils count.fields
 read_nmd.frq <-
function(frq.file,frq.title="",ntop=0,fishdefs=NA) {
  # by Simon Hoyle June 2008
  # SDH 2/2/09 change for multi-region
  # SDH 27/6/09 change for frq version 6
  # SDH 13/7/09 Changed version 6 approach to work for any number of seasons, regions
  # NMD 7/7/10 Changed to include Data flags - as is required for modifying for projections
  # load the top -------------------
  if (frq.title=="") frq.title <- readLines(frq.file,n=1)
  a <- scan(frq.file, nlines=200, comment.char="#")
  top <- readLines(frq.file,n=ntop) ; if (ntop>0) top <- top[2:ntop]
  nreg <- a[1]; nf <- a[2]; gendiff <- a[3]; ntg <- a[4]; yr1 <- a[5];ta<- a[6];tb<- a[7];tc<- a[8];td<- a[9];te<- a[10]
#  tc = number of seasons in a year;    te = version number of frq file, 4 lacks the season-region-flags that are present in version 6
  first <- 11; last <- first+nreg-1
  relreg <- a[first:last];
  first <- last+1; last <- first+ nf - 1
  fishreg <- a[first:last]
  if (nreg>1) {
    first <- last + 1; last <- first + (nreg * (nreg-1) / 2) -1
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
  if (te>=6) {
    last <- first + tc*nreg -1
    seas_reg_flags <- matrix(a[first:last],nrow=tc)
    first <- last+1
    }
  mpy <- a[first];
  first <- first+1; last <- first+ mpy-1
  mweeks <- a[first:last]
  first <- last+1; last <- first+8
  dl <- as.list(a[first:last])
  first <- last + 1
  if (te>=6) { last<-first+1 ; age_inds <- a[first:last]; first <- last + 1 }
    # how long is the top? -------------------
  lasttop_old <- grep(dl[1],readLines(frq.file,n=200))
  if (te>=6) lasttop <- grep(age_inds[1],readLines(frq.file,n=200)[(lasttop_old+1):200]) else lasttop <- 0
  lasttop <- lasttop_old + min(lasttop)
  # Load up data -- ------------------------
  a <- count.fields(frq.file, skip=lasttop)
  b <- c(0,cumsum(a))
  dat <- scan(frq.file, skip=lasttop, comment.char="#")
  mat <- matrix(0, length(a), max(a))
  for (i in 1:length(a)){ mat[i,1:a[i]] <- dat[(b[i]+1):b[i+1]] }
  # check the number of fisheries  -------------------
  true_nf <- length(unique(mat[,4]))
  #browser()
  if (nf!=true_nf) warning("nf = ",nf," and there are ",true_nf," fisheries")
  # set up definitions  -------------------
  if(is.na(fishdefs)) fishdefs <- data.frame(cbind(fishery=sort(unique(mat[,4])),gear=NA,nations=NA,areas=NA))
  # load into data frames  -------------------
  names(dl)=c("dsets","lfint","lffirst","lfwidth","lffactor","wfint","wffirst","wfwidth","wffactor")
  fish <- data.frame(fishreg=fishreg,ctype=ctype,fishdefs)
  if(te>=6) { reg <- list(relreg=relreg,incidence=incidence,seas_reg_flags=seas_reg_flags) } else
            { reg <- list(relreg=relreg,incidence=incidence) } 
  if(te>=6) { struct <- list(nreg=nreg,nf=nf,gendiff=gendiff,ntg=ntg,yr1=yr1,ta=ta,tb=tb,tc=tc,td=td,te=te,age_inds=age_inds) } else {
    struct <- list(nreg=nreg,nf=nf,gendiff=gendiff,ntg=ntg,yr1=yr1,ta=ta,tb=tb,tc=tc,td=td,te=te) }
  rtn <- list(frq.title=frq.title,top=top,fish=fish,reg=reg,struct=struct,dflags=dflags,mpy=mpy,mweeks=mweeks,dl=dl,mat=mat)
  return(rtn)
}
