read.mult.frq <-
function(frq.file,frq.title="",ntop=0,fishdefs=NA) {
##===================================================================================
## by Simon Hoyle June 2008
## SDH 2/2/09 change for multi-region
## SDH 27/6/09 change for frq version 6
## SDH 13/7/09 Changed version 6 approach to work for any number of seasons, regions
## NMD 7/7/10 Changed to include Data flags - as is required for modifying for projections
##  PK June 2011 - fixed a warning    
## NMD 7/7/10 Changed to include Data flags - as is required for modifying for projections
##===================================================================================
## load the top -------------------
    op <- options(warn=-1)
    ff <- readLines(frq.file)
    options(op)
    ff <- gsub("\032","",ff)   # to remove mysterious ^z
    writeLines(ff,"erasethisfile")
    if (frq.title=="") frq.title <- ff[1]
    a <- scan("erasethisfile", nlines=200, comment.char="#")
    #file.remove("erasethisfile")
    if (ntop>0) top <- ff[2:ntop] else top <- "#"
    nreg <- a[1]; nf <- a[2]; gendiff <- a[3]; ntg <- a[4]; yr1 <- a[5];ta<- a[6];tb<- a[7];tc<- a[8];td<- a[9];te<- a[10]
 ##  tc = number of seasons in a year;    te = version number of frq file, 4 lacks the season-region-flags that are present in version 6
#  Check for single or multi-species format ta = number of species
    if(ta < 2){
      nspp <- 1
      multspp <- FALSE
    } else {
      nspp <- ta
      multspp <- TRUE
    } 
    if(multspp){
      first <- 11; last <- first+nspp-1
      taggrps_spp <- a[first:last]
    }
# If multi-species input regions in which each species occurs
    if(multspp){
      reg_spp <- vector(mode="numeric")
      for(i in 1:nspp){
        first <- last+1; last <- first+nreg-1
        reg_spp <- rbind(reg_spp,a[first:last])
      }
    }
# Input relative region size
    if(!multspp){
      first <- 11; last <- first+nreg-1
    } else {
      first <- last+1; last <- first+nreg-1
    }
    relreg <- a[first:last];
# Fisheries regions
    first <- last+1; last <- first+ nf - 1
    fishreg <- a[first:last]
# Incidence matrix
    if (nreg>1) {
      first <- last + 1; last <- first + (nreg * (nreg-1) / 2) -1
      incidence <- a[first:last]
    } else { incidence <- NA }
    if(multspp){
      incidence_spp <- vector(mode="numeric")
      for(i in 2:nspp){
        first <- last + 1; last <- first + (nreg * (nreg-1) / 2) -1
        incidence_spp <- rbind(incidence_spp,a[first:last])
        if(nspp == 2) incidence_spp <- as.vector(incidence_spp)
      }
    }
# Data flags
    first <- last+1; last <- first+ nf - 1
    ctype <- a[first:last]  # First of data flags
    dflags <- matrix(0,nrow=5,ncol=length(ctype))
    dflags[1,] <- ctype
    for(i in 2:5){
      first <- last+1; last <- last + nf
      dflags[i,] <- a[first:last]
    }
# Season-region flags
    first <- last+1
    if (te>=6) {
      last <- first + tc*nreg -1
      seas_reg_flags <- matrix(a[first:last],nrow=tc)
      first <- last+1
      if(multspp){
        first <- last+1
        seas_reg_flags_spp <- array(0,dim=c(dim(seas_reg_flags),(nspp-1)))
        for(i in 1:(nspp-1)){
          last <- first + tc*nreg -1
          seas_reg_flags_spp[,,i] <- matrix(a[first:last],nrow=tc)
          first <- last+1
        }
      }
    }
# 
    mpy <- a[first];
    first <- first+1; last <- first+ mpy-1
    mweeks <- a[first:last]
#
    first <- last+1; last <- first+8
    dl <- as.list(a[first:last])
    first <- last + 1
    if (te>=6) {
      last<-first+1
      age_inds <- a[first:last]; first <- last + 1
    }
    lasttop_old <- grep(dl[1],readLines("erasethisfile",n=200))
#
    if (te>=6) {
      lasttop <- grep(age_inds[1],
          readLines("erasethisfile",n=200)[(lasttop_old[1]+1):200])
    } else { 
      lasttop <- 0
    }
    lasttop <- lasttop_old + min(lasttop)
## Load up data -- ------------------------
    a <- count.fields("erasethisfile", skip=lasttop)
    b <- c(0,cumsum(a))
    dat <- scan("erasethisfile", skip=lasttop, comment.char="#")
    file.remove("erasethisfile")
    mat <- matrix(0, length(a), max(a))
    if(te>=6){
      if(nspp > 1){
        tmp_spp_ind <- paste("sppind_",c(1:nspp),sep="")
        tmp_spp_agg <- paste("sppagg_",c(1:nspp),sep="")
        tmp <- c("year","qtr","week","fishery",tmp_spp_ind,tmp_spp_agg,
          "catch","effort","se")
        colnames(mat) <- c(tmp,(length(tmp)+1):max(a))
        rm(tmp_spp_ind,tmp_spp_agg,tmp)
      } else {
        colnames(mat) <- c("year","qtr","week","fishery","catch","effort","se",8:max(a))
      }
    } else {
      colnames(mat) <- c("year","qtr","week","fishery","catch","effort",7:max(a))
    }
    for (i in 1:length(a)){
      mat[i,1:a[i]] <- dat[(b[i]+1):b[i+1]]
    }
## check the number of fisheries  -------------------
    true_nf <- length(unique(mat[,4]))
    if (nf!=true_nf) warning("nf = ",nf," and there are ",true_nf," fisheries")
## set up definitions  -------------------
    if(is.na(fishdefs)) fishdefs <- data.frame(cbind(fishery=sort(unique(mat[,4])),gear=NA,nations=NA,areas=NA))
## load into data frames  -------------------
    names(dl)=c("dsets","lfint","lffirst","lfwidth","lffactor","wfint","wffirst","wfwidth","wffactor")
    dl[["dsets"]] <- nrow(mat)
    fish <- data.frame(fishreg=fishreg,ctype=ctype,fishdefs)
    if(te>=6){ 
      if(nspp > 1){
        reg <- list(relreg=relreg, reg_spp=reg_spp, incidence=incidence,
        incidence_spp=incidence_spp, seas_reg_flags=seas_reg_flags,
        seas_reg_flags_spp=seas_reg_flags_spp) 
      } else {
        reg <- list(relreg=relreg,incidence=incidence,seas_reg_flags=seas_reg_flags) 
      }
    } else { 
      reg <- list(relreg=relreg,incidence=incidence)
    } 
    if(te>=6){ 
      if(multspp){
        struct <- list(nreg=nreg, nf=nf, gendiff=gendiff, ntg=ntg, yr1=yr1, ta=ta, tb=tb, 
                    tc=tc, td=td, te=te, age_inds=age_inds, taggrps_spp=taggrps_spp)
      } else {
        struct <- list(nreg=nreg, nf=nf, gendiff=gendiff, ntg=ntg, yr1=yr1, ta=ta, tb=tb,
                    tc=tc, td=td, te=te, age_inds=age_inds)
      }
    } else {
      struct <- list(nreg=nreg, nf=nf, gendiff=gendiff, ntg=ntg, yr1=yr1, ta=ta, tb=tb,
                  tc=tc, td=td, te=te)
    }
    struct[["nf"]] <- length(unique(mat[,4]))    
    rtn <- list(frq.title=frq.title,top=top,fish=fish,reg=reg,struct=struct,dflags=dflags,mpy=mpy,mweeks=mweeks,dl=dl,mat=mat)
    return(rtn)
  }

