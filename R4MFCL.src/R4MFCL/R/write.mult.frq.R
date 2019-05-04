write.mult.frq <- function(frqfile,frq.obj) {
##============================================================================
## by Simon Hoyle June 2008
## fishery information is in $fish[] and $mat[,4]
## SDH 27/6/09 change for frq version 6
##  enhanced  PK 13/06/2011
## NMD 02/12/2013 - extended for multi-spp/sex format
##============================================================================
    x<-frq.obj
    m <- x$mat
    x$struct$nf <- length(unique(m[,4]))
    ttl <- x$frq.title
    defs <- paste(c("# Definition of fisheries","#",
      "# Fishery   Gear   Nation          Region    Season, "),collapse="\n")
  for (i in 1:dim(x$fish)[1]) defs <- rbind(defs,paste(c("#",x$fish[i,3:6]),collapse="       "))
  defs <-rbind(defs,"#")
  t1 <- rbind(c("# Number of   Number of   Use generic   Number of     Year1  Number ",
                "#  Region     Fisheries    diffusion    tag groups           species"))
#
# Determine number of species
  if(x$struct$ta > 1){
    multspp <- TRUE
    nspp <- x$struct$ta
  } else {
    multspp <- FALSE
    nspp <- 1
  }
  if(multspp){
    t1_spp <- rbind(c("# number of tag groups for",
      "# each species", paste(c("# ",c(1:nspp)),collapse="  ")))
    t2_spp <- list()
    for(i in 1:nspp){
      t2_spp[[i]] <- rbind(c(paste("# regions in which species ",i," occurs",sep=""),
                             paste(c("# ",c(1:x$struct$nreg)),collapse="  ")))
    }
  }
  t2 <- "# Relative Region Size"
  t3 <- rbind(c("#","# Region in which each fishery is located"))
  if(multspp){
    t4a <- character()
    for(i in 1:nspp){
      t4a <- rbind(t4a,c("#",paste("# Incidence matrix for species ",i," (",x$struct$nreg,
        " regions)",sep="")))
    }
  } else {
    t4a <- rbind(c("#","# Incidence matrix"))
  }
  nreg <- x$struct$nreg
  l_inc <- ""
  if(nreg > 1) {
    l_inc <- rep(NA,nreg-1)
    first <- 1; last <- nreg - 1
    for(i in 1:(nreg-1))
    {
      l_inc[i] <- paste(x$reg$incidence[first:last],collapse=" ")
        first <- last+1 ; last <- first + nreg-(i+2)
    }
  }
  if(multspp){
    if(nspp == 2){                               
      if(!is.null(dim(x$reg$incidence_spp)[[1]])){
        if(length(dim(x$reg$incidence_spp)) != 1){
          print("ERROR: number of species does not match incident matrix")
          stop()
        }
      }
      if(length(dim(x$reg$incidence_spp)) != 1){
        if(!is.null(dim(x$reg$incidence_spp)[[1]])){
          print("ERROR: number of species does not match incident matrix")
          stop()
        }
      }
      l_inc_spp <- ""
      if(nreg > 1) {
        l_inc_spp <- rep(NA,nreg-1)
        first <- 1; last <- nreg - 1
        for(i in 1:(nreg-1))
        {
          l_inc_spp[i] <- paste(x$reg$incidence_spp[first:last],collapse=" ")
            first <- last+1 ; last <- first + nreg-(i+2)
        }
      }
    } else {
      l_inc_spp <- list()
      for(ii in 1:(nspp-1)){         # rows start at species 2
        l_inc_spp[[ii]] <- ""
        if(nreg > 1) {
          l_inc_spp[[ii]] <- rep(NA,nreg-1)
          first <- 1; last <- nreg - 1
          for(i in 1:(nreg-1))
          {
            l_inc_spp[[ii]][i] <- paste(x$reg$incidence_spp[ii,first:last],collapse=" ")
              first <- last+1 ; last <- first + nreg-(i+2)
          }
        }
      }
    }
  }
#
  t4b <- rbind(c("#","# Data flags (for records 1, 0=catch in number; 1=catch in weight)"))
  if(multspp){
    t4c <- vector(mode="character")
    for(i in 1:nspp){
      t4c[i] <- rbind(paste("# Season-region flags for species ",i,sep=""))
    }
  } else {
    t4c <- "# Season-region flags"
  }
  t5 <- "# Number of movements per year"
  t6 <- "# Weeks in which movement occurs"
  t7 <- rbind(c("# fishery data","#","#","# Datasets / LFIntervals  LFFirst  LFWidth  LFFactor / WFIntervals  WFFirst  WFWidth"))
  t8 <- "# age_nage   age_age1"
#
  line1 <- paste(c("    ",paste(x$struct[1:5],collapse="           "),"", x$struct[6:10]),collapse=" ")
  if(multspp){
    line1a <- paste(x$struct$taggrps_spp,collapse=" ")
    line1b <- vector(mode="character")
    for(i in 1:nspp){
      line1b <- rbind(line1b,unlist(t2_spp[[i]])[,1])
      line1b <- rbind(line1b,unlist(t2_spp[[i]])[,2])
      line1b <- rbind(line1b,paste(x$reg$reg_spp[i,],collapse=" "))
    }      
  }
  line3 <- paste(c(" ",t(x$fish$fishreg)),collapse=" ")

  if(multspp){     # Incidence matrix
    line3a <- vector(mode="character")
    for(i in 1:nspp){
      if(i == 1){
        line3a <- paste(line3a,paste(t4a[i,],collapse="\n"),"\n",
          paste(l_inc,collapse="\n"),"\n",sep="")
      } else {
        if(nspp < 3){
          line3a <- paste(line3a,paste(t4a[i,],collapse="\n"),"\n",
            paste(l_inc_spp,collapse="\n"),"\n",sep="")
        } else {
          line3a <- paste(line3a,paste(t4a[i,],collapse="\n"),"\n",
            paste(unlist(l_inc_spp[[i]]),collapse="\n"),"\n",sep="")        
        }
      }
    }
    line3a <- paste(line3a,"#",sep="")
  }
  line4 <- vector(mode="character")
  for(i in 1:dim(x$dflags)[1]){
    if(i < dim(x$dflags)[1]){
      line4 <- paste(line4,paste(as.character(x$dflags[i,]),collapse=" "),"\n",sep="")
    } else {
      line4 <- paste(line4,paste(as.character(x$dflags[i,]),collapse=" "),sep="")
    }
  }
  if(x$struct$te>=6) {
    if(multspp){
      line4 <- rbind(line4,t4c[1])
      for (ssn in 1:x$struct$tc)
      {
        line4 <- rbind(line4,paste(x$reg$seas_reg_flags[ssn,],collapse=" "))
      }
      for(i in 1:(nspp-1)){      #x$reg$seas_reg_flags_spp 3rd dimension starts at species 2
        line4 <- rbind(line4,t4c[i+1])
        for (ssn in 1:x$struct$tc)
        {
          line4 <- rbind(line4,paste(x$reg$seas_reg_flags_spp[ssn,,i],collapse=" "))
        }
      }
    } else {
      line4 <- rbind(line4,t4c)
      for (ssn in 1:x$struct$tc)
      {
        line4 <- rbind(line4,paste(x$reg$seas_reg_flags[ssn,],collapse=" "))
      }
    }
  }
  line5 <- x$mpy
  line6 <- paste(t(x$mweeks),collapse=" ")
  x$dl$dsets <- dim(x$mat)[1]
  line7 <- paste(c("   ",paste(x$dl,collapse= "         ")),collapse="")
  if(x$struct$te>=6) line8 <- paste(t8,paste(c("   ",paste(x$struct$age_inds,collapse= "         ")),collapse=""),sep="\n") else line8 <- ""
  if(multspp){
    if(l_inc[1]=="") {
      top <- paste(c(ttl,x$top,defs,t1,line1,t1_spp,line1a,line1b,
         t2,paste(x$reg$relreg,collapse=" "),t3,line3,t4a,t4b,line4,t5,line5,
         t6,line6,t7,line7,line8),collapse="\n")
    } else {
      top <- paste(c(ttl,x$top,defs,t1,line1,t1_spp,line1a,line1b,    
         t2,paste(x$reg$relreg,collapse=" "),t3,line3,line3a,t4b,line4,t5,line5,
         t6,line6,t7,line7,line8),collapse="\n")
    }
  } else {
    if(l_inc[1]=="") {
      top <- paste(c(ttl,x$top,defs,t1,line1,t2,paste(x$reg$relreg,collapse=" "),
         t3,line3,t4a,t4b,line4,t5,line5,t6,line6,t7,line7,line8),collapse="\n")
    } else {
      top <- paste(c(ttl,x$top,defs,t1,line1,t2,paste(x$reg$relreg,collapse=" "),
         t3,line3,t4a,l_inc,t4b,line4,t5,line5,t6,line6,t7,line7,line8),collapse="\n")
    }
  }
  fish <- sort(unique(m[,4]))
  matout <- vector(mode="character",length=0)
  if(x$struct$te>8){
    poslf <- 8 + ((3*nspp)+nspp)
  } else if(x$struct$te==8){
    poslf <- 12
  } else if(x$struct$te>=6 & x$struct$te<8){
    poslf <- 8 
  } else {
    poslf <- 7
  }
  lfint <- x$dl$lfint ; wfint <- x$dl$wfint
  if ((lfint!=0 && wfint==0) || (lfint==0 && wfint!=0)) {
    for (i in 1:nrow(m)) {
      if (m[i,poslf]==-1) {
        matout[i] <- " -1"   #paste(" ",m[i,(poslf+1):ncol(m)],collapse=" ")
      } else {
        matout[i] <- paste(" ",m[i,-(1:(poslf-1))],collapse=" ")
      }
    }
  } else if(lfint!=0 && wfint!=0){
    for (i in 1:nrow(m)) {
      if (m[i,poslf]==-1) {matout[i] <- " -1" ; nlf <- poslf+1}
      else {matout[i] <- paste(" ",m[i,poslf:(poslf+lfint-1)],collapse=" "); nlf <- poslf+lfint}
      if(m[i,nlf]==-1) matout[i] <- paste(matout[i]," -1")
      else matout[i] <- paste(matout[i]," ",paste(m[i,(nlf):(nlf+wfint-1)],collapse=" ")) 
      ##struc <- paste(m[i,7:(poslf-1)],collapse=" ")
      ##if (m[i,poslf]==-1) nlf <- 1 else nlf <- x$dl$lfint
      ##if (m[i,poslf+nlf]==-1) nwf <- 1 else nwf <- x$dl$wfint
      ##matout[i] <- paste(" ",c(struc, m[i,poslf:(poslf+nlf-1)], m[i,(poslf+nlf):(poslf+nlf+nwf-1)]),collapse=" ")
    }
  }
  writeLines(top,frqfile)
  tmpchar <- paste("#  ",paste(dimnames(m)[[2]],collapse="  "),sep="")
  write(tmpchar,frqfile,append=T)
  if(poslf==7){
    write.table(cbind(format(m[,1]),format(m[,2:4]),
                    format(formatC(m[,5],format="f",digits=2),justify="right"),
                    format(formatC(m[,6],format="f",digits=2),justify="right"),matout),
              frqfile, quote=F, sep=" ", row.names=F,col.names=F,append=T)
  } else if(poslf==8){
    write.table(cbind(format(m[,1]),format(m[,2:4]),
                    format(formatC(m[,5],format="f",digits=2),justify="right"),
                    format(formatC(m[,6],format="f",digits=2),justify="right"),
                    format(formatC(m[,7],format="f",digits=4),justify="right"),matout),
              frqfile, quote=F, sep=" ", row.names=F,col.names=F,append=T)
  } else if(poslf==12){
    write.table(cbind(format(m[,1]),format(m[,2:8]),
                    format(formatC(m[,9],format="f",digits=2),justify="right"),
                    format(formatC(m[,10],format="f",digits=2),justify="right"),
                    format(formatC(m[,11],format="f",digits=4),justify="right"),matout),
              frqfile, quote=F, sep=" ", row.names=F,col.names=F,append=T)
  } else {
    pos_sppinds <- 4 + ((3*nspp)+nspp)
    write.table(cbind(format(m[,1]),format(m[,(2:pos_sppinds)]),
                    format(formatC(m[,(pos_sppinds+1)],format="f",digits=2),justify="right"),
                    format(formatC(m[,(pos_sppinds+2)],format="f",digits=2),justify="right"),
                    format(formatC(m[,(pos_sppinds+3)],format="f",digits=4),justify="right"),matout),
              frqfile, quote=F, sep=" ", row.names=F,col.names=F,append=T)
  }
#
}
