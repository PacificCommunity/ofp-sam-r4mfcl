# #' Function to write frq file
# #'
# #' @param frqfile file name of frq file to write
# #' @param frq.obj ourputs from read.frq
# #'
# #' @importFrom utils write.table
# #' @export
write.frq <- function(frqfile,frq.obj) {
##============================================================================
## by Simon Hoyle June 2008
## fishery information is in $fish[] and $mat[,4]
## SDH 27/6/09 change for frq version 6
##  enhanced  PK 13/06/2011
## NDD 25/04/2019: fixed bug where extra "-1" was being added between effort dev column and first lf observation
## NDD 25/04/2019: corrects for old frqs that have all 0s in the composition data instead of -1 when both lf and wf exist
##============================================================================
    x<-frq.obj
    version<-x$version
    nsp<-x$struct$nsp
    m <- x$mat
    x$struct$nf <- length(unique(m[,4]))
    ttl <- x$frq.title
    defs <- paste(c("# Definition of fisheries","#","# Fishery   Gear   Nation          Region    Season, "),collapse="\n")
  for (i in 1:dim(x$fish)[1]) defs <- rbind(defs,paste(c("#",x$fish[i,3:6]),collapse="       "))
  defs <-rbind(defs,"#")
  t1 <- rbind(c("# Number of   Number of   Use generic   Number of     Year1","#  Region     Fisheries    diffusion    tag groups"))
  t2 <- "# Relative Region Size"
  t3 <- rbind(c("#","# Region in which each fishery is located"))
  t4a <- rbind(c("#","# Incidence matrix"))
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
  t4b <- rbind(c("#","# Data flags (for records 1, 0=catch in number; 1=catch in weight)"))
  t4c <- "# Season-region flags"
  t5 <- "# Number of movements per year"
  t6 <- "# Weeks in which movement occurs"
  t7 <- rbind(c("# fishery data","#","#","# Datasets / LFIntervals  LFFirst  LFWidth  LFFactor / WFIntervals  WFFirst  WFWidth"))
  t8 <- "# age_nage   age_age1"
  line1 <- paste(c("    ",paste(x$struct[1:5],collapse="           "),"", x$struct[6:10]),collapse=" ")
  line2 <- x$reg
  line3 <- paste(c(" ",t(x$fish$fishreg)),collapse=" ")
  a <- paste(rep(0,x$struct$nf),collapse=" ")
  line4 <- vector(mode="character")
  for(i in 1:dim(x$dflags)[1]){
    if(i < dim(x$dflags)[1]){
      line4 <- paste(line4,paste(as.character(x$dflags[i,]),collapse=" "),"\n",sep="")
    } else {
      line4 <- paste(line4,paste(as.character(x$dflags[i,]),collapse=" "),sep="")
    }
  }
  if(x$struct$te>=6) {
    line4 <- rbind(line4,t4c)
    for (ssn in 1:x$struct$tc) {
      line4 <- rbind(line4,paste(x$reg$seas_reg_flags[ssn,],collapse=" "))
      }
    }
  line5 <- x$mpy
  ##browser()
  line6 <- paste(t(x$mweeks),collapse=" ")
  x$dl$dsets <- dim(x$mat)[1]
  line7 <- paste(c("   ",paste(x$dl,collapse= "         ")),collapse="")
  if(x$struct$te>=6) line8 <- paste(t8,paste(c("   ",paste(x$struct$age_inds,collapse= "         ")),collapse=""),sep="\n") else line8 <- ""
  if(l_inc[1]=="") {
    top <- paste(c(ttl,x$top,defs,t1,line1,t2,paste(x$reg$relreg,collapse=" "),t3,line3,t4a,t4b,line4,t5,line5,t6,line6,t7,line7,line8),collapse="\n")
  } else {
    top <- paste(c(ttl,x$top,defs,t1,line1,t2,paste(x$reg$relreg,collapse=" "),t3,line3,t4a,l_inc,t4b,line4,t5,line5,t6,line6,t7,line7,line8),collapse="\n")
  }
  fish <- sort(unique(m[,4]))
  matout <- vector(mode="character",length=0)
  poslf <- if(version<6){
    7
  }else if(version>=6 & version <8 ){
    8
  }else if(version==8 ){
    8+nsp*3
  }else if(version==9 ){
    8+nsp*4
  }else{
    stop("frq version is ",x$struct$te)
  } 
  lfint <- x$dl$lfint ; wfint <- x$dl$wfint
  if ((lfint!=0 && wfint==0) || (lfint==0 && wfint!=0)) {
    for (i in 1:nrow(m)) {
      if (m[i,poslf]==-1 || sum(m[i,poslf:dim(m)[2]])==0) {
        matout[i] <- " -1"   #paste(" ",m[i,(poslf+1):ncol(m)],collapse=" ")
      } else {
        matout[i] <- paste(" ",m[i,-(1:(poslf-1))],collapse=" ")
      }
    }
  } else if (lfint!=0 && wfint!=0) {
    for (i in 1:nrow(m)) {
      if (m[i,poslf]==-1 || sum(m[i,poslf+1:lfint-1])==0) 
      {
        matout[i] <- " -1"
        if(m[i,poslf]==-1)
        {
          nlf <- poslf+1
        } else {
          nlf <- poslf+lfint
        }
      } else {
        matout[i] <- paste(" ",m[i,poslf:(poslf+lfint-1)],collapse=" "); nlf <- poslf+lfint
      }
      if(m[i,nlf]==-1 || sum(m[i,nlf+1:wfint-1])==0)
      {
        matout[i] <- paste(matout[i]," -1")
      } else {
        matout[i] <- paste(matout[i]," ",paste(m[i,(nlf):(nlf+wfint-1)],collapse=" ")) 
      }
    }
  }
  writeLines(top,frqfile)
  if(poslf==7) {
    write.table(cbind(format(m[,1]),format(m[,2:4]),
                    format(formatC(m[,5],format="f",digits=2),justify="right"),
                    format(formatC(m[,6],format="f",digits=2),justify="right"),matout),
              frqfile, quote=F, sep=" ", row.names=F,col.names=F,append=T)
  } else {
    write.table(cbind(format(m[,1]),format(m[,2:4]),
                    format(formatC(m[,5],format="f",digits=2),justify="right"),
                    format(formatC(m[,6],format="f",digits=2),justify="right"),
                    format(formatC(m[,7],format="f",digits=4),justify="right"),matout),
              frqfile, quote=F, sep=" ", row.names=F,col.names=F,append=T)
  }
}
