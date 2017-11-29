#' @importFrom utils write.table
 write_nmd.frq <-
function(new.frq,frq.obj) {
  # by Simon Hoyle June 2008
  # fishery information is in $fish[] and $mat[,4]
  # SDH 27/6/09 change for frq version 6
  # NMD 7/7/10 added dflags object to output (deleted use of x$fish$ctype)
  x<-frq.obj
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
  line6 <- paste(t(x$mweeks),collapse=" ")
  line7 <- paste(c("   ",paste(x$dl,collapse= "         ")),collapse="")
  if(x$struct$te>=6) line8 <- paste(t8,paste(c("   ",paste(x$struct$age_inds,collapse= "         ")),collapse=""),sep="\n") else line8 <- ""
  if(l_inc=="") {
    top <- paste(c(ttl,x$top,defs,t1,line1,t2,paste(x$reg$relreg,collapse=" "),t3,line3,t4a,t4b,line4,t5,line5,t6,line6,t7,line7,line8),collapse="\n")
  } else {
    top <- paste(c(ttl,x$top,defs,t1,line1,t2,paste(x$reg$relreg,collapse=" "),t3,line3,t4a,l_inc,t4b,line4,t5,line5,t6,line6,t7,line7,line8),collapse="\n")
  }
  fish <- sort(unique(m[,4]))
  matout <- vector(mode="character",length=0)
  if(x$struct$te>=6) poslf <- 8 else poslf <- 7
  if ((x$dl$lfint!=0 && x$dl$wfint==0) || (x$dl$lfint==0 && x$dl$wfint!=0)) {
    for (i in 1:dim(m)[1]) {
      if (m[i,poslf]==-1) matout[i] <- paste(m[i,1:poslf],collapse=" ") else  {matout[i] <- paste(m[i,],collapse=" ")}
      } }
  if (x$dl$lfint!=0 && x$dl$wfint!=0) {
    for (i in 1:dim(m)[1]) {
      struc <- paste(m[i,1:(poslf-1)],collapse=" ")
      if (m[i,poslf]==-1) nlf <- 1 else nlf <- x$dl$lfint
      if (m[i,poslf+nlf]==-1) nwf <- 1 else nwf <- x$dl$wfint
      matout[i] <- paste(c(struc, m[i,poslf:(poslf+nlf-1)], m[i,(poslf+nlf):(poslf+nlf+nwf-1)]),collapse=" ")
      } }
  writeLines(top,new.frq)
  write.table(matout, new.frq, quote=F, sep=" ", row.names=F,col.names=F,append=T)
}
