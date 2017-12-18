#' read ini file
#'
#' @param ini.file file name of ini file
#' @param nSp number of species/sex
#' @param nReg number of regions
#' @param mpy number of movement per year
#' @param size of incidence matrix
#' @export
read.ini <- function(ini.file,nSp=1,nReg=2,mpy=4,incidence=c(1,1))
##============================================================================
## by Simon Hoyle June 2008
##  revised, PK June 2011
## YT 24/02/2017 Revised to read ini file for multi-sp/sex model
## Because ini file is not self consistent, unless standard comments lines are used
## Additional new arguments
## This also allows to have non-standard comments lines
##============================================================================
## incidence : incidence matrix defined in frq file
## mpy : number of movements per year
## nSp : number of species
## nReg : number of region
{
  a <- readLines(ini.file)
  hpts <- grep("^#",a)
  ini.obj <- list()

  pos <- grep("version number",a,ignore.case=T)+1
#  cat("L16\n");browser()
  if(length(pos)==0) ini.obj$version=0
  else ini.obj$version <- as.numeric(a[pos]) # scanText(a[pos],what=0)
  if(ini.obj$version>1001){
    ini.obj<-read.ini1002(ini.file=ini.file,nSp=nSp,nReg=nReg,mpy=mpy,incidence=incidence)
  }else{
    pos <- grep("# tag fish rep *$",a,ignore.case=T)+1
    if(length(pos)>0) {
      p2 <- hpts[hpts>pos][1]-1
      ini.obj$tag.fish.rep <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
    }

    pos <- grep("# tag fish rep group flags",a,ignore.case=T)+1
    if(length(pos)>0) {
      p2 <- hpts[hpts>pos][1]-1
      ini.obj$grpflags <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
    }

    pos <- grep("# tag_fish_rep active flags",a,ignore.case=T)+1
    if(length(pos)>0) {
      p2 <- hpts[hpts>pos][1]-1
      ini.obj$activeflags <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
    }

    pos <- grep("# tag_fish_rep target",a,ignore.case=T)+1
    if(length(pos)>0) {
      p2 <- hpts[hpts>pos][1]-1
      ini.obj$reptarget <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
    }

    pos <- grep("# tag_fish_rep penalty",a,ignore.case=T)+1
    if(length(pos)>0) {
      p2 <- hpts[hpts>pos][1]-1
      ini.obj$reppenalty <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
    }

    pos <- grep("# sv\\(29\\)",a)+1
    if(length(pos)>0) { if(!(pos%in%hpts)) {
      ini.obj$sv29 <- scanText(a[pos],what=0)
      a[pos] <- paste("#",a[pos]) # to keep it from being chosen below
    }}

    pos <- grep("# number of age classes",a,ignore.case=T)+1
    ini.obj$nages <- as.numeric(a[pos])

    pos <- grep("# MATURITY AT AGE",a,ignore.case=T)+1
    ini.obj$mat <- scanText(a[pos],what=0)

    pos <- grep("# natural mortality",a,ignore.case=T)+1
    ini.obj$M <- scanText(a[pos],what=0)

    pos <- grep("# move",a,ignore.case=T)+1
    ini.obj$movemap <- scanText(a[pos],what=0)
    if(ini.obj$movemap[1]==0)
    {
      ini.obj$diffcoffs <- 0
    } else {
      pos <- grep("# diffusion coffs",a,ignore.case=T)+1
      p2 <- hpts[hpts>pos][1]-1
      ini.obj$diffcoffs <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
    }

    pos <- grep("# age_pars",a,ignore.case=T)+1
    if(length(pos)>0) {
      p2 <- hpts[hpts>pos][1]-1
      ini.obj$age_pars <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
    }

    pos <- grep("# recruitment",a,ignore.case=T)+1
    ini.obj$recbyreg <- scanText(a[pos],what=0)

    b <- scanText(a[(hpts[hpts>pos][1]):length(a)],what=0, comment.char="#")
 #   cat("L92\n");browser()
    ini.obj$VBLmin   <- b[1:3]
    ini.obj$VBLmax   <- b[4:6]
    ini.obj$VBK      <- b[7:9]
    ini.obj$LW       <- b[10:11]
    ini.obj$steepness<- ini.obj$sv29 #  b[12]  # YT 24/02/2017
    ini.obj$sdLatA   <- b[12:14]
    ini.obj$Ldep_sd  <- b[15:17]
    ini.obj$Nmeanconstr <- b[18:length(b)]
  }
  return(ini.obj)
}

#### Function to read ini files with version==1002

read.ini1002<-function(ini.file,nSp=2,nReg=2,mpy=4,incidence=c(1,1),verbose=TRUE){

  if(verbose)cat("running read.ini1002\n")
  dat<-readLines(ini.file,warn=FALSE)

  # parse all the numeric values into a long vector (allnums)
  allnums <- NULL
  for(i in 1:length(dat)){
    # split along blank spaces
    mysplit <- strsplit(dat[i],split="[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit!=""]
    # if final value is a number is followed immediately by a pound ("1#"),
    # this needs to be split
    nvals <- length(mysplit)
    if(nvals>0) mysplit[nvals] <- strsplit(mysplit[nvals],"#")[[1]][1]
    # convert to numeric
    nums <- suppressWarnings(as.numeric(mysplit))
    if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
    else maxcol <- length(nums)
    if(maxcol > 0){
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }
  ini.obj<-list()
  i<-1
  ini.obj$version<-as.numeric(allnums[i]);i<-i+1
#  cat("L129\n");browser()
  ini.obj$nages<-if(nSp==1){allnums[i]}else{allnums[i+1:nSp-1]};i<-i+nSp
  nages<-ini.obj$nages
  ini.obj$reg.flg<-matrix(data=allnums[i+1:(nSp*nReg*10)-1],nrow=10,byrow=TRUE);i<-i+nSp*nReg*10
  ini.obj$sp.flg<-if(nSp==1){allnums[i+1:10-1]}else{matrix(data=allnums[i+1:(10*nSp)-1],ncol=10,byrow=TRUE)};i<-i+10*nSp
  ini.obj$mat<-if(nSp==1){
      allnums[i+1:nages-1]
    }else{
      matrix(data=allnums[i+1:sum(nages)-1],nrow=nSp,byrow=TRUE)
    }
  i<-i+ifelse(nSp==1,nages,sum(nages))
  ini.obj$M<-allnums[i+1:nSp-1];i<-i+nSp
  ini.obj$movemap <-allnums[i+1:mpy-1];i<-i+mpy
  ini.obj$diffcoffs <-if(nReg==1){
      0
    }else{
      matrix(data=allnums[i+1:(length(ini.obj$movemap)*sum(incidence)*nSp)-1],nrow=length(ini.obj$movemap)*nSp,byrow=TRUE)
    }
    i<-i+ifelse(nReg==1,nSp,length(ini.obj$movemap)*sum(incidence)*nSp)
  nages1<-if(nSp==1){nages}else{nages[1]}
  ini.obj$age_pars <-matrix(data=allnums[i+1:(nages1*10*nSp)-1],byrow=TRUE,ncol=nages1);i<-i+nages1*10*nSp
  ini.obj$recbyreg <-if(nSp==1){allnums[i+1:nReg-1]}else{matrix(data=allnums[i+1:(nReg*nSp)-1],byrow=TRUE,ncol=nReg)};i<-i+nReg*nSp
#  cat("L149\n");browser()
  ini.obj$VBLmin   <- allnums[i+1:3-1];i<-i+3
  ini.obj$VBLmax   <- allnums[i+1:3-1];i<-i+3
  ini.obj$VBK      <- allnums[i+1:3-1];i<-i+3

  if(nSp>1){
    for(j in 2:nSp){
      ini.obj$VBLmin   <- rbind(ini.obj$VBLmin,allnums[i+1:3-1]);i<-i+3
      ini.obj$VBLmax   <- rbind(ini.obj$VBLmax,allnums[i+1:3-1]);i<-i+3
      ini.obj$VBK      <- rbind(ini.obj$VBK,allnums[i+1:3-1]);i<-i+3
    }
  }
#  cat("L161\n");browser()
  ini.obj$LW     <- allnums[i+1:2-1];i<-i+2
  ini.obj$steepness<- allnums[i];i<-i+1
  if(nSp>1){
    for(j in 2:nSp){
      ini.obj$LW     <- rbind(ini.obj$LW,allnums[i+1:2-1]);i<-i+2
      ini.obj$steepness<- rbind(ini.obj$steepness,allnums[i]);i<-i+1
    }
  }

  ini.obj$sdLatA   <- allnums[i+1:3-1];i<-i+3
  ini.obj$Ldep_sd  <- allnums[i+1:3-1];i<-i+3
  if(nSp>1){
    for(j in 2:nSp){
      ini.obj$sdLatA   <- rbind(ini.obj$sdLatA,allnums[i+1:3-1]);i<-i+3
      ini.obj$Ldep_sd  <- rbind(ini.obj$Ldep_sd,allnums[i+1:3-1]);i<-i+3
    }
  }
  ini.obj$Nmeanconstr <- allnums[i];i<-i+1
  # need to add codes to read mean constraints
  ini.obj$nSp<-nSp
  return(ini.obj)
}

