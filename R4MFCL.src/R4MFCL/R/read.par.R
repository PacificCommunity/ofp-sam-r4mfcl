#' by Simon Hoyle June 2008
#' This is only partly built. Needs to be extended so it gets the whole par file into an object. Then do the same for write.par...
#' Nick : added sections explicitly for # tag flags; # tag fish rep; # tag fish rep group flags; # tag_fish_rep active flags; # tag_fish_rep target; # tag_fish_rep penalty
#' NMD 22/06/12 - allow instance of no tag flags or tag pars at all (e.g. striped marlin)
#' YT : 27/02/17 : Upgraded for multi species/sex model
#' YT : 10/06/2017 : made it compatible to para file version 1042
#' @author Simon Hoyle
#' @param par.file charactor file name of par file
#' @param verbose if TRUE verbose outputs will be made
#' @param DEBUG enable "debug" through browser()
#' @param nfisheries Number of fisheries, default is NA and determined internally
#' @param nReg number of regions , default : NA and determined internally 
#'
#' @importFrom magrittr "%>%"
#' @export
read.par <-
function(par.file,verbose=TRUE,DEBUG=FALSE,nfisheries=NA,nReg=NA) {
#  require(magrittr)
  datfromstr<-function (datstring)
  {
    out<-if(length(datstring)>1){
      datstring %>% sapply(.,"trimws")  %>% strsplit(split = "[[:blank:]]+") %>% sapply(.,"as.numeric",USE.NAMES =FALSE,simplify="array") %>%t()
    }else{
      datstring %>% trimws() %>% strsplit(split = "[[:blank:]]+") %>% "[["(1) %>% as.numeric()
    }
    return(out)
  }
  cat("Starting read.par ; file name of par.file is ",par.file,"\n")
  a <- readLines(par.file)
  if(verbose)cat("L27 ; ") ; if(DEBUG)browser()
  pfl <- datfromstr(a[2]) # as.numeric(unlist(strsplit(a[2],split="[[:blank:]]+"))[-1])
  version<-pfl[200]
  pos1<-grep("# The number of age classes",a)
  nages <- a[pos1+1]
  afl.ln<- grep(a,pattern="age flags")[1]+1 # YT 24/02/2017 To allow to read par file of multi sp/sex model
  afl <- datfromstr(a[afl.ln]) # as.numeric(unlist(strsplit(a[afl.ln],split="[[:blank:]]+"))[-1])
  mp.nages<-ifelse(!is.na(ax<-grep(a,pattern="# Multi-species the number of age classes")[1]+1),ax,NA)
# 13/03/2017 YT added code to read multi-sp/sex age flags
  mp.afl.ln<-ifelse(!is.na(ax<-grep(a,pattern="# multi-species age flags")[1]+1),ax,NA)
  nSp<-ifelse(is.na(mp.afl.ln),1,2) ## YT current code does not support multi-species model with number of species more than 2
  if(verbose)cat("L41;") ; if(DEBUG)browser()
  mp.afl<-if(!is.na(mp.afl.ln)){
      datfromstr(a[mp.afl.ln])
  }else{NULL}
  ### New code to determine number of regions 
  if(is.na(nReg)){
  	pos1<-grep("# region control flags",a)
  	nReg<-a[pos1+1] %>% trimws() %>% strsplit(split="[[:blank:]]+") %>% "[["(1) %>% length()
  }
  ######### 
  pos1 <- grep("fish flags",a) ; if(length(pos1)>1)pos1<-pos1[1]
  if(is.na(nfisheries)){
  	pos2<-grep("tag flags",a) ;
  	if(length(pos2)==0)   pos2 <- grep("# region control flags",a) 
  	nfisheries<-pos2-1-pos1  # 2018/09/06 Corrected to cound nfisheries nfisheries<-pos2-1-(pos1+1)
  }else{
  	pos2<-pos1+nfisheries-1
  }
  if(verbose)cat("L59 ;")  ; if(DEBUG)browser()
  ffl<-datfromstr(a[(pos1+1):(pos2-1)])
  if(verbose)cat("L61 ;")  ; if(DEBUG)browser()
#  cat("L21 read.par.r ;");browser()
 # nfisheries <- dim(ffl)[1]
  ## check if selectivity time block is implemented
  if(verbose)cat("L64 ;"); if(DEBUG)browser()
  if(verbose)cat("dim(ffl)=",dim(ffl),"\n")
  do.selblocks<-ifelse(any(ffl[,71]>0),TRUE,FALSE)
# Check if there are the new tag report sections in the par file
  tsw <- 0  #Default switch setting on tag parameters to zero
  tsw1 <- 0  #Default switch setting on tag parameters to zero
  tsw2 <- 0  #Default switch setting on tag parameters to zero
  tsw3 <- 0  #Default switch setting on tag parameters to zero
  if(length(as.numeric(grep("# tag fish rep",a))) > 0){
#   set the switch on for existence of tagging reporting parameters
    tsw <- 1
#   load block of tag flags
    pos1 <- pos2 ; pos2 <- min(grep("# tag fish rep",a))
    tfl<-datfromstr(a[(pos1+1):(pos2-1)])
#   load block of tag fish rep
    pos1 <- pos2 ; pos2 <- grep("# tag fish rep group flags",a)
    trpfl<-datfromstr(a[(pos1+1):(pos2-1)])
#   load block of tag fish rep group flags
    pos1 <- pos2 ; pos2 <- grep("# tag_fish_rep active flags",a)
    trpgpfl<-datfromstr(a[(pos1+1):(pos2-1)])
# Check for presence of tag_fish_rep target and tag_fish_rep penalty blocks
    if(length(as.numeric(grep("# tag_fish_rep target",a))) > 0) (tsw2 <- 1)
#   load block of tag_fish_rep active flags
    pos1 <- pos2
    if(tsw2 == 1 ){
      pos2 <- grep("# tag_fish_rep target",a)
    } else {
      pos2 <- grep("# region control flags",a)
    }
    trpacfl<-datfromstr(a[(pos1+1):(pos2-1)])
    if(tsw2 == 1 ){
#   load block of tag_fish_rep target
      pos1 <- pos2 ; pos2 <- grep("# tag_fish_rep penalty",a)
      treptarg<-datfromstr(a[(pos1+1):(pos2-1)])
#   load block of tag_fish_rep penalty
      pos1 <- pos2 ; pos2 <- grep("# region control flags",a)
      treppen<-datfromstr(a[(pos1+1):(pos2-1)])
    }
  } else if(length(as.numeric(grep("tag flags",a))) > 0) {   # Tag reporting rate parameter blocks - just load tag flags
#   load block of tag flags
    tsw3 <- 1
    pos1 <- pos2 ; pos2 <- grep("# region control flags",a)
    tfl<-datfromstr(a[(pos1+1):(pos2-1)])
  } else { #no tag data at all
    tsw1 <- 1
  }
## 13/03/2017 YT Added code to read region control flags
	if(length(grep("# region control flags",a))>0){
  	pos1<-grep("# region control flags",a)+1;
  	pos2<-pos1+9
  	if(verbose)cat("L114; ") #;browser()
  	rcfl<-datfromstr(a[pos1:pos2])
  }else{
  	rcfl<-NULL	
  }
  sp.fl<-if(nSp>1){
    pos1<-grep("# species flags",a)+1
    datfromstr(a[pos1+0:1])
  }else{NULL}
  pos1 <- grep("# percent maturity", a)[1]+1
  #cat("L125\n");browser()
  maturity <- as.numeric(unlist(strsplit(trimws(a[pos1]),split="[[:blank:]]+")))
  if(nSp>1){
    mp.maturity <- datfromstr(a[pos1+2]) # as.numeric(unlist(strsplit(a[pos1+2],split="[[:blank:]]+"))[-1])
  }
  pos1 <- grep("# total populations scaling parameter", a)[1]+1; totpop <- as.double(a[pos1])
  if(nSp>1){mp.totpop<-as.double(a[pos1+2])}
  pos1 <- grep("# implicit total populations scaling parameter", a)[1]+1; totpop_implicit <- as.double(a[pos1])
  pos1 <- grep("# rec init pop level difference", a)[1]+1; rec_init <- as.double(a[pos1])
  pos1 <- grep("# recruitment times", a)[1]+1; rectimes <- datfromstr(a[pos1]) #as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# relative recruitment", a)[1]+2; rel_recruitment <- datfromstr(a[pos1]) #as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  if(nSp>1){pos1 <- grep("# multi-species relative recruitment", a)[1]+2; mp.rel_recruitment <- datfromstr(a[pos1])}
  pos1 <- grep("# fishery selectivity", a)[1]+1 #; selectivity <- datfromstr(a[pos1]) #as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  if(verbose)cat("L112 ; ") #;browser()
  selectivity <- if(!do.selblocks){
    datfromstr(a[pos1:(pos1+nfisheries-1)])
  }else{
    datfromstr(a[pos1:(pos1+nfisheries-1+sum(ffl[,71]))])
  }
  pos1 <- grep("# natural mortality coefficient", a)[1]+2; Mbase <- as.double(a[pos1])
  if(nSp>1){mp.Mbase<-as.double(a[pos1+2])}
  if(verbose)cat("L120 ; ")  #;browser()
  pos1 <- grep("# effort deviation coefficients", a)[1]; pos1b <- pos1+nfisheries; effdevcoffs <- datfromstr(a[(pos1+1):pos1b]) #strsplit(a[(pos1+1):pos1b],split="[[:blank:]]+")
  rowMax <- max(sapply(effdevcoffs, length))
  effdevcoffs <- do.call(rbind, lapply(effdevcoffs, function(x){ length(x) <- rowMax; as.numeric(x[2:rowMax]) }))
  pos1 <- grep("# average catchability coefficients", a)[1]+3; meanqs <- as.numeric(unlist(strsplit(trimws(a[pos1]),split="[[:blank:]]+")))
  pos1 <- grep("# Objective function value", a)[1]; obj <- as.double(a[pos1 + 1])
  pos1 <- grep("# The number of parameters", a)[1]; npars <- as.double(a[pos1 + 1])
  pos1 <- grep("# Maximum magnitude gradient value", a)[1]; gradient <- as.double(a[pos1 + 1])
  pos1 <- grep("# The von Bertalanffy parameters", a)[1];
     Lmin <- datfromstr(a[pos1+1])[1] # as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[1])
     Lmax <- datfromstr(a[pos1+2])[1] # as.numeric(unlist(strsplit(a[pos1+2],split="[[:blank:]]+"))[1])
     K    <- datfromstr(a[pos1+3])[1] # as.numeric(unlist(strsplit(a[pos1+3],split="[[:blank:]]+"))[1])
# For multi sp/sex models YT 13/03/2017
  if(nSp>1){
     mp.Lmin <- datfromstr(a[pos1+7])[1] # as.numeric(unlist(strsplit(a[pos1+7],split="[[:blank:]]+"))[1])
     mp.Lmax <- datfromstr(a[pos1+8])[1] # as.numeric(unlist(strsplit(a[pos1+8],split="[[:blank:]]+"))[1])
     mp.K    <- datfromstr(a[pos1+9])[1] # as.numeric(unlist(strsplit(a[pos1+9],split="[[:blank:]]+"))[1])
  }
  if(verbose)cat("L161 ; ") #;browser()
  pos1 <- grep("# Variance parameters", a)[1];
  growth_vars <-c(datfromstr(a[pos1+1])[1],datfromstr(a[pos1+2])[1])
  if(verbose)cat("L164 ; ") #;browser()
  pos1 <- grep("# extra par for Richards", a)[1]; Richards <- as.double(a[pos1 + 1])
  if(verbose)cat("L166 ; ") #;browser()
  if(nSp>1){
    mp.Richards <- as.double(a[pos1 + 5])
  }
  pos1 <- grep("# age-class related parameters \\(age_pars\\)", a)[1];
  M_offsets <-  datfromstr(a[pos1+4]) # as.numeric(unlist(strsplit(a[pos1+4],split="[[:blank:]]+"))[-1])
  gr_offsets <- datfromstr(a[pos1+5]) #as.numeric(unlist(strsplit(a[pos1+5],split="[[:blank:]]+"))[-1])
  if(verbose)cat("L150;") #;browser()

  if(nSp>1){
    pos1_0 <- grep("#multi-species age-class related parameters \\(age_pars\\), species:",a)
    mp.M_offsets<-list()
    mp.gr_offsets<-list()
    for(sp in 2:nSp){
      pos1<-pos1_0[sp-1]
      mp.M_offsets[[sp]] <-  datfromstr(a[pos1+4])
      mp.gr_offsets[[sp]] <- datfromstr(a[pos1+5])
    }
  }
## extra fishery parameters (fishpars)
pos1<-grep("^# extra fishery parameters",a)[1]+3
nrow<-if(version>=1052){
  50
}else{
  20
}
if(verbose)cat("L195 ;") #;browser()
fish_pars<- datfromstr(a[pos1+1:nrow-1])
if(verbose)cat("L197 ; ") #;browser()
if(version>1051){ 
#  cat("L199\n");browser()
  pos1<-grep("# species parameters",a);sp_pars<-datfromstr(a[pos1+1:20+1])
}else{

}
##
if(length(grep("# The grouped_catch_dev_coffs flag",a))>0){
	pos1<-grep("# The grouped_catch_dev_coffs flag",a)
  nrow<-length(unique(ffl[,29]))
	grouped_catch_dev_coffs<-datfromstr(a[pos1+1:nrow])
}

if(verbose)cat("L200 ; ") #;browser()
# Check for existence of new tagging inputs
  if(tsw != 0){
    if(tsw2 == 1){  # Load all tag reporting rate blocks
      par.obj <- list(pfl=pfl,
                  nages=nages,
                  afl=afl,ffl=ffl,tfl=tfl,trpfl=trpfl,trpgpfl=trpgpfl,trpacfl=trpacfl,treptarg=treptarg,treppen=treppen,
                  maturity=maturity,totpop=totpop,totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,rel_recruitment=rel_recruitment,
                  Mbase=Mbase,
                  selectivity=selectivity,
                  effdevcoffs=effdevcoffs,
                  meanqs=meanqs,
                  obj=obj,
                  npars=npars,
                  gradient=gradient,
                  Lmin=Lmin, Lmax=Lmax, K=K, growth_vars=growth_vars, Richards=Richards, gr_offsets=gr_offsets,
                  rem=a[pos2:length(a)])
    } else {      # Just load up to (including) tag rep active flags
      par.obj <- list(pfl=pfl,
                  nages=nages,
                  afl=afl,ffl=ffl,tfl=tfl,trpfl=trpfl,trpgpfl=trpgpfl,trpacfl=trpacfl,
                  maturity=maturity,totpop=totpop,totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,rel_recruitment=rel_recruitment,
                  Mbase=Mbase,
                  selectivity=selectivity,
                  effdevcoffs=effdevcoffs,
                  meanqs=meanqs,
                  obj=obj,
                  npars=npars,
                  gradient=gradient,
                  Lmin=Lmin, Lmax=Lmax, K=K, growth_vars=growth_vars, Richards=Richards, gr_offsets=gr_offsets,
                  rem=a[pos2:length(a)])
    }
  } else if(tsw3 == 1) {    # Just load up to and including tag flags
    par.obj <- list(pfl=pfl,
                  nages=nages,
                  afl=afl,ffl=ffl,tfl=tfl,
                  maturity=maturity,totpop=totpop,totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,rel_recruitment=rel_recruitment,
                  Mbase=Mbase,
                  selectivity=selectivity,
                  effdevcoffs=effdevcoffs,
                  meanqs=meanqs,
                  obj=obj,
                  npars=npars,
                  gradient=gradient,
                  Lmin=Lmin, Lmax=Lmax, K=K, growth_vars=growth_vars, Richards=Richards, gr_offsets=gr_offsets,
                  rem=a[pos2:length(a)])
  } else if(tsw1 == 1) {    # No tag data at all
    par.obj <- list(pfl=pfl,
                  nages=nages,
                  afl=afl,ffl=ffl,
                  maturity=maturity,totpop=totpop,totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,rel_recruitment=rel_recruitment,
                  Mbase=Mbase,
                  selectivity=selectivity,
                  effdevcoffs=effdevcoffs,
                  meanqs=meanqs,
                  obj=obj,
                  npars=npars,
                  gradient=gradient,
                  Lmin=Lmin, Lmax=Lmax, K=K, growth_vars=growth_vars, Richards=Richards, gr_offsets=gr_offsets,
                  rem=a[pos2:length(a)])
  }
  par.obj$nSp<-nSp
  par.obj$version<-version
  par.obj$filename<-par.file
  par.obj$fish_pars<-fish_pars
  par.obj$rcfl<-rcfl # region control flags
  par.obj$do.selblocks<-do.selblocks
  par.obj$nReg <- nReg
  if(nSp>1){
    par.obj$mp.Lmin<-mp.Lmin
    par.obj$mp.Lmax<-mp.Lmax
    par.obj$mp.K<-mp.K
    par.obj$mp.Richards<-mp.Richards
    par.obj$mp.maturity<-mp.maturity
    par.obj$mp.afl<-mp.afl
    par.obj$mp.nages<-mp.nages
    par.obj$mp.rel_recruitment<-mp.rel_recruitment
    par.obj$sp_pars<-sp_pars
    par.obj$mp.totpop<-mp.totpop
    par.obj$mp.Mbase<-mp.Mbase
    par.obj$mp.M_offsets<-mp.M_offsets
    par.obj$mp.gr_offsets<-mp.gr_offsets
  }
  if(verbose)cat(" finished read.par\n")
  return(invisible(par.obj))
}
