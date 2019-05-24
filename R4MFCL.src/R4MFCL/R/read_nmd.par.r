 read_nmd.par <-
function(par.file) {
  # by Simon Hoyle June 2008
  # This is only partly built. Needs to be extended so it gets the whole par file into an object. Then do the same for write.par...
  # Nick : added sections explicitly for # tag flags; # tag fish rep; # tag fish rep group flags; # tag_fish_rep active flags; # tag_fish_rep target; # tag_fish_rep penalty

  a <- readLines(par.file)
  pfl <- as.numeric(unlist(strsplit(a[2],split="[[:blank:]]+"))[-1])
  vsn <- pfl[200]
  nages <- a[5]
  afl <- as.numeric(unlist(strsplit(a[7],split="[[:blank:]]+"))[-1])
  pos1 <- grep("fish flags",a) ;
# Check for version
  if(vsn > 1047)   pos1 <- pos1[1]   # Version 1048 has historical flags  
  pos2 <- grep("tag flags",a) ;
  if(length(pos2)==0){
    pos2 <- grep("# region control flags",a) ;
    if(vsn == 1055) pos2 <- pos2 - 2
  } else {
    if(vsn == 1055) pos2 <- pos2 - 1
  }
  ffl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  for (i in (pos1+2):(pos2-1)) {
    ffl <- rbind(ffl, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }
  nfisheries <- dim(ffl)[1]
  pos2 <- grep("tag flags",a) ;
# Check if there are the new tag report sections in the par file
  tsw <- 0  #Default switch setting on tag parameters to zero
  tsw1 <- 0  #Default switch setting on tag parameters to zero
  tsw2 <- 0  #Default switch setting on tag parameters to zero
  tsw3 <- 0  #Default switch setting on tag parameters to zero
  if(length(as.numeric(grep("# tag fish rep",a))) > 0){
#   set the switch on for existence of tagging reporting parameters
    tsw <- 1
#   load block of tag flags
    pos1 <- pos2 ; 
    pos2 <- min(grep("# tag fish rep",a))
    if(vsn == 1055 || pfl[195]>0) pos2 <- pos2 - 2
    tfl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    for (i in (pos1+2):(pos2-1)) {
      tfl <- rbind(tfl, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }
#   load block of tag fish rep
    pos2 <- min(grep("# tag fish rep",a))
    pos1 <- pos2 ; pos2 <- grep("# tag fish rep group flags",a)
    trpfl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    for (i in (pos1+2):(pos2-2)) {
      trpfl <- rbind(trpfl, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }
#   load block of tag fish rep group flags
    pos1 <- pos2 ; pos2 <- grep("# tag_fish_rep active flags",a)
    trpgpfl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    for (i in (pos1+2):(pos2-1)) {      
      trpgpfl <- rbind(trpgpfl, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }
# Check for presence of tag_fish_rep target and tag_fish_rep penalty blocks
    if(length(as.numeric(grep("# tag_fish_rep target",a))) > 0) (tsw2 <- 1)
#   load block of tag_fish_rep active flags
    pos1 <- pos2
    if(tsw2 == 1 ){
      pos2 <- grep("# tag_fish_rep target",a)
    } else {
      pos2 <- grep("# region control flags",a)
    }
    trpacfl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    for (i in (pos1+2):(pos2-1)) {
      trpacfl <- rbind(trpacfl, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }

    if(tsw2 == 1 ){
#   load block of tag_fish_rep target
      pos1 <- pos2 ; pos2 <- grep("# tag_fish_rep penalty",a)
      treptarg <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
      for (i in (pos1+2):(pos2-2)) {        # Note this is pos2-2 because there is a blank line
        treptarg <- rbind(treptarg, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
      }
#   load block of tag_fish_rep penalty
      pos1 <- pos2 ; pos2 <- grep("# region control flags",a)
      treppen <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
      for (i in (pos1+2):(pos2-2)) {        # Note this is pos2-2 because there is a blank line
        treppen <- rbind(treppen, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
      }
    } 
  } else if(length(as.numeric(grep("tag flags",a))) > 0 & length(as.numeric(grep("# tag fish rep",a))) == 0 ) {   # Tag reporting rate parameter blocks - just load tag flags
#   load block of tag flags
    tsw3 <- 1
    pos1 <- pos2 ; pos2 <- grep("# region control flags",a)
    tfl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    for (i in (pos1+2):(pos2-1)) {
      tfl <- rbind(tfl, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }
  } else { #no tag data at all
    tsw1 <- 1
    pos2 <- grep("# region control flags",a)
  }
  pos1 <- grep("# percent maturity", a)[1]+1; maturity <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# total populations scaling parameter", a)[1]+1; totpop <- as.double(a[pos1])
  pos1 <- grep("# implicit total populations scaling parameter", a)[1]+1; totpop_implicit <- as.double(a[pos1])
  pos1 <- grep("# rec init pop level difference", a)[1]+1; rec_init <- as.double(a[pos1])
  pos1 <- grep("# recruitment times", a)[1]+1; rectimes <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# relative recruitment", a)[1]+2; rel_recruitment <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  if(vsn > 1047)    # read in the lagrangian_lambdas
  {
    pos1 <- grep("# Lambdas for augmented Lagrangian", a)[1]
    if(vsn > 1052)
    {
      pos1b <- grep("# Other lambdas for augmented Lagrangian", a)[1]
    } else {
      pos1b <- grep("# Reporting rate dev coffs", a)[1]
    }
    lagrangeLs <- strsplit(a[(pos1+2):(pos1b-2)],split="[[:blank:]]+")
    rowMax <- max(sapply(lagrangeLs, length)) 
    lagrangeLs <- do.call(rbind, lapply(lagrangeLs, function(x){ length(x) <- rowMax; as.numeric(x[2:rowMax]) }))  
    if(vsn > 1052)  # Other lambdas for augmented Lagrangian
    {
      pos1c <- grep("# Reporting rate dev coffs", a)[1]
      lagrangeLs_lkprof <- as.numeric(unlist(strsplit(a[(pos1b+2):(pos1c-2)],split="[[:blank:]]+")))[-1]
    }
  } else {
    lagrangeLs <- NA  
    lagrangeLs_lkprof <- NA
  }
  pos1 <- grep("# fishery selectivity", a)[1]; 
  selectivity <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  for (i in (pos1+2):(pos1+nfisheries)) {
    selectivity <- rbind(selectivity, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }
  pos1 <- grep("# natural mortality coefficient", a)[1]+2; Mbase <- as.double(a[pos1])
  
  pos1 <- grep("# effort deviation coefficients", a)[1]; pos1b <- pos1+nfisheries; effdevcoffs <- strsplit(a[(pos1+1):pos1b],split="[[:blank:]]+")
  rowMax <- max(sapply(effdevcoffs, length)) 
  effdevcoffs <- do.call(rbind, lapply(effdevcoffs, function(x){ length(x) <- rowMax; as.numeric(x[2:rowMax]) }))
  pos1 <- grep("# average catchability coefficients", a)[1]+3; meanqs <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Objective function value", a)[1]; obj <- as.double(a[pos1 + 1])
  pos1 <- grep("# The number of parameters", a)[1]; npars <- as.double(a[pos1 + 1])
  pos1 <- grep("# Maximum magnitude gradient value", a)[1]; gradient <- as.double(a[pos1 + 1])
  pos1 <- grep("# The von Bertalanffy parameters", a)[1]; 
     Lmin <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[1])
     Lmax <- as.numeric(unlist(strsplit(a[pos1+2],split="[[:blank:]]+"))[1])
     K    <- as.numeric(unlist(strsplit(a[pos1+3],split="[[:blank:]]+"))[1])
  pos1 <- grep("# Variance parameters", a)[1]; 
     growth_vars <- c(as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[1]),as.numeric(unlist(strsplit(a[pos1+2],split="[[:blank:]]+"))[1]))
  pos1 <- grep("# extra par for Richards", a)[1]; Richards <- as.double(a[pos1 + 1])
  pos1 <- grep("# age-class related parameters \\(age_pars\\)", a)[1]; 
  M_offsets <-  as.numeric(unlist(strsplit(a[pos1+4],split="[[:blank:]]+"))[-1])
  gr_offsets <- as.numeric(unlist(strsplit(a[pos1+5],split="[[:blank:]]+"))[-1])

  if(vsn > 1055)    # read in the maturity at length
  {
    pos1 <- grep("# maturity at length", a)[1]+1; maturity_len <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  } else {
    maturity_len <- NA  
  }
  
  
# Check for existence of new tagging inputs
  if(tsw != 0){
    if(tsw2 == 1){  # Load all tag reporting rate blocks
      par.obj <- list(pfl=pfl,
                  nages=nages,
                  afl=afl,ffl=ffl,tfl=tfl,trpfl=trpfl,trpgpfl=trpgpfl,trpacfl=trpacfl,treptarg=treptarg,treppen=treppen,
                  maturity=maturity,maturity_len=maturity_len,totpop=totpop,totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,rel_recruitment=rel_recruitment,
                  lagrangeLs=lagrangeLs,
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
                  maturity=maturity,maturity_len=maturity_len,totpop=totpop,totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,rel_recruitment=rel_recruitment,
                  lagrangeLs=lagrangeLs,
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
                  maturity=maturity,maturity_len=maturity_len,totpop=totpop,totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,rel_recruitment=rel_recruitment,
                  lagrangeLs=lagrangeLs,
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
                  maturity=maturity,maturity_len=maturity_len,totpop=totpop,totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,rel_recruitment=rel_recruitment,
                  lagrangeLs=lagrangeLs,
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
  return(par.obj)
  }
