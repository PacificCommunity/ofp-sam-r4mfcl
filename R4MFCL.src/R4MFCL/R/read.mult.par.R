 read.mult.par <-
function(par.file, num_spp = 1) {
  # by Simon Hoyle June 2008
  # This is only partly built. Needs to be extended so it gets the whole par file into an object. Then do the same for write.par...
  # Nick : added sections explicitly for # tag flags; # tag fish rep; # tag fish rep group flags; # tag_fish_rep active flags; # tag_fish_rep target; # tag_fish_rep penalty
  # Nick 25 November 2011: extended to read in multi-species parameters
  a <- readLines(par.file)
  pfl <- as.numeric(unlist(strsplit(a[2],split="[[:blank:]]+"))[-1])
  vsn <- pfl[200]

  multspp_pars <- list()         # A list element for each species being a list of the multi-spp parameters
  pos1 <- grep("# The number of age classes",a)
  multspp_pars[[1]] <- list()
  multspp_pars[[1]]$nages <- as.numeric(a[(pos1+1)])
  if(num_spp > 1){
    for(i in 2:num_spp){
      pos1 <- grep("# Multi-species the number of age classes",a)
      multspp_pars[[i]] <- list()
      multspp_pars[[i]]$nages <- as.numeric(a[(pos1+(i-1))])
    }
  }
  pos1 <- grep("# age flags",a)
  afl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  if(num_spp > 1){
    pos1 <- grep("# age flags",a)
    multspp_pars[[1]]$afl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    pos1 <- grep("# multi-species age flags",a)
    for(i in 2:num_spp){
      multspp_pars[[i]]$afl <- as.numeric(unlist(strsplit(a[(pos1+(i-1))],split="[[:blank:]]+"))[-1])
    }
  }
  pos1 <- grep("fish flags",a) ; 
# Check for version
  if(vsn > 1047)   pos1 <- pos1[1]   # Version 1048 has historical flags  
  pos2 <- grep("tag flags",a) ;
  if(length(pos2)==0)   pos2 <- grep("# region control flags",a) ;
  ffl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  for (i in (pos1+2):(pos2-1)) {
    ffl <- rbind(ffl, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }
  nfisheries <- dim(ffl)[1]
# Check if there are the new tag report sections in the par file
  tsw <- 0  #Default switch setting on tag parameters to zero
  tsw2 <- 0  #Default switch setting on tag parameters to zero
  if(length(as.numeric(grep("# tag fish rep",a))) > 0){
#   set the switch on for existence of tagging reporting parameters
    tsw <- 1
#   load block of tag flags
    pos1 <- pos2 ; pos2 <- min(grep("# tag fish rep",a))
    tfl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    for (i in (pos1+2):(pos2-1)) {
      tfl <- rbind(tfl, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }
#   load block of tag fish rep
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
  } else {   # Tag reporting rate parameter blocks - just load tag flags
#   load block of tag flags
    pos1 <- pos2 ; pos2 <- grep("# region control flags",a)
    tfl <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
    for (i in (pos1+2):(pos2-1)) {
      tfl <- rbind(tfl, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }
  }
  pos1 <- grep("# percent maturity", a)[1]+1;
  multspp_pars[[1]]$maturity <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  if(num_spp > 1){  #######      read #multi-species percent maturity
    for(i in 2:num_spp){
      pos1 <- grep("#multi-species percent maturity",a)[1]+1
      multspp_pars[[i]]$maturity <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
    }
  }
  pos1 <- grep("# total populations scaling parameter", a)[1]+1; multspp_pars[[1]]$totpop <- as.double(a[pos1])
  if(num_spp > 1){  #######      read #multi-species total populations scaling parameter
    for(i in 2:num_spp){
      pos1 <- grep("# multi-species total populations scaling parameter",a)[1]+1
      multspp_pars[[i]]$totpop <- as.double(a[pos1])
    }
  }

  pos1 <- grep("# implicit total populations scaling parameter", a)[1]+1; totpop_implicit <- as.double(a[pos1])
  pos1 <- grep("# rec init pop level difference", a)[1]+1; rec_init <- as.double(a[pos1])
  pos1 <- grep("# recruitment times", a)[1]+1; rectimes <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# relative recruitment", a)[1]+2; multspp_pars[[1]]$rel_recruitment <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  if(num_spp > 1){  #######      read #multi-species relative recruitment
    for(i in 2:num_spp){
      pos1 <- grep("# multi-species relative recruitment",a)[1]+1
      multspp_pars[[i]]$rel_recruitment <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
    }
  }

  pos1 <- grep("# fishery selectivity", a)[1]+1; 
  selectivity <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  for (i in (pos1+1):(pos1+nfisheries-1)) {
    selectivity <- rbind(selectivity, as.numeric(unlist(strsplit(a[i],split="[[:blank:]]+"))[-1]))
    }
  pos1 <- grep("# natural mortality coefficient", a)[1]+2;
  multspp_pars[[1]]$Mbase <-  as.double(a[pos1])
  if(num_spp > 1){  #######      read #multi-species natural mortality coefficient
    for(i in 2:num_spp){
      pos1 <- grep("#multi-species natural mortality coefficient",a)[1]+1
      multspp_pars[[i]]$Mbase <-  as.double(a[pos1])
    }
  }
  pos1 <- grep("# effort deviation coefficients", a)[1]; pos1b <- pos1+nfisheries; effdevcoffs <- strsplit(a[(pos1+1):pos1b],split="[[:blank:]]+")
  rowMax <- max(sapply(effdevcoffs, length)) 
  effdevcoffs <- do.call(rbind, lapply(effdevcoffs, function(x){ length(x) <- rowMax; as.numeric(x[2:rowMax]) }))
  pos1 <- grep("# average catchability coefficients", a)[1]+3; meanqs <- as.numeric(unlist(strsplit(a[pos1],split="[[:blank:]]+"))[-1])
  pos1 <- grep("# Objective function value", a)[1]; obj <- as.double(a[pos1 + 1])
  pos1 <- grep("# The number of parameters", a)[1]; npars <- as.double(a[pos1 + 1])
  pos1 <- grep("# Maximum magnitude gradient value", a)[1]; gradient <- as.double(a[pos1 + 1])


  # age-class related parameters (age_pars)
  pos1 <- grep("# age-class related parameters ",a) + 3
  pos <- pos1
  if(num_spp > 1){  
    pos1_2 <- min(grep("#multi-species age-class related parameters ",a)) - 2
  } else {
    pos1_2 <- grep("# region parameters",a) - 2
  }
  remi <- c(pos1:pos1_2)
  tmp <- as.numeric(unlist(strsplit(a[remi[1]],split="[[:blank:]]+"))[-1])
  for (i in 2:length(remi)) {
    tmp <- rbind(tmp, as.numeric(unlist(strsplit(a[remi[i]],split="[[:blank:]]+"))[-1]))
  }
  multspp_pars[[1]]$age_pars <-  tmp
  if(num_spp > 1){  
    pos <- grep("#multi-species age-class related parameters ",a) + 2
    for(i in 2:num_spp){
      pos1 <- pos[i-1]
      if(i == num_spp){
        pos1_2 <- grep("# region parameters",a) - 2
      } else {
        pos1_2 <- pos[i] - 2
      }
      remi <- c(pos1:pos1_2)
      tmp <- as.numeric(unlist(strsplit(a[remi[1]],split="[[:blank:]]+"))[-1])
      for (ii in 2:length(remi)) {
        tmp <- rbind(tmp, as.numeric(unlist(strsplit(a[remi[ii]],split="[[:blank:]]+"))[-1]))
      }
      multspp_pars[[i]]$age_pars <-  tmp
    }
  }
  rm(tmp,pos,pos1,pos1_2)

# region parameters
  pos1 <- grep("# region parameters",a) + 1
  pos1_2 <- pos1 + 9
  remi <- c(pos1:pos1_2)
  tmp <- as.numeric(unlist(strsplit(a[remi[1]],split="[[:blank:]]+"))[-1])
  for (ii in 2:length(remi)) {
    tmp <- rbind(tmp, as.numeric(unlist(strsplit(a[remi[ii]],split="[[:blank:]]+"))[-1]))
  }
  nreg <- dim(tmp)[[2]] / num_spp
  multspp_pars[[1]]$region_pars <-  tmp[,c(1:nreg)]
  if(num_spp > 1){
    for(i in 2:num_spp){
      multspp_pars[[i]]$region_pars <-  
       tmp[,c(((nreg*(num_spp-1))+1):((nreg*(num_spp-1))+nreg))]
    }
  }
# species_flags
  if(num_spp > 1){
    pos1 <- grep("# species flags",a) + 1
    pos1_2 <- pos1 + 1
    remi <- c(pos1:pos1_2)
    tmp <- as.numeric(unlist(strsplit(a[remi[1]],split="[[:blank:]]+"))[-1])
    for (ii in 2:length(remi)) {
      tmp <- rbind(tmp, as.numeric(unlist(strsplit(a[remi[ii]],split="[[:blank:]]+"))[-1]))
    }
    for(i in 1:num_spp){
      multspp_pars[[i]]$species_flags <- tmp
    }
    rm(tmp)
  }
#
  pos1 <- grep("# The von Bertalanffy parameters", a)[1]; 
  multspp_pars[[1]]$Lmin <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+")))
  multspp_pars[[1]]$Lmax <- as.numeric(unlist(strsplit(a[pos1+2],split="[[:blank:]]+")))
  multspp_pars[[1]]$K    <- as.numeric(unlist(strsplit(a[pos1+3],split="[[:blank:]]+")))
  if(num_spp > 1){  #######      read #multi-species von bertalanffy parameters
    for(i in 2:num_spp){
      pos1 <- grep("# extra par for Richards",a)[1]
      multspp_pars[[i]]$Lmin <- as.numeric(unlist(strsplit(a[pos1+2],split="[[:blank:]]+")))
      multspp_pars[[i]]$Lmax <- as.numeric(unlist(strsplit(a[pos1+3],split="[[:blank:]]+")))
      multspp_pars[[i]]$K    <- as.numeric(unlist(strsplit(a[pos1+4],split="[[:blank:]]+")))
    }
  }
  pos1 <- grep("# Variance parameters", a)[1]; 
  multspp_pars[[1]]$sdLatA <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+")))
  multspp_pars[[1]]$Ldep_sd <- as.numeric(unlist(strsplit(a[pos1+2],split="[[:blank:]]+"))) 
  if(num_spp > 1){  #######      read multi-spp parameters
    multspp_pars[[1]]$sdLatA <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+")))
    multspp_pars[[1]]$Ldep_sd <- as.numeric(unlist(strsplit(a[pos1+2],split="[[:blank:]]+"))) 
    for(i in 2:num_spp){
      multspp_pars[[i]]$sdLatA <- as.numeric(unlist(strsplit(a[pos1+((i*2)-1)],split="[[:blank:]]+")))
      multspp_pars[[i]]$Ldep_sd <- as.numeric(unlist(strsplit(a[pos1+((i*2))],split="[[:blank:]]+"))) 
    }
  }
  pos1 <- grep("# extra par for Richards", a)[1]; 
  multspp_pars[[1]]$Richards <- as.double(a[pos1 + 1])
  if(num_spp > 1){  #######      read multi-spp parameters
    for(i in 2:num_spp){
      multspp_pars[[i]]$Richards <- as.double(a[pos1 + ((i-1)*4)+1])
    }
  }
  pos1 <- grep("# Seasonal growth parameters", a)[1]; 
  multspp_pars[[1]]$season_grw_pars <- as.numeric(unlist(strsplit(a[pos1+1],split="[[:blank:]]+"))[-1])
  if(num_spp > 1){  #######      read multi-spp parameters
    pos1 <- grep("# Extra multi-species growth parameters", a)[1]; 
    for(i in 2:num_spp){
      multspp_pars[[i]]$season_grw_pars <- as.numeric(unlist(strsplit(a[pos1+1+(i-2)],split="[[:blank:]]+"))[-1])
    }
  }

  pos1 <- grep("# age-class related parameters \\(age_pars\\)", a)[1]; 
  multspp_pars[[1]]$M_offsets <-  as.numeric(unlist(strsplit(a[pos1+4],split="[[:blank:]]+"))[-1])
  multspp_pars[[1]]$gr_offsets <- as.numeric(unlist(strsplit(a[pos1+5],split="[[:blank:]]+"))[-1])
  if(num_spp > 1){  #######      read multi-spp parameters
    pos1 <- grep("#multi-species age-class related parameters \\(age_pars\\), species:",a)
    for(i in 2:num_spp){
      multspp_pars[[i]]$M_offsets <-  as.numeric(unlist(strsplit(a[pos1[i-1]+3],split="[[:blank:]]+"))[-1])
      multspp_pars[[i]]$gr_offsets <- as.numeric(unlist(strsplit(a[pos1[i-1]+4],split="[[:blank:]]+"))[-1])
    }
  }
  
# Check for existence of new tagging inputs
  if(tsw != 0){
    if(tsw2 == 1){  # Load all tag reporting rate blocks
      par.obj <- list(pfl=pfl,multspp_pars=multspp_pars,                  
                  afl=afl,ffl=ffl,tfl=tfl,trpfl=trpfl,trpgpfl=trpgpfl,trpacfl=trpacfl,treptarg=treptarg,treppen=treppen,
                  totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,
                  selectivity=selectivity,
                  effdevcoffs=effdevcoffs,
                  meanqs=meanqs,
                  obj=obj,
                  npars=npars,
                  gradient=gradient,                                    
                  rem=a[pos2:length(a)])
    } else {      # Just load up to (including) tag rep active flags
      par.obj <- list(pfl=pfl,multspp_pars=multspp_pars,                  
                  afl=afl,ffl=ffl,tfl=tfl,trpfl=trpfl,trpgpfl=trpgpfl,trpacfl=trpacfl,
                  totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,
                  selectivity=selectivity,
                  effdevcoffs=effdevcoffs,
                  meanqs=meanqs,
                  obj=obj,
                  npars=npars,
                  gradient=gradient,                                    
                  rem=a[pos2:length(a)])
    }    
  } else {    # Just load up to and including tag flags
    par.obj <- list(pfl=pfl,multspp_pars=multspp_pars,                  
                  afl=afl,ffl=ffl,tfl=tfl,
                  totpop_implicit=totpop_implicit,
                  rec_init=rec_init,rectimes=rectimes,
                  selectivity=selectivity,
                  effdevcoffs=effdevcoffs,
                  meanqs=meanqs,
                  obj=obj,
                  npars=npars,
                  gradient=gradient,                                    
                  rem=a[pos2:length(a)])
  }  
  return(par.obj)
  }
