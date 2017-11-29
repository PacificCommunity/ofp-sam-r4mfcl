#' @importFrom utils write.table
write.ini <-
function(ini.file,ini.obj, old.format=FALSE) {
##============================================================================
## by Simon Hoyle June 2008
##  revised, PK June 2011
##  revised, YT May 2017 for 2 sex model
##============================================================================
  o <- ini.obj
  nSp<-ifelse(is.null(o$nSp),1,o$nSp)
  #a1 <- character()
  con <- file(ini.file,open="w")
  on.exit(close(con))

  if(with(ini.obj,exists("version")) & !old.format) {
    writeLines(c("# ini version number",o$version),con)
  }
  if(length(o$nages)==1){
    writeLines(c("# number of age classes",o$nages),con)
  }else{
    writeLines(c("# number of age classes",o$nages[1]),con)
    writeLines(c("# number of age classes (for female)",o$nages[2]),con)
  }

  if(!is.null(o$reg.flg)){
    writeLines("# region_flags",con)
    write.table(o$reg.flg,con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(!is.null(o$sp.flg)){
    writeLines("#_species_flags",con)
    write.table(o$sp.flg,con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(with(o,exists("tag.fish.rep"))) {
    writeLines("# tag fish rep",con)
    write.table(o$tag.fish.rep,con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(with(o,exists("grpflags"))) {
    writeLines("# tag fish rep group flags",con)
    write.table(o$grpflags,con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(with(o,exists("activeflags"))) {
    writeLines("# tag_fish_rep active flags",con)
    write.table(o$activeflags,con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(with(o,exists("reptarget"))) {
    writeLines("# tag_fish_rep target",con)
    write.table(o$reptarget,con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(with(o,exists("reppenalty"))) {
    writeLines("# tag_fish_rep penalty",con)
    write.table(o$reppenalty,con,quote=F,append=T,row.names=F,col.names=F)
  }
  if(length(o$nages)==1){
    writeLines(c("# maturity at age",paste(o$mat,collapse=" ")),con)
  }else{
    writeLines("# maturity at age of male/species1",con)
    writeLines(paste(o$mat[1,],collapse=" "),con)
    writeLines("# maturity at age of female/species2",con)
    writeLines(paste(o$mat[2,],collapse=" "),con)
  }
  if(length(o$nages)==1){
    writeLines(c("# natural mortality (per year)",o$M),con)
  }else{
    writeLines(c("# natural mortality (per year)",o$M),con)
  }

  if(with(o,exists("movemap"))) writeLines(c("# movement map",paste(o$movemap,collapse=" ")),con)

  if(with(o,exists("diffcoffs"))) writeLines("# diffusion coffs (per year)",con)
  write.table(o$diffcoffs,con,quote=F,append=T,row.names=F,col.names=F)


  writeLines("# age_pars",con)
  nSp<-ifelse(is.null(o$nSp),1,o$nSp)
  if(nSp==1){
  write.table(o$age_pars,con,quote=F,append=T,row.names=F,col.names=F)
  }else{
    writeLines("# age_pars",con)
    write.table(o$age_pars[1:10,],con,quote=F,append=T,row.names=F,col.names=F)
    writeLines("# age_pars for female",con)
    write.table(o$age_pars[11:20,],con,quote=F,append=T,row.names=F,col.names=F)
  }

  if(nSp==1){
    writeLines(c("# recruitment distribution by region",paste(o$recbyreg,collapse=" ")),con)
  }else{
    writeLines(c("# recruitment distribution by region",paste(o$recbyreg[1,],collapse=" ")),con)
    writeLines(c("# recruitment distribution by region for female",paste(o$recbyreg[2,],collapse=" ")),con)
  }
  if(length(o$nages)==1){
    writeLines(c("# The von Bertalanffy parameters",
               "# Initial  lower bound  upper bound",
               "# ML1",
                paste(paste(o$VBLmin,collapse=" ")),
               "# ML2",
                paste(paste(o$VBLmax,collapse=" ")),
               "# K (per year)",
                paste(paste(o$VBK,collapse=" ")),
               "# Length-weight parameters",
                paste(paste(o$LW,collapse=" ")),
               "# sv(29)",
                o$steepness,
               "# Generic SD of length at age",
                paste(paste(o$sdLatA,collapse=" ")),
               "# Length-dependent SD",
                paste(paste(o$Ldep_sd,collapse=" ")),
               "# The number of mean constraints",
                paste(paste(o$Nmeanconstr,collapse=" "))), con)
  }else{
    writeLines(c("# The von Bertalanffy parameters of male/species1",
               "# Initial  lower bound  upper bound",
               "# ML1",
                paste(paste(o$VBLmin[1,],collapse=" ")),
               "# ML2",
                paste(paste(o$VBLmax[1,],collapse=" ")),
               "# K (per year)",
                paste(paste(o$VBK[1,],collapse=" ")),
               "# The von Bertalanffy parameters of female/species2",
               "# Initial  lower bound  upper bound",
               "# ML1",
                paste(paste(o$VBLmin[2,],collapse=" ")),
               "# ML2",
                paste(paste(o$VBLmax[2,],collapse=" ")),
               "# K (per year)",
                paste(paste(o$VBK[2,],collapse=" ")),
               "# Length-weight parameters",
                paste(paste(o$LW[1,],collapse=" ")),
               "# sv(29), steepness",
                o$steepness[1],
               "# Length-weight parameters",
                paste(paste(o$LW[2,],collapse=" ")),
               "# sv(29), steepness",
                o$steepness[2],
               "# Generic SD of length at age",
                paste(paste(o$sdLatA[1,],collapse=" ")),
               "# Length-dependent SD",
                paste(paste(o$Ldep_sd[1,],collapse=" ")),
                "# Generic SD of length at age",
                paste(paste(o$sdLatA[2,],collapse=" ")),
               "# Length-dependent SD",
                paste(paste(o$Ldep_sd[2,],collapse=" ")),
               "# The number of mean constraints",
                paste(paste(o$Nmeanconstr,collapse=" "))), con)
  }
}
