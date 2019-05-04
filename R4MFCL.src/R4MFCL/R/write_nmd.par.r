 write_nmd.par <-
function(par.file,par.obj) {
  # by Simon Hoyle June 2008
  # SDH 29/01/09 changed order of obj and par to be consistent with other
  # SDH 29/01/09 fixed bug where p$ was missing from p$nages
  # NMD 22/06/12 - allow instance of no tag flags or tag pars at all (e.g. striped marlin)
  p <- par.obj
  con <- file(par.file,open="wt")
  a <- c("# The parest_flags",paste("",formatC(p$pfl,format="d"),collapse=""))
  a <- c(a,"","# The number of age classes",p$nages,"# age flags",paste("",formatC(p$afl,format="d"),collapse=""),"")
  if(p$pfl[200] > 1054) a <- c(a,"871")
  a <- c(a,"# fish flags")
  writeLines(a,con)
#  apply(ffl,1,paste,collapse=" ")
  write.table(p$ffl, con, quote=F, sep=" ", row.names=rep("",dim(p$ffl)[1]),col.names=F,append=T)
  if("tfl" %in% names(p)){
    if(p$pfl[200] > 1054) writeLines("872",con)
    writeLines("# tag flags",con)
    write.table(p$tfl, con, quote=F, sep=" ", row.names=rep("",dim(p$tfl)[1]),col.names=F,append=T)
  }
# Check for existence of new tag reporting rate parameters
  if("trpfl" %in% names(p)){
    if(p$pfl[200] > 1054){
      writeLines("1872",con)
      writeLines("",con)
    } 
    writeLines("# tag fish rep",con)
    write.table(p$trpfl, con, quote=F, sep=" ", row.names=rep("",dim(p$trpfl)[1]),col.names=F,append=T)
    writeLines(" ",con)
    writeLines("# tag fish rep group flags",con)
    write.table(p$trpgpfl, con, quote=F, sep=" ", row.names=rep("",dim(p$trpgpfl)[1]),col.names=F,append=T)
    writeLines("# tag_fish_rep active flags",con)
    write.table(p$trpacfl, con, quote=F, sep=" ", row.names=rep("",dim(p$trpacfl)[1]),col.names=F,append=T)
  }
  if("treptarg" %in% names(p)){
    writeLines("# tag_fish_rep target",con)
    write.table(p$treptarg, con, quote=F, sep=" ", row.names=rep("",dim(p$treptarg)[1]),col.names=F,append=T)
    writeLines(" ",con)
    writeLines("# tag_fish_rep penalty",con)
    write.table(p$treppen, con, quote=F, sep=" ", row.names=rep("",dim(p$treppen)[1]),col.names=F,append=T)
    writeLines(" ",con)
  }
  if(p$pfl[200] > 1054) writeLines("2872",con)
  writeLines(p$rem,con)
  close(con)
  }
