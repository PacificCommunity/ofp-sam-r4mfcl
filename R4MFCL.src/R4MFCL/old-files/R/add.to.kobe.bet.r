# Adds points from uncertainty grid identified by grid.critera
add.to.kobe.bet <- function(vars=grid.criteria[,2],grid.obj=grid.out){
  if(length(unique(vars))==2)  {
    my.cols <- c("black","white")
  } else {
    my.cols <- gray(seq(0,1,length=length(unique(vars))))
  }
  for(i in 1:length(unique(vars))){
    runs <- match(vars,unique(vars)[i])
    F.Fmsy <- grid.obj["Fcurr.Fmsy",!is.na(runs)]
    SB.SBmsy <- grid.obj["SBcurr.SBmsy",!is.na(runs)]
    points(SB.SBmsy,F.Fmsy, col=my.cols[i], pch=16, cex=1)
  }
}

