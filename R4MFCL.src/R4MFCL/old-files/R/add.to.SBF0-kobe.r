add.to.SBF0kobe <- function(vars=grid.criteria[,2],grid.obj=grid.out,SBopt="latest")
{
        if(length(unique(vars))==2)
        {
        my.cols <- c("black","white")
        } else
        {
    my.cols <- gray(seq(0,1,length=length(unique(vars))))
        }
    SB <- ifelse(SBopt=="latest","SBlatest.SBF0","SBcurr.SBF0")
    
    for(i in 1:length(unique(vars)))
    {
    runs <- match(vars,unique(vars)[i])
    y <- grid.obj["Fcurr.Fmsy",!is.na(runs)]
    x <- grid.obj[SB,!is.na(runs)]
    points(x,y, col=1, bg=my.cols[i], pch=21, cex=1)
    }
}
