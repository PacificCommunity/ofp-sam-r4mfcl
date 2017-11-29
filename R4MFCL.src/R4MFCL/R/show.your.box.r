#' SJDM 8/7/2014 modifies Nicks swordfish code to be a (hopefully) relatively general way of ploting box and whiskers for grid summaries generated
#' by generate.grid.summaries.r
#' first factor is number of 1st character in current.grid.runs that indicates factor level
#' e.g. 001_H0W0M0T0C0 it would be 6, for X001_H0W0M0T0C0 it would be 7 etc.
#' See bottom of the script for examples of how to run from a stored file or an R object
#'
#' @importFrom utils write.table read.table
#' @importFrom grDevices windows rainbow savePlot dev.off
#' @importFrom graphics boxplot par axis
#' @importFrom stats aggregate quantile
show.your.box <- function(grid.summary.path="C:/Users/SamM/Desktop/grid.out.txt",
                          xaxs_nms=list(H=c("0.8", "0.65", "0.95"),
                                        W=c("20;100", "40;100"),
                                        M=c("Fixed mean = 0.25", "1"),
                                        T=c("2qtr", "1qtr"),
                                        C=c("ex. PH", "inc. PH")),
                           col_lst=list(H=c(rainbow(40)[24], NA, NA),
                                        W=c(rainbow(40)[24], NA),
                                        M=c(rainbow(40)[24], NA),
                                        T=c(rainbow(40)[24], NA),
                                        C=c(rainbow(40)[24], NA)),
                           xnams=c('Steepness', 'Size data weighting', 'Mortality',  'Tag mixing', 'CPUE'),
                           winsize=c(10,10),first.factor=7,response.vars=c("Fcurr.Fmsy","SBcurr.SBmsy"),
                           response.names=c("Fcurr/Fmsy", "SBcurr/SBmsy"),from.file="TRUE",grid.sum=grid.out,
                           fig.names=c("name1","name2"),plotlay=c(3,2))
{

    if(from.file == "TRUE")
    {
        grid.out <- read.table(grid.summary.path,header=TRUE,sep="")
    } else {
        grid.out <- grid.sum
    }
     
    N.axis <- length(xnams)

    current.grid.runs <- dimnames(grid.out)[[2]]    # Use grid runs from grid.out
    grid.criteria <- current.grid.runs # Initialise design matrix

    for(i in (1:N.axis*2 + first.factor - 2))
    {
        tmp.vector <- substring(current.grid.runs,i,i)
        grid.criteria <- cbind(grid.criteria, tmp.vector)
    }

    # Set up pointer for factor options
    run_type <- grid.criteria[,c(2:(N.axis+1))]
    tmp <- matrix(0,nrow=dim(run_type)[1],ncol=dim(run_type)[2])
    tmp[,] <- as.numeric(run_type[,])
    tmp <- tmp[,] + 1
    run_type <- tmp

    for(i in 1:length(response.vars))
    {
        tmp.plot <- grid.out[response.vars[i],]

        mgmt.out <- list(unlist(tmp.plot), run_type)
        names(mgmt.out)<- c(response.vars[i], "run_type")

        windows(winsize)
        par(mfrow=plotlay,mai=c(1,1,0.2,0.1))

              for(j in 1:N.axis){                  # Loop over factors
                  boxplot(mgmt.out[[1]] ~ mgmt.out[[2]][,j],  col=col_lst[[j]], xlab= xnams[j], ylab=response.names[i] , las=1, ylim=c(0, max(tmp.plot)) ,font.lab=2, cex.lab=1.2, xaxt='n')
                  axis(1, at=unique(mgmt.out[[2]][,j]), labels=xaxs_nms[[j]])
              }
        savePlot(paste(figdir,fig.names[i], sep="/"), type="png")
        dev.off()
    }
}


## YFT
## This method stores the grid summaries and then calls them up to make the plot
#generate.grid.summaries(grid.dir = "L:/yft/2014/assessment/Model_runs/grid",parfl = "12.par",
#                                    repfl = "plot-12.par.rep",out.dir="C:/Users/SamM/Desktop/grid.out.txt",writefile="TRUE")
#
#show.your.box(grid.summary.path="C:/Users/SamM/Desktop/grid.out.txt",
#                          xaxs_nms=list(H=c("0.8", "0.65", "0.95"),
#                                        W=c("20;100", "40;100"),
#                                        M=c("Fixed mean = 0.25", "Estimated"),
#                                        T=c("2qtr", "1qtr - fixed region pars"),
#                                        C=c("ex. PH", "inc. PH")),
#                           col_lst=list(H=c(rainbow(40)[24], NA, NA),
#                                        W=c(rainbow(40)[24], NA),
#                                        M=c(rainbow(40)[24], NA),
#                                        T=c(rainbow(40)[24], NA),
#                                        C=c(rainbow(40)[24], NA)),
#                           xnams=c('Steepness', 'Size data weighting', 'Mortality',  'Tag mixing', 'CPUE'),
#                           winsize=c(10,10),first.factor=7,response.vars=c("Fcurr.Fmsy","SBcurr.SBmsy"),
#                           response.names=c("Fcurr/Fmsy", "SBcurr/SBmsy"),from.file="TRUE",grid.sum=grid.out,
#                           fig.names=c("name1","name2"))
#
## YFT
## This method doesn't store the summaries and makes the plot straight from the R.object "grid.out"
#grid.out <- generate.grid.summaries(grid.dir = "L:/yft/2014/assessment/Model_runs/grid",parfl = "12.par",
#                                    repfl = "plot-12.par.rep",out.dir="C:/Users/SamM/Desktop/grid.out.txt",writefile="FALSE")
#
#show.your.box(grid.summary.path="NULL",
#                          xaxs_nms=list(H=c("0.8", "0.65", "0.95"),
#                                        W=c("20;100", "40;100"),
#                                        M=c("Fixed mean = 0.25", "Estimated"),
#                                        T=c("2qtr", "1qtr - fixed region pars"),
#                                        C=c("ex. PH", "inc. PH")),
#                           col_lst=list(H=c(rainbow(40)[24], NA, NA),
#                                        W=c(rainbow(40)[24], NA),
#                                        M=c(rainbow(40)[24], NA),
#                                        T=c(rainbow(40)[24], NA),
#                                        C=c(rainbow(40)[24], NA)),
#                           xnams=c('Steepness', 'Size data weighting', 'Mortality',  'Tag mixing', 'CPUE'),
#                           winsize=c(10,10),first.factor=6,response.vars=c("Fcurr.Fmsy","SBcurr.SBmsy"),
#                           response.names=c("Fcurr/Fmsy", "SBcurr/SBmsy"),from.file="FALSE",grid.sum=grid.out, # grid.sum needs to be whatever you call the output above
#                           fig.names=c("name1","name2"))





