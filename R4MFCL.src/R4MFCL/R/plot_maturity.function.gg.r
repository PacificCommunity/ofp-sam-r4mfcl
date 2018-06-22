#' Making plot of maturity at age
#' 
#' @param inifiles string or vector of string of filename(s) of ini file 
#' @param modlab string or vector of string of model name(s) to be used in legend
#' @importFrom graphics legend plot lines
#' @export
plot_maturity.function.gg <- function(inifiles,
                                    modlab = c("Base","Base"),verbose=TRUE)
{
# Plot Mat-at-age from some different runs
# Plots basecase last
# something different in read.ini that requires the it to be done within the function
    Mat <- list()
    for(i in 1:length(inifiles))
    {
        ini<-read.ini(unlist(inifiles[i]))
        if(is.null(ini$version) | ini$version<1002){
        	cat("L18\n") #;browser()
          Mat[[i]] <-ini$mat
        }else if(ini$version>=1002){
        		if(verbose)cat("L20 ;\n") #;browser()
            #if(dim(ini$sp.flg)[1]>2)stop("currntly only single species with 2sex or singel sex model is supported")
            if(is.null(ini$nSp) || ini$nSp==1){
            	Mat[[i]] <-ini$mat
            	cat("L25\n") #;browser()
            }else{
            	Mat[[i]] <-ini$mat[which(ini$sp.flg[,2]==1),]
            	#cat("L26\n");browser()
            }
        }else{
        	stop("Error : L23 in plot_maturity.function.gg.r")
        }
        cat("Mat[[",i,"]]\n");print(Mat[[i]])
    }
    # get the maximum value across the runs
    maxy <- max(unlist(lapply(Mat,max)))
		
    cat("L35 length(inifiles)=",length(inifiles),"\n")
    # if only one series then just make a plot
    if(length(inifiles)==1)
    {
        plot(1:length(Mat[[1]]), Mat[[1]], type="n", ylab="Reproductive output", xlab="Age class", ylim=c(0,maxy),las=1)
        lines(1:length(Mat[[1]]), Mat[[1]], lwd=2, col="black")
    }else{
    	plot(1:length(Mat[[1]]), Mat[[1]], type="n", ylab="Reproductive output", xlab="Age class", ylim=c(0,maxy),las=1)

    	for(i in 2:length(inifiles))
    	{
      	  lines(1:length(Mat[[i]]), Mat[[i]], lwd=2, col=i)
    	}
			cat("L48\n")#;browser()
    	lines(1:length(Mat[[1]]), Mat[[1]], lwd=2, col="black")
    	legend("topright", lwd=2, col=1:length(Mat), lty=c(1,1), legend=modlab,bty="n")
    }
}
