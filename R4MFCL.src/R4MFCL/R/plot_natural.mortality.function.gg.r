#' Making plot of natural mortality using ggplot
#' @param repfiles list of outputs of read.rep
#' @param modlab vector of stringof labels to be used in legend 
#  ' @importFrom graphics matplot legend matlines
#' @importFrom ggplot2 ggplot geom_line
#' @export
plot_natural.mortality.function.gg <- function(repfiles=list(read.rep(baserep),
read.rep(baserep)),modlab = c("Base","Base"),verbose=TRUE,plot=TRUE)
{
# Plot M-at-age from some different runs
# Plots basecase last
	if(verbose)cat("L12 ;\n")
	require(ggplot2)
  M <- list()
  nSps<-vector(mode="numeric",length=length(repfiles))
  for(i in 1:length(repfiles))
  {
    M[[i]] <- repfiles[[i]]$MatAge
    nSps[i]<-ifelse(is.null(repfiles[[i]]$nSp),1,repfiles[[i]]$nSp)
  }
	if(verbose)cat("L21 ;\n")

  # get the maximum value across the runs
  maxy <- max(unlist(lapply(M,max)))
  if(verbose)cat("L25 ;\n")
  # if only one series then just make a plot
  if(length(repfiles)==1)
  {
    if(nSps[1]==1){
    #  plot(1:length(M[[1]]), M[[1]], type="n", ylab="Natural mortality", xlab="Age class", ylim=c(0,maxy), las=1)
    #  lines(1:length(M[[1]]), M[[1]], lwd=2, col="black")
      M.df<-data.frame(M=M[[1]],age=1:length(M[[1]]))
      pl<-ggplot(data=M.df,aes(x=age,y=M))+geom_line()
    }else{
      matplot(1:dim(M[[1]])[2], t(M[[1]]), type="n", ylab="Natural mortality", xlab="Age class", ylim=c(0,maxy), las=1)
      matlines(x=1:dim(M[[1]])[2], t(M[[1]]), lwd=2, col=1:dim(M[[1]])[1])
      if( nSps[1]==2 & sum(repfiles[[1]]$spSexPtr)==1){
        modlab[which(repfiles[[1]]$spSexPtr==1)]<-"Female"
        modlab[which(repfiles[[1]]$spSexPtr==0)]<-"Male"
      }
      legend("topright", lwd=2, col=1:dim(M[[1]])[1], lty=c(1,1), legend=modlab,bty="n")
    }
  }else{
  	M.df<-list()
  	for(i in 1:length(repfiles)){
  		M.df[[i]]<-if(nSps[i]==1){
  			data.frame(M=M[[i]],age=1:length(M[[i]]),modlab=modlab[i])
  		}else{
  			modlabFemale<-paste0(modlab[i],"_Female")
        modlabMale<-paste0(modlab[i],"_Male")
  			Mmale<-data.frame(M=M[[i]][1,],age=1:length(M[[i]][1,]),modlab=modlabMale)
  			Mfemale<-data.frame(M=M[[i]][2,],age=1:length(M[[i]][2,]),modlab=modlabFemale)
  			M.df[[i]]<-rbind(Mmale,Mfemale)
  		}
  	}
  	if(verbose)cat("L56 ;\n")
  	M.df<-do.call("rbind",M.df)
  	if(verbose)cat("L58 ; \n") #;browser()
    #plot(1:length(M[[1]]), M[[1]], type="n", ylab="Natural mortality", xlab="Age class", ylim=c(0,maxy), las=1)
    pl<-ggplot(data=M.df,aes(x=age,y=M,col=modlab))+geom_line()
    #for(i in 2:length(repfiles))
    #{
    #  lines(1:length(M[[i]]), M[[i]], lwd=2, col=i)
    #}

    #lines(1:length(M[[1]]), M[[1]], lwd=2, col="black")
    #legend("topright", lwd=2, col=1:length(M), lty=c(1,1), legend=modlab,bty="n")
  }
  if(plot)plot(pl)
  return(pl)
}
