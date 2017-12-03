#'
#' @importFrom dplyr filter
#' @importFrom tidyr gather 
#' @importFrom ggplot2 ggplot theme theme_set theme_bw labs geom_line ylab labs aes_string ylim xlab
# ' @importFrom scales 
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr "%>%" 
#'
plot_FAA<-function(rep,plot=TRUE,verbose=TRUE,Year=1952:2015,YLAB="Fishing mortality/year",onlyTotal=FALSE){
#library(dplyr)
#library(tidyr)
#library(ggplot2)
#library(scales)
#library("gridExtra")
theme_set(theme_bw())

#library(R4MFCL)

add.FAAs<-function(FAA){
  FAA<-as.data.frame(FAA)
  colnames(FAA)[1:20]<-paste0("",1:20)
  FAA$"11_15"<-rowMeans(FAA[,paste0("",11:15)])
  FAA$"16_20"<-rowMeans(FAA[,paste0("",16:20)])
  FAA$"11+"<-rowMeans(FAA[,paste0("",11:20)])
  FAA$"7+"<-rowMeans(FAA[,paste0("",7:20)])
  FAA$"7_10"<-rowMeans(FAA[,paste0("",7:10)])
  FAA$"4_6"<-rowMeans(FAA[,paste0("",4:6)])
  FAA$"1_3"<-rowMeans(FAA[,paste0("",1:3)])
  FAA$Year<-Year
  return(FAA)
}
#cat("L26\n") #;browser()
FAA<-rep$FbyAgeYr
#maxF<-max(FAA)
#cat("L27\n") #;browser()
FAAreg<-lapply(1:2,function(i){add.FAAs(rep$FatYrAgeReg[[1]][,,i])})
#cat("L30\n") #;browser()
FAA<-add.FAAs(FAA)
maxF<-max(max(FAA[,c("1_3","4_6","7+")]),sapply(FAAreg,function(x){max(x[,c("1_3","4_6","7+")])}))
#cat("L33\n");browser()
pl.all<-FAA%>%gather(Age,F,-Year)%>% filter(.,Age %in% c("1_3","4_6","7+"))%>% ggplot(aes_string(x="Year",y="F",color="Age"))
pl.all<-pl.all+geom_line(size=1)+ylab(YLAB)+labs(title="Total")+labs(colour="")+theme(legend.position="bottom")+ylim(0,maxF*1.05)
pl.reg<-FAAreg%>%lapply(.,function(z){
  pl<-z%>%gather(.,Age,F,-Year)%>% filter(.,Age %in% c("1_3","4_6","7+"))%>% ggplot(aes_string(x="Year",y="F",color="Age"))
  pl<-pl+geom_line(size=1)+ylab(YLAB)+xlab("")+labs(colour="")+theme(legend.position="none")+ylim(0,maxF*1.05)
  return(pl)
})
for(i in 1:2)pl.reg[[i]]<-pl.reg[[i]]+labs(title=paste0("Region ",i))
cat("L41\n") #;browser()

pl<-if(!onlyTotal){
  grid.arrange(pl.reg[[1]],pl.reg[[2]],pl.all,ncol=2)
  }else{
  pl.all
  }
if(plot)print(pl)

return(invisible(pl))

}