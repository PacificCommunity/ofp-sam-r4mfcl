#'
# ' @importFrom dplyr filter
#' @importFrom tidyr gather_ 
#' @importFrom ggplot2 ggplot  theme_set theme_bw labs geom_line ylab labs aes_string xlab facet_wrap
# ' @importFrom scales 
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr "%>%" 
#' @export
#'
plot_FAANAAbyDecade2<-function(rep,plot=TRUE,verbose=TRUE,Year=1952:2015,col=heat.colors(7),
YLAB="average N@A by decade"){
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
#  FAA$"11_15"<-rowMeans(FAA[,paste0("",11:15)])
#  FAA$"16_20"<-rowMeans(FAA[,paste0("",16:20)])
#  FAA$"11+"<-rowMeans(FAA[,paste0("",11:20)])
#  FAA$"7+"<-rowMeans(FAA[,paste0("",7:20)])
#  FAA$"7_10"<-rowMeans(FAA[,paste0("",7:10)])
#  FAA$"4_6"<-rowMeans(FAA[,paste0("",4:6)])
#  FAA$"1_3"<-rowMeans(FAA[,paste0("",1:3)])
  FAA$Year<-floor(Year/10)*10
  return(FAA)
}
cat("L26\n") #;browser()
FAA<-rep$FbyAgeYr
#cat("L27\n") #;browser()
#FAAreg<-lapply(1:2,function(i){add.FAAs(rep$FatYrAgeReg[[1]][,,i])})

cat("L30\n") #;browser()
FAA<-add.FAAs(FAA)
#Fdecade<-list()
decades<-c(1950,1960,1970,1980,1990,2000,2010)
Fdecade<-t(sapply(decades,function(y){colMeans(FAA[FAA$Year==y,1:20])}))
rownames(Fdecade)<-paste(decades)
Fdecade<-as.data.frame(Fdecade)
Fdecade$decade<-rownames(Fdecade)
Fdecade.long<-Fdecade %>%gather_(., key_col="Age",gather_cols=paste(1:20),value_col="F")
Fdecade.long$Age<-as.numeric(paste(Fdecade.long$Age))
pl.F<-Fdecade.long %>%ggplot(.,aes(x=Age,y=F))+geom_line()+xlab("")+ylab("Average F by decade)")
#pl<-pl+geom_line()
pl.F<-pl.F+facet_wrap(~decade,ncol=1)
cat("L37\n") #;browser()
#par(mfrow=c(2,1))
#matplot(Fdecade,lty=1:7,col=col,lwd=3,ylab="Average fishing mortality at age by decade",type="l")
#legend(legend=paste(decades),lty=1:7,col=col,lwd=rep(3,7),"topright")
#cat("L34\n");browser()

#pl.all<-FAA%>%gather(Age,F,-Year)%>% filter(.,Age %in% c("1_3","4_6","7+"))%>% ggplot(aes(x=Year,y=F,color=Age))+geom_line(size=1)+ylab("Fishing mortality/year")+labs(title="Total")
#pl.reg<-FAAreg%>%lapply(.,function(z){
#  pl<-z%>%gather(.,Age,F,-Year)%>% filter(.,Age %in% c("1_3","4_6","7+"))%>% ggplot(aes(x=Year,y=F,color=Age))+geom_line(size=1)+ylab("Fishing mortality/year")
#  return(pl)
#})
#for(i in 1:2)pl.reg[[i]]<-pl.reg[[i]]+labs(title=paste0("Region ",i))
#cat("L41\n") #;browser()
#pl<-grid.arrange(pl.reg[[1]],pl.reg[[2]],pl.all,ncol=2)
#if(plot)print(pl)

###########################################
# Numbers at age
NAA<-as.data.frame(apply(rep$NatYrAgeReg,1:2,"sum"))
colnames(NAA)[1:20]<-paste0("",1:20)
NAA$Year<-floor(Year/10)*10
Ndecade<-t(sapply(decades,function(y){colMeans(NAA[NAA$Year==y,1:20])}))
rownames(Ndecade)<-paste(decades)
Ndecade<-as.data.frame(Ndecade)
Ndecade$decade<-rownames(Ndecade)
cat("L69\n") #;browser()
Ndecade.long<-Ndecade %>%gather_(., key_col="Age",gather_cols=paste(1:20),value_col="F")
Ndecade.long$Age<-as.numeric(paste(Ndecade.long$Age))
pl.N<-Ndecade.long %>%ggplot(.,aes_string(x="Age",y="F"))+geom_line()+xlab("")+ylab(YLAB)
#pl<-pl+geom_line()
pl.N<-pl.N+facet_wrap(~decade,ncol=1)
pl<-grid.arrange(pl.N, pl.F,
             ncol = 2)
cat("L73\n") # ;browser()


return(invisible(pl))

}