#' Plot  of F at Age
#' @param rep output.of read.rep
#' @param plot LOGICAL if plot be sent to graphics device
#' @param verbose LOGICAL if TRUE, make verbose outputs
#' @param Year range of year
#' @param YLAB label for y-axis
#' @param onlyTotal LOGICAL if TRUE both F@A of whole population and F@A by region be made, otherwise F@A of whole population only be made
#' @importFrom dplyr filter
#' @importFrom tidyr gather 
#' @importFrom ggplot2 ggplot theme theme_set theme_bw labs geom_line ylab labs aes_string ylim xlab
# ' @importFrom scales 
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr "%>%" 
#' @importFrom rlang UQ UQS '!!' '!!!' parse_expr
#' @export
#'
plot_FAA<-function(rep,plot=TRUE,verbose=TRUE,Year=1952:2015,
			YLAB="Fishing mortality/year",onlyTotal=FALSE){
cat("Starting plot_FAA ;")
theme_set(theme_bw())

.<-"XXXX"

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
if(verbose)cat("L37 ; ") #;browser()
FAA<-rep$FbyAgeYr
#maxF<-max(FAA)
#cat("L27\n") #;browser()
FAAreg<-if(is.list(rep$FatYrAgeReg)){
	lapply(1:2,function(i){add.FAAs(rep$FatYrAgeReg[[1]][,,i])})
	}else{
		apply(rep$FatYrAgeReg,1:2,add.FAAs)
	}
#cat("L30\n") #;browser()
FAA<-add.FAAs(FAA)
maxF<-max(max(FAA[,c("1_3","4_6","7+")]),sapply(FAAreg,function(x){max(x[,c("1_3","4_6","7+")])}))
if(verbose)cat("L49\n");browser()
pl.all<-FAA%>%
           gather(key="Age",value="F",-!!sym("Year")) %>%
  #         filter('%in%'( !!sym("Age"), !!!syms(c("1_3","4_6","7+")))) %>%
  #         filter(match( !!sym("Age"), c("1_3","4_6","7+"))>0) %>%  
           filter(eval(parse_expr( 'Age %in% c("1_3","4_6","7+")' ))) %>% 
           ggplot(aes_string(x="Year",y="F",color="Age"))
pl.all<-pl.all+geom_line(size=1)+ylab(YLAB)+labs(title="Total")+labs(colour="")+theme(legend.position="bottom")+ylim(0,maxF*1.05)
pl.reg<-FAAreg%>%lapply(.,function(z){
  pl<-z%>%gather(.,key="Age",value="F",-!!sym("Year"))%>% 
    filter(.,eval(parse_expr('Age %in% c("1_3","4_6","7+")'))) %>% ggplot(aes_string(x="Year",y="F",color="Age"))
  pl<-pl+geom_line(size=1)+ylab(YLAB)+xlab("")+labs(colour="")+theme(legend.position="none")+ylim(0,maxF*1.05)
  return(pl)
})
for(i in 1:2)pl.reg[[i]]<-pl.reg[[i]]+labs(title=paste0("Region ",i))
if(verbose)cat("L64 ; ") #;browser()

pl<-if(!onlyTotal){
  grid.arrange(pl.reg[[1]],pl.reg[[2]],pl.all,ncol=2)
  }else{
  pl.all
  }
if(plot)print(pl)
cat(" Finished plot_FAA\n")
return(invisible(pl))

}