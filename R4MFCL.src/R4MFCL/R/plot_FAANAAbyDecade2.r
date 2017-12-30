#' Make plots of F@A and N@A by decade
#'
#' @param rep outputs of read.rep
#' @param plot LOGICAL if TRUE plot will be sent to graphics device
#' @param verbose if TRUE verbose outputs be made
#' @param Year range of years
#' @param YLAB string label for y-axis
#'
# ' @param col
#'
#' @importFrom dplyr filter
#'  
#' @importFrom tidyr gather 
#' @importFrom ggplot2 ggplot  theme_set theme_bw labs geom_line ylab labs aes_string xlab facet_wrap
# ' @importFrom scales 
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr "%>%" 
#' @export
#'
plot_FAANAAbyDecade2<-function(rep,
                               plot=TRUE,
                               verbose=TRUE,
                               Year=1952:2015, # col=heat.colors(7),
                               YLAB="average N@A by decade"){
theme_set(theme_bw())

.<-"XXXX"

add.FAAs<-function(FAA){
  FAA<-as.data.frame(FAA)
  colnames(FAA)[1:20]<-paste0("",1:20)

  FAA$Year<-floor(Year/10)*10
  return(FAA)
}
cat("L31 ; ") #;browser()
FAA<-rep$FbyAgeYr
FAA<-add.FAAs(FAA)
#Fdecade<-list()
decades<-c(1950,1960,1970,1980,1990,2000,2010)
Fdecade<-t(sapply(decades,function(y){colMeans(FAA[FAA$Year==y,1:20])}))
rownames(Fdecade)<-paste(decades)
Fdecade<-as.data.frame(Fdecade)
Fdecade$decade<-rownames(Fdecade)
Fdecade.long<-Fdecade %>%gather(., key="Age",eval(parse_expr("paste(1:20)")),value="F")
Fdecade.long$Age<-as.numeric(paste(Fdecade.long$Age))
pl.F<-Fdecade.long %>%ggplot(.,aes_string(x="Age",y="F"))+geom_line()+xlab("")+ylab("Average F by decade)")
#pl<-pl+geom_line()
pl.F<-pl.F+facet_wrap(~decade,ncol=1)
cat("L45\n") #;browser()

###########################################
# Numbers at age
NAA<-as.data.frame(apply(rep$NatYrAgeReg,1:2,"sum"))
colnames(NAA)[1:20]<-paste0("",1:20)
NAA$Year<-floor(Year/10)*10
Ndecade<-t(sapply(decades,function(y){colMeans(NAA[NAA$Year==y,1:20])}))
rownames(Ndecade)<-paste(decades)
Ndecade<-as.data.frame(Ndecade)
Ndecade$decade<-rownames(Ndecade)
cat("L56 ; ") #;browser()
Ndecade.long<-Ndecade %>% gather(., key="Age",eval(parse_expr("paste(1:20)")),value="F")
Ndecade.long$Age<-as.numeric(paste(Ndecade.long$Age))
pl.N<-Ndecade.long %>%ggplot(.,aes_string(x="Age",y="F"))+geom_line()+xlab("")+ylab(YLAB)
pl.N<-pl.N+facet_wrap(~decade,ncol=1)
pl<-grid.arrange(pl.N, pl.F,ncol = 2)
cat("Finished plot_FAANAAbyDecade2\n") # ;browser()


return(invisible(pl))

}