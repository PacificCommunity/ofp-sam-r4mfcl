#' Function to plot selectivity at mean length at age 
#'
#' @param filename file name of "selectivity-multi-sex", default : "selectivity-multi-sex"
#' @param fishlab vector of string names of fisheries
#' @param rep outputs of read.rep
#' @param xlab caption of x-axis
#' @param ylab caption of y-axis
#' @param ncol number of columns of plots
#' @param dir horizontal order or not
#' @param use.selex.multi.sex LOGICAL if selex from selectivity-multi-sex be used instead from plot.rep, default : FALSE
#' @param plot LOGICAL if plot be sent ot graphics device
#' @param verbose verbose or not
#' @importFrom ggplot2 ggplot theme_set theme_bw geom_line aes_string geom_point facet_wrap guides labs ylab
#' @importFrom tidyr gather separate unite
#' @importFrom dplyr mutate
#' @importFrom magrittr '%>%'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym syms
#' @importFrom stringr str_pad
#' @export
#'
plot_selectivity.atLength<-function(filename="selectivity-multi-sex",
                                    fishlab,
         #                           par=read.par("09.par"),
                                    rep=read.rep("plot-09.par.rep"),
                                    xlab="cm",
                                    ylab="Selectivity",
                                    ncol=NULL,
                                    dir="h",
                                    use.selex.multi.sex=FALSE,
                                    plot=TRUE,
                                    verbose=TRUE){
  if(verbose)cat("Starting plot_selectivityatLength\n")

  theme_set(theme_bw())
  nSp<-rep$nSp
  tblocks<-!all(rep$SelexTblocks==1)
  nfishWTblocks<-sum(rep$SelexTblocks)/rep$nSp
  nfish<-rep$nFisheries # length(rep$SelexTblocks)/rep$nSp
  FL0<-unlist(sapply(1:nfish,function(i){
    nblk<-rep$SelexTblocks[i]
    tmp<-if(nblk==1){paste(i)}else{paste(i,1:nblk,sep="_")}
    if(i<10){paste0("0",tmp)}else{tmp}
  }))

  use.selex.multi.sex<-(all(rep$SelexTblocks==1) & use.selex.multi.sex & filename=="selectivity-multi-sex" & file.exists(filename) & file.size(filename)>0 )
  if(use.selex.multi.sex){
    xx<-readLines(filename)
     nfishWTblocks<-nfish<-length(grep(xx,pattern="^# fishery"))
    xx[sort(c((1:nfish)*3,(1:nfish)*3-1))] %>%
      sapply(function(x){trimws(x) %>% strsplit(split=" +")->tmp; as.numeric(tmp[[1]])},simplify="array") ->yy
  }else{
    yy<-t(rep$SelAtAge)
    if(verbose)cat("L44 ; ") #;browser()
   if(all(rep$SelexTblocks==1)){
      nfish<- dim(rep$SelAtAge)[1]/rep$nSp
      tblocks<-FALSE
      nfishWTblocks<-nfish/rep$nSp
    }else{
      nfish<- length(rep$SelexTblocks)/rep$nSp
      tblocks<-TRUE
      nfishWTblocks<-sum(rep$SelexTblocks)/rep$nSp
    }
  }
  nSp<-rep$nSp
  FL0<-unlist(sapply(1:nfish,function(i){
    nblk<-rep$SelexTblocks[i]
    tmp<-if(nblk==1){paste(i)}else{paste(i,1:nblk,sep="_")}
    if(i<10){paste0("0",tmp)}else{tmp}
  }))  
  if(verbose)cat("L71;")
  dimnames(yy)[[1]]<-paste0(1:dim(yy)[1])
  if(verbose)cat("L73;") #; browser()
  dimnames(yy)[[2]]<-if(use.selex.multi.sex){
    paste0("FL",rep(1:nfish,each=2),c("Male","Female"))
  }else{
    if(nSp==1){
      paste0("FL",FL0)
    }else{
      paste0("FL",rep(FL0,2),c(rep("Male",nfishWTblocks),rep("Female",nfishWTblocks)))
    }
  }
  yy<-apply(yy,1:2,as.numeric)
  yy.dt2<-as.data.table(t(yy))
  yy.dt2$Gender<-if(use.selex.multi.sex){
    xx<-rep("Both",dim(yy.dt2)[1])
    xx[grep(colnames(yy),pattern="Male$")]<-"Male"
    xx[grep(colnames(yy),pattern="Female$")]<-"Female"
    xx
  }else{
    if(nSp>1){
      c(rep("Male",nfishWTblocks),rep("Female",nfishWTblocks))
    }else{
      rep("Both",nfishWTblocks)
    }
  }
  fishlab<-if(use.selex.multi.sex){
    if(is.null(fishlab)){
      paste(rep(FL0,each=2),paste0("FL",rep(substr(FL0,1,2),each=2)),sep="_")
    }else{
      paste(rep(FL0,each=2),rep(unlist(sapply(1:nfish,function(i){rep(fishlab[i],rep$SelexTblocks[i])})),each=2),sep="_")
    }
  }else{
    if(is.null(fishlab)){
      paste(rep(FL0,2),paste0("FL",rep(FL0,2)),sep="_")
    }else{
      paste(rep(FL0,2),rep(unlist(sapply(1:nfish,function(i){rep(fishlab[i],rep$SelexTblocks[i])})),2),sep="_")
    }
  }


  if(verbose)cat("L103 ;")#;browser()
  yy.dt2$Fishery<-fishlab[1:(nfishWTblocks*nSp)]
  yy.dt2 %>% unite(col="Fishery_Gender",!!!syms(c("Fishery","Gender")),sep="-") %>%
    gather(key="AgeClass",value="selex",remove=-!!sym("Fishery_Gender")) %>%
    separate(col="Fishery_Gender",into=c("Fishery","Gender"),sep="-") %>%
    mutate(Age=as.numeric(!!sym("AgeClass")),Fish=!!sym("Fishery"))-> yy.dt3
  if(verbose)cat("L109;") #;  browser()
  yy.dt3$meanL<-1:dim(yy.dt3)[1]
  for(i in 1:dim(yy.dt3)[1]){
    Gender<-if(nSp>1){
      ifelse(yy.dt3[i,"Gender"]=="Male",1,2)
    }else{
      "Both"
    }
    Age   <-as.numeric(yy.dt3[i,"Age"])
    meanL <-if(nSp>1){
      rep$mean.LatAge[Gender,Age]
    }else{
      rep$mean.LatAge[1,Age]
    }
   yy.dt3[i,"meanL"]<-meanL
  }
  if(verbose)cat("L125;") #;browser()
  p<-yy.dt3 %>% ggplot(aes_string(x="meanL",y="selex"))
  p<-p+xlab(xlab)+ylab(ylab)
   p<-p+geom_line(aes_string(color="Gender"))+geom_point(aes_string(color="Gender"),size=1)+facet_wrap(~Fish,ncol=ncol,dir=dir)
  if(nSp==1)p<-p+labs(colour="")+guides(color=FALSE)  
  if(plot)print(p)
#  browser()
  return(invisible(p))
}