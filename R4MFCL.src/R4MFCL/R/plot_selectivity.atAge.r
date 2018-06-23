#' Function to plot selectivity at age of 2 sex model from
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
#' @param withData logical if TRUE return value will be p : ggplot object, wide data and long data, default : FALSE
#' @importFrom ggplot2 ggplot theme_set theme_bw geom_line aes_string geom_point facet_wrap guides labs ylab
#' @importFrom tidyr gather separate unite
#' @importFrom dplyr mutate
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_split str_trim
#' @importFrom data.table as.data.table
#' @importFrom rlang sym syms
#' @export
#'
plot_selectivity.atAge<-function(filename="selectivity-multi-sex",
                                 fishlab,
                                 xlab="Age",
                                 ylab="Selectivity",
                                 ncol=NULL,
                                 dir="h",
                                 rep=read.rep("plot-09.par.rep"),
                                 use.selex.multi.sex=FALSE,
                                 plot=TRUE,
                                 verbose=TRUE,
                                 withData=FALSE
                          ){
  if(verbose)cat("Starting plot_selectivityatAge\n")

  theme_set(theme_bw())
  nSp<-rep$nSp
  tblocks<-!all(rep$SelexTblocks==1)
  nfishWTblocks<-sum(rep$SelexTblocks)/rep$nSp
  nfish<-rep$nFisheries/rep$nSp # length(rep$SelexTblocks)/rep$nSp
  FL0<-unlist(sapply(1:nfish,function(i){
            nblk<-rep$SelexTblocks[i]
            tmp<-if(nblk==1){paste(i)}else{paste(i,1:nblk,sep="_")}
            if(i<10){paste0("0",tmp)}else{tmp}
          }))

  use.selex.multi.sex<-(all(rep$SelexTblocks==1) & use.selex.multi.sex & filename=="selectivity-multi-sex" & file.exists(filename) & file.size(filename)>0 )
  if( use.selex.multi.sex){
  ## To-do need to deal with time blocks
    xx<-readLines(filename)
     nfishWTblocks<-nfish<-length(grep(xx,pattern="^# fishery"))
    xx[sort(c((1:nfish)*3,(1:nfish)*3-1))] %>%
      sapply(function(x){trimws(x) %>% strsplit(split=" +")->tmp; as.numeric(tmp[[1]])},simplify="array") ->yy
  }else{
     yy<-t(rep$SelAtAge)
    if(verbose)cat("L56;") #;browser()
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
  if(verbose)cat("L67;")
  
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
  #######
  cat("L83 ;")
  #yy<-apply(yy,1:2,as.numeric)
  yy.dt2<-as.data.table(t(yy));colnames(yy.dt2)<-paste(1:dim(yy.dt2)[2])
  yy.dt2$Fishery<-fishlab[1:(nfishWTblocks*nSp)]
  yy.dt2$Gender<-if(nSp>1){c(rep("Male",nfishWTblocks),rep("Female",nfishWTblocks))
  }else{rep("Both",nfishWTblocks)}
  if(verbose)cat("L88;") #;browser()
  yy.dt2 %>% unite(col="Fishery_Gender",!!!syms(c("Fishery","Gender")),sep="-") %>%
      gather(key="AgeClass",value="selex",remove=-!!sym("Fishery_Gender")) %>%
      separate(col="Fishery_Gender",into=c("Fishery","Gender"),sep="-") %>%
      mutate(Age=as.numeric(!!sym("AgeClass")),Fish=!!sym("Fishery"))-> yy.dt3
  if(verbose)cat("L93;") #;browser()
  p<-yy.dt3 %>% ggplot(aes_string(x="Age",y="selex"))
  p<-p+xlab(xlab)+ylab(ylab)
  p<-p+geom_line(aes_string(color="Gender"))+geom_point(aes_string(color="Gender"),size=1)+facet_wrap(~Fish,ncol=ncol,dir=dir)+ylab(ylab)
  if(nSp==1)p<-p+labs(colour="")+guides(color=FALSE)    #+guide_legend(label=FALSE)
#  browser()
  if(plot)print(p)
  if(verbose)cat("Finished plot_selectivity.atAge\n") # ;browser()
  if(withData){
    output<-list(p=p,widedata=yy.dt2,longdata=yy.dt3)
    return(invisible(output))
  }else{
    return(invisible(p))
  }
}

