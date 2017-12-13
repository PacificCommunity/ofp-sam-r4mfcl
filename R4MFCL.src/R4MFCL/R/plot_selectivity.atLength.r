## Function to plot selectivity of 2 sex model from
#' @importFrom ggplot2 ggplot theme_set theme_bw geom_line aes_string geom_point facet_wrap guides labs ylab
#' @importFrom tidyr gather separate unite
#' @importFrom dplyr mutate
#' @importFrom magrittr '%>%'
#' @importFrom data.table as.data.table
#' @importFrom rlang sym syms
#  ' @importFrom stringr str_split
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
#  require(R4MFCL)
#  require(ggplot2)
#  require(reader)
#  require(stringr)
#  require(magrittr)
#  require(data.table)
#  require(dplyr)
#  require(tidyr)
  theme_set(theme_bw())
 
  if(all(rep$SelexTblocks==1) & use.selex.multi.sex & filename=="selectivity-multi-sex" & file.exists(filename) & file.size(filename)>0){
    xx<-readLines(filename)
     nfishWTblocks<-nfish<-length(grep(xx,pattern="^# fishery"))
    xx[sort(c((1:nfish)*3,(1:nfish)*3-1))] %>%
      sapply(function(x){trimws(x) %>% strsplit(split=" +")->tmp; as.numeric(tmp[[1]])},simplify="array") ->yy
  }else{
    yy<-t(rep$SelAtAge)
    if(verbose)cat("L33;") #;browser()
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
  if(verbose)cat("L59;")
  dimnames(yy)[[1]]<-paste0(1:dim(yy)[1])
  if(verbose)cat("L61;")
  dimnames(yy)[[2]]<-if(all(rep$SelexTblocks==1) & use.selex.multi.sex & filename=="selectivity-multi-sex" & file.exists(filename) & file.size(filename)>0){
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
  yy.dt2$Gender<-if(all(rep$SelexTblocks==1) & use.selex.multi.sex & filename=="selectivity-multi-sex" & file.exists(filename) & file.size(filename)>0){
#    rep(c("Male","Female"),nfishWTblocks)
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
  fishlab<-if(all(rep$SelexTblocks==1) & use.selex.multi.sex & filename=="selectivity-multi-sex" & file.exists(filename) & file.size(filename)>0){
    if(is.null(fishlab)){
      paste(rep(FL0,each=2),paste0("FL",rep(FL0,each=2)),sep="_")
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

#  browser()
  if(verbose)cat("L92;")#;browser()
  yy.dt2$Fishery<-fishlab[1:(nfishWTblocks*nSp)]
  yy.dt2 %>% unite(col="Fishery_Gender",!!!syms(c("Fishery","Gender")),sep="-") %>%
    gather(key="AgeClass",value="selex",remove=-!!sym("Fishery_Gender")) %>%
    separate(col="Fishery_Gender",into=c("Fishery","Gender"),sep="-") %>%
    mutate(Age=!!sym("as.numeric(AgeClass)"),Fish=!!sym("Fishery"))-> yy.dt3
  if(verbose)cat("L98;") #;  browser()
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
      rep$mean.LatAge[Age]
    }
   yy.dt3[i,"meanL"]<-meanL
  }
  if(verbose)cat("L114;") #;browser()
  p<-yy.dt3 %>% ggplot(aes_string(x="meanL",y="selex"))
  p<-p+xlab(xlab)+ylab(ylab)
   p<-p+geom_line(aes_string(color="Gender"))+geom_point(aes_string(color="Gender"),size=1)+facet_wrap(~Fish,ncol=ncol,dir=dir)
  print(p)
#  browser()
  return(invisible(p))
}