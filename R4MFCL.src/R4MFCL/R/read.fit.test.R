#' Function to read {length, weight}.fit files
#' @param fit.file CHRACTER file name of *.fit file
#' @param verbose produce verbose screen outputs?
#' @param rep outputs of read.rep
#' @param overall.composition.plot if TRUE make a plot of overall.composition.plot
#' @param fit if TRUE and if overall.composition.plo is TRUE predicted fits will be overlayed to overall.composition.plot
#' @param plot TRUE plot will be created on screen
#' @param plot.ctl list of control param,aters for overall.composition.plot
#' @importFrom dplyr filter select_ summarize select
#' @importFrom tidyr gather unite separate 
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot geom_bar geom_line xlab ylab theme_set theme_bw element_blank aes_string
#' @importFrom rlang quo sym syms ":="
# ' @import scales 
#' @importFrom stringr str_pad
#' @export
 read.fit.test <-
function(fit.file,
         verbose=FALSE,
         rep=NULL,
         overall.composition.plot=FALSE,
         fit=FALSE,
         plot=TRUE,
  plot.ctl=list(xlabel="Length(cm)",dir="h",line.wdth=0.5,Ncols=2,lincol="#FF3333",fillcol="#6699CC",nbrks=3)) {
  # Simon Hoyle March 2010
  # loads the observed and expected LF from the length.fit file by fishery and time period
  # YT March-April 2017 to catch up versions 2 and 3 fit file
  # YT June 2017 fixed for version 3 fit file with single species
  # 
  # Quick solution to avoid "R CMD check and no visible binding for global variable '.'"
  #if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "n"),add=TRUE)
  .<-"XXXXXX"
  cat("Starting read.fit ;")
  if(overall.composition.plot)theme_set(theme_bw())
  datfromstr<-function (datstring)
  {
    out<-if(length(datstring)>1){
      datstring %>% trimws() %>% strsplit(split = "[[:blank:]]+") %>% sapply(.,"as.numeric",USE.NAMES =FALSE,simplify="array") %>%t()
    }else{
      datstring %>% trimws() %>% strsplit(split = "[[:blank:]]+") %>% "[["(1) %>% as.numeric()
    }
    return(out)
  }

  a <- readLines(fit.file)

  version<- if(strsplit(a[1],split=" +")[[1]][1]=='#'){
             as.numeric(strsplit(a[1],split=" +")[[1]][3])
            }else{
              1
            }
  if(verbose)cat("L52;") #;browser()

  Nfsh <- as.numeric(a[ifelse(version==1,3,4)])-1    # Determine the number of fisheries from file header
  Nskips <- as.numeric(a[ifelse(version==1,5,6)])    # Determine the number of lines in the matrix for each fishery, from file header
#    size.pars <- scan(filename, nlines=1, skip=ifelse(version==1,1,2))  # Extract the parameters that determine the size bins - no. bins, first bin size, bin width
#    sizebins <- seq(from=size.pars[2], by=size.pars[3], length.out=size.pars[1])   # Construct the size bins from the file header
  # Pointer to species for each fishery
  fishSpPtr <-if(version>=2){datfromstr(a[8])}else{NA}
  nbins <- as.numeric(unlist(strsplit(a[ifelse(version==1,2,3)],split="[[:blank:]]+")))[1]
  binfirst <- as.numeric(unlist(strsplit(a[ifelse(version==1,2,3)],split="[[:blank:]]+")))[2]
  binwidth <- as.numeric(unlist(strsplit(a[ifelse(version==1,2,3)],split="[[:blank:]]+")))[3]
  #number of fisheries
  nfish <- as.numeric(a[ifelse(version==1,3,4)])-1
  #records per fishery
#  recsperfish <- as.numeric(unlist(strsplit(str_trim(a[ifelse(version==1,4,5)]),split="[[:blank:]]+")))
  recsperfish <- datfromstr(a[ifelse(version==1,4,5)])
  # Number of species
  nSp<-ifelse(version>=2,length(unique(fishSpPtr)),1)
  # number of age classes
  nages <- as.numeric(a[ifelse(version==1,5,6)])
  if(fit.file=="weight.fit"){
  # remove lines of "#wghtsum"
    a<-a[-(grep("^#wghtsum",a))]
  # remove lines of those contents is #wghtfrq
    a<-a[-(grep("^#wghtfrq",a))]
  # remove lines of those contents is #predicted weight distribution
    a<-a[-(grep("^#predicted weight distribution",a))]
  }
  # fishery locations
  fishlocs <- grep("^# fishery",a)   # f <- 1
  if(verbose)cat("L82; ") #;browser()
  dates <- list(); obslf <- list(); predlf <- list();spPtr<-list();smplSz<-list()
  pos.offset<-ifelse(version<=2,0,2)
  for (f in 1:nfish) {
    if (recsperfish[f] > 0) {
      dloc <- seq(fishlocs[f]+1,by=7+pos.offset+nages,length.out=recsperfish[f])
      dates[[f]] <- if(recsperfish[f]>1){
        data.frame(datfromstr(a[dloc])) 
      }else{
        data.frame(t(datfromstr(a[dloc])))
      }
      colnames(dates[[f]])<-c("Year","Month","Week")
      if(version>2){
        spPtrloc<-seq(fishlocs[f]+2,by=7+pos.offset+nages,length.out=recsperfish[f])
        spPtr[[f]]<-if(nSp>1){
          data.frame(datfromstr(a[spPtrloc]))
        }else{
          data.frame(t(datfromstr(a[spPtrloc])))
        }
        smplSz[[f]]<-as.numeric(a[spPtrloc+1])
      }
      if(verbose)cat("L103 f=",f,";") 
      obsloc <- seq(fishlocs[f]+4+pos.offset,by=7+pos.offset+nages,length.out=recsperfish[f])
      if(recsperfish[f]>1){
        obslf[[f]] <- data.frame(datfromstr(a[obsloc])) 
        predlf[[f]] <- data.frame(datfromstr(a[obsloc+1])) 
      }else{
        obslf[[f]] <- data.frame(t(datfromstr(a[obsloc]))) 
        predlf[[f]] <- data.frame(t(datfromstr(a[obsloc+1])))
      }
      if(verbose)cat("L112 ; ") 
      colnames(obslf[[f]])<-
        colnames(predlf[[f]])<-paste0("B",seq(from=binfirst,length.out=nbins,by=binwidth))
      if(verbose)cat("L115 ;")#;browser()
    }else{
      dates[[f]] <-NULL
    }
  }
  if(verbose){cat("L120 ;")}

  makeNewdata<-function(lf,set=NA){
    if(verbose)cat("starting makeNewdata\n")

    set<-ifelse(!is.na(set),set,ifelse(deparse(substitute(lf))=="obslf","Obs","Pred"))
  
    newdata.tmp<-lapply((1:length(dates))[!sapply(dates,"is.null")],function(i){
      if(verbose)cat("L128 ; i=",i,";")
      tmp<-if(version<=2){
        cbind(dates[[i]],lf[[i]])
      }else{
        if(dim(dates[[i]])[1]>1){
          cbind(dates[[i]],spPtr[[i]],smplSz[[i]],lf[[i]])
        }else{
          cbind(dates[[i]],t(spPtr[[i]]),smplSz[[i]],lf[[i]])
        }
      }
      ################################################
      tmp$Fishery<-i
      tmp$Sp<-if(version>2){
        fishSpPtr[i]
      }else if(version==2){
        ifelse(i<=(nfish/nSp),1,2) 
      }else{
        NA
      }
      if(version>2)tmp$Gender<-ifelse(fishSpPtr[i]==1,1,2)
      if(version>=2)tmp$RealFishery<-ifelse(i<=nfish/nSp,i,i-nfish/nSp)
      colnames(tmp)[1:3]<-c("Year","Month","Week")
      if(version>2){
        if(nSp==2){
          colnames(tmp)[4:6]<-c("Sp1","Sp2","smplsz")
        }else if(nSp==1){
          colnames(tmp)[4:5]<-c("Sp1","smplsz")
        }else{
          stop("nSp=",nSp, "in read.fit")
        }
      }
      tmp$Set<-ifelse(!is.na(set),set,stop("L159 in makeNewdata in read.fit set is NA "))
      return(tmp)
    })
    return(newdata.tmp)
  }

   newdata.obs<-makeNewdata(obslf)
  ############## for version >2 ############

  if(version>2 & nSp>1){
    if(verbose)cat("L169 ;") 
    n<-length(newdata.obs)
    for(i in 1:(n/nSp)){
      fish<-as.numeric(unique(newdata.obs[[i]]$Fishery))
      if(!is.null(spPtr[[fish]]) & any(spPtr[[fish]]==spPtr[[fish+nfish/nSp]]) &
        any(spPtr[[fish]][,1]==spPtr[[fish+nfish/nSp]][,2]) ){
        newdata.obs[[i]][,"Gender"]<-"Both"
        newdata.obs[[i]][5+1:nbins]<-newdata.obs[[i]][5+1:nbins]*newdata.obs[[i]]$smplSz
      }else if(is.null(spPtr[[fish]])){
        cat("L178 fish=",fish,"\n")
      }else{
        cat("i=",i,"\n")  #  ;browser()
        stop("Additional codes is needed if sex specific composition data is availabale")
      }
    }
    xz<-(1:length(newdata.obs))[sapply(newdata.obs,function(x){unique(x$Gender)})!="Both"]
    newdata.obs[xz]<-NULL
  }

  newdata.obs<-do.call("rbind",newdata.obs)
  if(verbose)cat("L189;") 

###################################################
   newdata.pred<-makeNewdata(predlf)
  if(verbose)cat("L193 ;") 
  if(!is.null(rep)){
    Year1<-rep$Year1
    Rlz.t.fsh<-rep$Rlz.t.fsh
    Rlz.t.fsh1<-Rlz.t.fsh-Year1+1
    yq<-lapply(dates,function(x){x[,1]+(x[,2]%/%3+1)/4-0.125})
  }else{yq<-NULL}
  ####### for version>2 & nSp>1
  if(version>2 & nSp>1){
    if(is.null(rep))stop("rep is needed" )
    pcatch<-rep$PredCatch
    pcatch1<-1:length(dates) %>% sapply(function(i){
                        tmp<-sapply(yq[[i]],function(x){which(Rlz.t.fsh1[i,]==x)});if(length(tmp)>0){pcatch[i,tmp]}else{NULL}
                    })
    if(verbose)cat("length(pcatch)=",length(pcatch),"\n","length(pcatch1)=",length(pcatch1),"\n")
    for(i in 1:length(newdata.pred)){
      if(verbose)cat("L209 i=",i,";")
      fish<-as.numeric(unique(newdata.pred[[i]]$Fishery))
      prop<-pcatch1[[fish]]/if(fish<=nfish/nSp){
        pcatch1[[fish]]+pcatch1[[fish+nfish/nSp]]}else{pcatch1[[fish-nfish/nSp]]+pcatch1[[fish]]}
      newdata.pred[[i]][5+1:nbins]<-newdata.pred[[i]][5+1:nbins]*prop*newdata.pred[[i]]$smplSz
    }
  }
  yq<-if(!is.null(rep)){lapply(yq,function(x){x+Year1-1})}else{NULL}
  if(verbose)cat("L217 ;") 
  ####################################
  newdata.pred<-do.call("rbind",newdata.pred)
  newdata<-rbind(newdata.obs,newdata.pred)
  if(verbose)cat("L221 ;") # ; browser()
  #########################################
  if(0){
  col.offset<-if(version==1){3}else{ifelse(nSp==1,5,6)}
  }
  if(verbose)cat("L226 ;") # ; browser()
  colnames(newdata.pred)[1:3]<-colnames(newdata.obs)[1:3]<-colnames(newdata)[1:3]<-c("timeperiod","month","week")
  col.offset<-ifelse(version>2,3,1)
  if(version>2){
    colnames(newdata.pred)[4:(4+nSp)]<-colnames(newdata.obs)[4:(4+nSp)]<-colnames(newdata)[4:(4+nSp)]<-
                    if(nSp==1){c("Both","nsmpl")}else{c("Male","Female","nsmpl")}
  }
  if(verbose)cat("L233 ;") # ;browser() 
  colnames(newdata)[col.offset+1+nSp+1:nbins]<-
  colnames(newdata.pred)[col.offset+1+nSp+1:nbins]<-
  colnames(newdata.obs)[col.offset+1+nSp+1:nbins]<-paste0(binfirst+(0:(nbins-1))*binwidth)
  newdata$yq<-newdata.obs$yq<-newdata.pred$yq<-if(!is.null(yq)){unlist(yq)}else{NA}
  if(verbose)cat("L238 ;")  # ; browser()
  nms<-if(version==1){
    c("timeperiod","month","week","Fishery","Set")
  }else if(version==2){
    c("timeperiod","month","week","Fishery","Sp","Set")
  }else if(version>2 & nSp==1){
    c("timeperiod","month","week","Both","nsmpl","Fishery","Sp","Gender","Set")
  }else if(version>2 & nSp==2){
    c("timeperiod","month","week","Male","Female","nsmpl","Fishery","Sp","Gender","Set")
  }
 # if(!is.null(rep))
  nms<-c(nms,"yq")
  
  if(version>=2){
    newdata %<>% select(.,-!!sym("Fishery")) %>% rename(Fishery=!!sym("RealFishery"))
    newdata.obs %<>% select(.,-!!sym("Fishery")) %>% rename(Fishery=!!sym("RealFishery"))
    newdata.pred %<>% select(.,-!!sym("Fishery")) %>% rename(Fishery=!!sym("RealFishery"))
  }
  if(verbose)cat("L256 ; ")  # ;browser()
  longdata<-newdata %>% unite(.,col="United",from=!!!syms(nms),remove=TRUE,sep="_") %>%
                     gather(key="bin",value="frq",-!!sym("United")) %>% separate(.,col="United",into=nms,sep="_")
                 
  if(verbose)cat("L260 ;") 

  longdata.obs<-newdata.obs %>% 
              unite(.,col="United",!!!syms(nms),remove=TRUE,sep="_") %>%
              gather(key="bin",value="frq",-!!sym("United")) %>% separate(.,col="United",into=nms,sep="_")
  
  if(verbose)cat("L266 ;")

  if(overall.composition.plot){
    if(nSp==1 || version<=2)stop("overall.composition.plot is only available for 2 sex and version 3 fit file")
    plot.data <- longdata %>% group_by(!!!syms(c("Fishery","Gender","Set","bin"))) %>% summarize(!!"n":=sum(sym("frq")))
    plot.data$Fishery<-
      paste(if(nfish/nSp<10){plot.data$Fishery}else{str_pad(paste(plot.data$Fishery),width=2,pad="0")},
        plot.ctl$fleetlabs[as.numeric(paste(plot.data$Fishery))],sep="_")
      if(verbose){cat("L274 ;");cat("colnames(plot.data):\n",colnames(plot.data),"\n")}
      p<-plot.data %>% filter(.,!!sym("Set")=="Obs") %>% ggplot(aes_string(x="bin",y="n"))+
        geom_bar(stat="identity", colour=plot.ctl$fillcol, fill=plot.ctl$fillcol)+
        facet_wrap(~Fishery,ncol=plot.ctl$Ncols,scales="free_y",dir=plot.ctl$dir)
      p<-p+xlab(plot.ctl$xlabel) + ylab("Samples")
      if(fit){
        if(verbose){cat("L280 ; ");cat("colnames(plot.data):\n",colnames(plot.data),"\n")}
        p<-p+ plot.data %>% filter(.,!!sym("Set")=="Pred") %>%
          geom_line(data=.,aes_string(x="SizeBin",y="n",group="Gender",color="Gender"), size=plot.ctl$line.wdth,position="Stack")
      }
      p<-p+scale_y_continuous(breaks=pretty_breaks(n=plot.ctl$nbrks)) +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      if(plot)print(p)
  }
  
  if(verbose)cat("L289 ;") # ;browser()
  longdata.pred<- newdata.pred %>% unite(.,col="United",!!!syms(nms),remove=TRUE,sep="_") %>%
                    gather(key="bin",value="frq",-!!sym("United"))  %>% separate(.,col="United",into=nms,sep="_")
             
  results<-list(dates=dates,
                        obslf=obslf,
                        predlf=predlf,
                        spPtr=if(version>2){spPtr}else{NULL},
                        smplSz=if(version>2){smplSz}else{NULL},
                        version=version,
                        header=list(nfish=nfish,
                                    nages=nages,
                                    nbins=nbins,
                                    binfirst=binfirst,
                                    binwidth=binwidth,
                                    nSp=nSp),
                        longdata=longdata,
                        longdata.obs=longdata.obs,
                        longdata.pred=longdata.pred,
                        newdata=newdata,
                        newdata.obs=newdata.obs,
                        newdata.pred=newdata.pred)
#  if(!is.null(yq))results$yq<-yq
  if(overall.composition.plot){
    results$p<-p
    results$plot.data<-plot.data
  }
  if(verbose)cat("L316 Finished read.fit\n")
  return(invisible(results))
}
