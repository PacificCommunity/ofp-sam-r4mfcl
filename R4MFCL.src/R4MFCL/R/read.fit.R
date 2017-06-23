 read.fit <-
function(fit.file,verbose=FALSE,rep=NULL,overall.composition.plot=FALSE,fit=FALSE,plot=TRUE,
  plot.ctl=list(xlabel="Length(cm)",dir="h",line.wdth=0.5,Ncols=2,lincol="#FF3333",fillcol="#6699CC",nbrks=3)) {
  # Simon Hoyle March 2010
  # loads the observed and expected LF from the length.fit file by fishery and time period
  # YT March-April 2017 to catch up versions 2 and 3 fit file
  # YT June 2017 fixed for version 3 fit file with single species
#  require(stringr)
  cat("Starting read.fit ;")
  require(dplyr)
  require(tidyr)
  require(magrittr)
  require(ggplot2)
  require(scales)
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
  if(verbose)cat("L33;") #;browser()

  Nfsh <- as.numeric(a[ifelse(version==1,3,4)])-1  #scan(filename, nlines=1, skip=ifelse(version==1,2,3)) - 1   # Determine the number of fisheries from file header
  Nskips <- as.numeric(a[ifelse(version==1,5,6)]) #scan(filename, nlines=1, skip=ifelse(version==1,4,5))   # Determine the number of lines in the matrix for each fishery, from file header
#    size.pars <- scan(filename, nlines=1, skip=ifelse(version==1,1,2))  # Extract the parameters that determine the size bins - no. bins, first bin size, bin width
#    sizebins <- seq(from=size.pars[2], by=size.pars[3], length.out=size.pars[1])   # Construct the size bins from the file header
  # Pointer to species for each fishery
#  fishSpPtr <-if(version>=2){as.numeric(str_split(str_trim(a[8]),pattern=" +")[[1]])}else{NA}
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
  if(verbose)cat("L64; ") #;browser()
  dates <- list(); obslf <- list(); predlf <- list();spPtr<-list();smplSz<-list()
  pos.offset<-ifelse(version<=2,0,2)
  for (f in 1:nfish) {
    if (recsperfish[f] > 0) {
      dloc <- seq(fishlocs[f]+1,by=7+pos.offset+nages,length.out=recsperfish[f])
      dates[[f]] <- if(recsperfish[f]>1){
        data.frame(datfromstr(a[dloc])) # data.frame(t(matrix(as.numeric(unlist(strsplit(a[dloc],split="[[:blank:]]+"))),nrow=3)))
      }else{
        data.frame(t(datfromstr(a[dloc])))
      }
      colnames(dates[[f]])<-c("Year","Month","Week")
      if(version>2){
        spPtrloc<-seq(fishlocs[f]+2,by=7+pos.offset+nages,length.out=recsperfish[f])
        spPtr[[f]]<-if(nSp>1){
          data.frame(datfromstr(a[spPtrloc])) # data.frame(t(matrix(as.numeric(unlist(strsplit(str_trim(a[spPtrloc]),split="[[:blank:]]+"))),nrow=nSp)))
        }else{
          data.frame(t(datfromstr(a[spPtrloc])))
        }
        smplSz[[f]]<-as.numeric(a[spPtrloc+1])
      }
      if(verbose)cat("L85 f=",f,";") #;browser()
      obsloc <- seq(fishlocs[f]+4+pos.offset,by=7+pos.offset+nages,length.out=recsperfish[f])
      if(recsperfish[f]>1){
        obslf[[f]] <- data.frame(datfromstr(a[obsloc])) #data.frame(t(matrix(as.numeric(unlist(strsplit(str_trim(a[obsloc]),split="[[:blank:]]+"))),nrow=nbins)))
        predlf[[f]] <- data.frame(datfromstr(a[obsloc+1])) # data.frame(t(matrix(as.numeric(unlist(strsplit(str_trim(a[obsloc+1]),split="[[:blank:]]+"))),nrow=nbins)))
      }else{
        obslf[[f]] <- data.frame(t(datfromstr(a[obsloc]))) #data.frame(t(matrix(as.numeric(unlist(strsplit(str_trim(a[obsloc]),split="[[:blank:]]+"))),nrow=nbins)))
        predlf[[f]] <- data.frame(t(datfromstr(a[obsloc+1]))) # data.frame(t(matrix(as.numeric(unlist(strsplit(str_trim(a[obsloc+1]),split="[[:blank:]]+"))),nrow=nbins)))
      }
      if(verbose)cat("L94 ; ") # ;browser()
 #     cat("f=",f,"; ",colnames(obslf[[f]]),", nbins=",nbins,"\n" )
      colnames(obslf[[f]])<-
        colnames(predlf[[f]])<-paste0("B",seq(from=binfirst,length.out=nbins,by=binwidth))
      if(verbose)cat("L98;")#;browser()
    }else{
      dates[[f]] <-NULL
    }
  }
  if(verbose){cat("L103;")}

  newdata.obs<-lapply((1:length(dates))[!sapply(dates,"is.null")],function(i){
    if(verbose)cat("L106;i=",i,";")
    tmp<-if(version<=2){
      cbind(dates[[i]],obslf[[i]])
    }else{
      if(dim(dates[[i]])[1]>1){
        cbind(dates[[i]],spPtr[[i]],smplSz[[i]],obslf[[i]])
      }else{
        cbind(dates[[i]],t(spPtr[[i]]),smplSz[[i]],obslf[[i]])
      }
    }
    tmp$Fishery<-i
    tmp$Sp<-if(version>2){
      fishSpPtr[i]
    }else if(version==2){
      ifelse(i<=nfish,1,2) # This may need to be changed to ifelse(i<=(nfish/2),1,2)
    }else{
      NA
    }
    if(version>2)tmp$Gender<-ifelse(fishSpPtr[i]==1,1,2)
    if(version>=2)tmp$RealFishery<-ifelse(i<=nfish/2,i,i-nfish/2)
    colnames(tmp)[1:3]<-c("Year","Month","Week")
    if(version>2)colnames(tmp)[4:5]<-c("Sp1","Sp2")
    tmp$Set<-"Obs"
    return(tmp)
  })
  if(verbose)cat("L131;") # ;browser()
  ############## for version >2 ############

  if(version>2 & nSp>1){
    if(verbose)cat("L135\n") #;browser()
    n<-length(newdata.obs)
    for(i in 1:(n/2)){
      fish<-as.numeric(unique(newdata.obs[[i]]$Fishery))
      if(!is.null(spPtr[[fish]]) & any(spPtr[[fish]]==spPtr[[fish+nfish/2]]) &
        any(spPtr[[fish]][,1]==spPtr[[fish+nfish/2]][,2]) ){
 #       newdata.obs[[i+n/2]]<-NULL ;cat("newdata.obs[[",i+n/2,"]] was set to NULL\n");browser()
        newdata.obs[[i]][,"Gender"]<-"Both"
        newdata.obs[[i]][5+1:nbins]<-newdata.obs[[i]][5+1:nbins]*newdata.obs[[i]]$smplSz
      }else if(is.null(spPtr[[fish]])){
        cat("HERE fish=",fish,"\n")
      }else{
        cat("i=",i,"\n");browser()
        stop("Additional codes is needed if sex specific composition data is availabale")
      }
    }
    xz<-(1:length(newdata.obs))[sapply(newdata.obs,function(x){unique(x$Gender)})!="Both"]
    newdata.obs[xz]<-NULL
  }

  newdata.obs<-do.call("rbind",newdata.obs)
  if(verbose)cat("L156;") # ;browser()

###################################################
  newdata.pred<-lapply((1:length(dates))[!sapply(dates,"is.null")],function(i){
    tmp<-if(version<=2){
      cbind(dates[[i]],predlf[[i]])
    }else{
      if(dim(dates[[i]])[1]>1){
        cbind(dates[[i]],spPtr[[i]],smplSz[[i]],predlf[[i]])
      }else{
        cbind(dates[[i]],t(spPtr[[i]]),smplSz[[i]],predlf[[i]])
      }
    }
    tmp$Fishery<-i
    tmp$Sp<-if(version==1){
      NA
    }else if(version==2){
      ifelse(i<=nfish,1,2) # This may need to be changed to ifelse(i<=(nfish/2),1,2)
    }else{
      fishSpPtr[i]
    }
    if(version>2)tmp$Gender<-ifelse(fishSpPtr[i]==1,"Male","Female")
    if(version>=2)tmp$RealFishery<-ifelse(i<=nfish/2,i,i-nfish/2)
        colnames(tmp)[1:3]<-c("Year","Month","Week")
    if(version>2)colnames(tmp)[4:5]<-c("Sp1","Sp2")
    tmp$Set<-"Pred"
    return(tmp)
  })
  if(verbose)cat("L184;") # ;browser()
  ####### for version>2 & nSp>1
  if(version>2 & nSp>1){
    if(is.null(rep))stop("rep is needed" )
    pcatch<-rep$PredCatch
    Year1<-rep$Year1
    Rlz.t.fsh<-rep$Rlz.t.fsh
    Rlz.t.fsh1<-Rlz.t.fsh-Year1+1
 #   tmp<-read.fit(filename)
    yq<-lapply(dates,function(x){x[,1]+(x[,2]%/%3+1)/4-0.125})
    pcatch1<-1:length(dates) %>% sapply(function(i){
                        tmp<-sapply(yq[[i]],function(x){which(Rlz.t.fsh1[i,]==x)});if(length(tmp)>0){pcatch[i,tmp]}else{NULL}
                    })
    if(verbose)cat("length(pcatch)=",length(pcatch),"\n")
    if(verbose)cat("length(pcatch1)=",length(pcatch1),"\n")

    for(i in 1:length(newdata.pred)){
      if(verbose)cat("L201 i=",i,";")

      fish<-as.numeric(unique(newdata.pred[[i]]$Fishery))
#       if(fish==12)browser()
      prop<-pcatch1[[fish]]/if(fish<=nfish/2){
        pcatch1[[fish]]+pcatch1[[fish+nfish/2]]}else{pcatch1[[fish-nfish/2]]+pcatch1[[fish]]}
    #  newdata.pred[[i]]$smplSz*0.5*prop
      newdata.pred[[i]][5+1:nbins]<-newdata.pred[[i]][5+1:nbins]*prop*newdata.pred[[i]]$smplSz
#      if(fish==12)browser()
    }
  }
  if(verbose)cat("L212 ;") ;
#  browser()
  ####################################
  newdata.pred<-do.call("rbind",newdata.pred)
  if(verbose)cat("L216 ;") #;
#  browser()
  newdata<-rbind(newdata.obs,newdata.pred)
  if(verbose)cat("L219 ;")
  if(nSp>1){
    colnames(newdata)[6+1:nbins]<-
      paste0(ifelse(fit.file=="length.fit","L","W"),seq(from=binfirst,length.out=nbins,by=binwidth))
  }else{
    colnames(newdata)[5+1:nbins]<-
      paste0(ifelse(fit.file=="length.fit","L","W"),seq(from=binfirst,length.out=nbins,by=binwidth))

  }
  if(verbose)cat("L228\n") #;browser()
  colnames(newdata.pred)[1:3]<-colnames(newdata.obs)[1:3]<-colnames(newdata)[1:3]<-c("timeperiod","month","week")
  col.offset<-ifelse(version>2,3,1)
  if(version>2){
    if(nSp==2){
      colnames(newdata.pred)[4:6]<-colnames(newdata.obs)[4:6]<-colnames(newdata)[4:6]<-c("Male","Female","nsmpl")
    }else if(nSp==1){
      colnames(newdata.pred)[4:5]<-colnames(newdata.obs)[4:5]<-colnames(newdata)[4:5]<-c("Both","nsmpl")
    }
  }
  if(verbose)cat("L238;") #;browser()
  colnames(newdata.pred)[col.offset+1+nSp+1:nbins]<-
  colnames(newdata.obs)[col.offset+1+nSp+1:nbins]<-paste0(binfirst+(0:(nbins-1))*binwidth)
#  colnames(newdata)[col.offset+1+nSp+1:nbins]<-
  if(verbose)cat("L242;")  #;browser()
  tmp<-newdata %>% {
                    if(version==1)
                      unite(.,United,timeperiod,month,week,Fishery,Set,remove=TRUE,sep="_")
                    else if(version==2)
                      select(.,-Fishery) %>%unite(United,timeperiod,month,week,RealFishery,Sp,Set,remove=TRUE,sep="_")
                    else if(nSp==1)
                      select(.,-Fishery) %>%
                        unite(United,timeperiod,month,week,Both,nsmpl,RealFishery,Sp,Gender,Set,remove=TRUE,sep="_")
                    else if(nSp==2)
                      select(.,-Fishery) %>%
                        unite(United,timeperiod,month,week,Male,Female,
                          nsmpl,RealFishery,Sp,Gender,Set,remove=TRUE,sep="_")
                    else
                      stop("nSp=",nSp)
                   } %>% gather(key=bin,value=frq,-United)->tmp2
  if(verbose)cat("L258; ") # ;browser()
  longdata<-tmp2 %>% { if(version==1)
                        separate(.,col=United,into=c("timeperiod","month","week","Fishery","Set"),sep="_")
                      else if(version==2)
                        separate(.,col=United,into=c("timeperiod","month","week","RealFishery","Sp","Set"),sep="_")
                      else  if(nSp==2)
                        separate(.,col=United,into=c("timeperiod","month","week","Male",
                          "Female","nsmpl","RealFishery","Sp","Gender","Set"),sep="_")
                      else if(nSp==1)
                        separate(.,col=United,into=c("timeperiod","month","week","Both","nsmpl",
                          "RealFishery","Sp","Gender","Set"),sep="_")
                      else
                        stop("nSp=",nSp)
                    }
  longdata$SizeBin<-as.numeric(substr(longdata$bin,start=2,stop=6))
  if(verbose)cat("L273;") #;browser()

  tmp.obs<-newdata.obs %>% {
                    if(version==1)
                      unite(.,United,timeperiod,month,week,Fishery,Set,remove=TRUE,sep="_")
                    else if(version==2)
                      select(.,-Fishery) %>%unite(United,timeperiod,month,week,RealFishery,Sp,Set,remove=TRUE,sep="_")
                    else if(nSp==1)
                      select(.,-Fishery) %>%
                        unite(United,timeperiod,month,week,Both,nsmpl,RealFishery,Sp,Gender,Set,remove=TRUE,sep="_")
                    else if(nSp==2)
                      select(.,-Fishery) %>%
                        unite(United,timeperiod,month,week,Male,Female,
                          nsmpl,RealFishery,Sp,Gender,Set,remove=TRUE,sep="_")
                    else
                      stop("nSp=",nSp)
                   } %>% gather(key=bin,value=frq,-United)->tmp2
  if(verbose)cat("L290;") # ;browser()
  longdata.obs<-tmp2 %>% { if(version==1)
                        separate(.,col=United,into=c("timeperiod","month","week","Fishery","Set"),sep="_")
                      else if(version==2)
                        separate(.,col=United,into=c("timeperiod","month","week","RealFishery","Sp","Set"),sep="_")
                      else  if(nSp==2)
                        separate(.,col=United,into=c("timeperiod","month","week","Male",
                          "Female","nsmpl","RealFishery","Sp","Gender","Set"),sep="_")
                      else if(nSp==1)
                        separate(.,col=United,into=c("timeperiod","month","week","Both","nsmpl",
                          "RealFishery","Sp","Gender","Set"),sep="_")
                      else
                        stop("nSp=",nSp)
                    }
    if(verbose)cat("L304;")

    if(overall.composition.plot){
      if(nSp==1 || version<=2)stop("overall.composition.plot is only available for 2 sex and version 3 fit file")
      plot.data <- longdata %>% group_by(RealFishery,Gender,Set,SizeBin) %>% summarize(n=sum(frq))
      plot.data$Fishery<-
        paste(if(nfish/2<10){
          plot.data$RealFishery}else{stringr::str_pad(paste(plot.data$RealFishery),width=2,pad="0")},
          plot.ctl$fleetlabs[as.numeric(paste(plot.data$RealFishery))],sep="_")
      if(verbose){cat("L313\n");cat("colnames(plot.data):\n",colnames(plot.data),"\n")}
      p<-plot.data %>% dplyr::filter(.,Set=="Obs") %>% ggplot(aes(x=SizeBin,y=n))+
        geom_bar(stat="identity", colour=plot.ctl$fillcol, fill=plot.ctl$fillcol)+
        facet_wrap(~Fishery,ncol=plot.ctl$Ncols,scales="free_y",dir=plot.ctl$dir)
      p<-p+xlab(plot.ctl$xlabel) + ylab("Samples")
      if(fit){
        p<-p+ plot.data %>% filter(Set=="Pred") %>%
          geom_line(data=.,aes(x=SizeBin,y=n,group=Gender,color=Gender), size=plot.ctl$line.wdth,position="Stack")
      }
      p<-p+scale_y_continuous(breaks=pretty_breaks(n=plot.ctl$nbrks)) +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      if(plot)print(p)
    }
   tmp<-newdata.pred %>% {
                    if(version==1)
                      unite(.,United,timeperiod,month,week,Fishery,Set,remove=TRUE,sep="_")
                    else if(version==2)
                      select(.,-Fishery) %>%unite(United,timeperiod,month,week,RealFishery,Sp,Set,remove=TRUE,sep="_")
                    else if(nSp==1)
                      select(.,-Fishery) %>%
                        unite(United,timeperiod,month,week,Both,nsmpl,RealFishery,Sp,Gender,Set,remove=TRUE,sep="_")
                    else if(nSp==2)
                      select(.,-Fishery) %>%
                        unite(United,timeperiod,month,week,Male,Female,
                          nsmpl,RealFishery,Sp,Gender,Set,remove=TRUE,sep="_")
                    else
                      stop("nSp=",nSp)
                   } %>% gather(key=bin,value=frq,-United)->tmp2
  if(verbose)cat("L340;") # ;browser()
  longdata.pred<-tmp2 %>% { if(version==1)
                        separate(.,col=United,into=c("timeperiod","month","week","Fishery","Set"),sep="_")
                      else if(version==2)
                        separate(.,col=United,into=c("timeperiod","month","week","RealFishery","Sp","Set"),sep="_")
                      else  if(nSp==2)
                        separate(.,col=United,into=c("timeperiod","month","week","Male",
                          "Female","nsmpl","RealFishery","Sp","Gender","Set"),sep="_")
                      else if(nSp==1)
                        separate(.,col=United,into=c("timeperiod","month","week","Both","nsmpl",
                          "RealFishery","Sp","Gender","Set"),sep="_")
                      else
                        stop("nSp=",nSp)
                    }
  if(verbose)cat("L354 finished read.fit\n") # ;browser()
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
                        longdata.pred=longdata.pred)
  if(overall.composition.plot){
    results$p<-p
    results$plot.data<-plot.data
  }
  return(invisible(results))
}
