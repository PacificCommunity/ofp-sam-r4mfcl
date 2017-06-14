 read.fit <-
function(fit.file,verbose=FALSE) {
  # Simon Hoyle March 2010
  # loads the observed and expected LF from the length.fit file by fishery and time period
  # YT March-April 2017 to catch up versions 2 and 3 fit file
  # YT June 2017 fixed for version 3 fit file with single species
#  require(stringr)
  require(dplyr)
  require(tidyr)
  require(magrittr)
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
#    cat("L12\n");browser()
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
#  cat("L44 read.fit.r\n");browser()
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
      if(version>2){
        spPtrloc<-seq(fishlocs[f]+2,by=7+pos.offset+nages,length.out=recsperfish[f])
        spPtr[[f]]<-if(nSp>1){
          data.frame(datfromstr(a[spPtrloc])) # data.frame(t(matrix(as.numeric(unlist(strsplit(str_trim(a[spPtrloc]),split="[[:blank:]]+"))),nrow=nSp)))
        }else{
          data.frame(t(datfromstr(a[spPtrloc])))
        }
        smplSz[[f]]<-as.numeric(a[spPtrloc+1])
      }
#      cat("L74\n");browser()
      obsloc <- seq(fishlocs[f]+4+pos.offset,by=7+pos.offset+nages,length.out=recsperfish[f])
      if(recsperfish[f]>1){
        obslf[[f]] <- data.frame(datfromstr(a[obsloc])) #data.frame(t(matrix(as.numeric(unlist(strsplit(str_trim(a[obsloc]),split="[[:blank:]]+"))),nrow=nbins)))
        predlf[[f]] <- data.frame(datfromstr(a[obsloc+1])) # data.frame(t(matrix(as.numeric(unlist(strsplit(str_trim(a[obsloc+1]),split="[[:blank:]]+"))),nrow=nbins)))
      }else{
        obslf[[f]] <- data.frame(t(datfromstr(a[obsloc]))) #data.frame(t(matrix(as.numeric(unlist(strsplit(str_trim(a[obsloc]),split="[[:blank:]]+"))),nrow=nbins)))
        predlf[[f]] <- data.frame(t(datfromstr(a[obsloc+1]))) # data.frame(t(matrix(as.numeric(unlist(strsplit(str_trim(a[obsloc+1]),split="[[:blank:]]+"))),nrow=nbins)))
      }
    }
  }
  if(verbose){cat("L89;")}
  newdata.obs<-lapply((1:nfish)[!sapply(dates,"is.null")],function(i){
    if(verbose)cat("L91;i=",i,";")
 #   if(i==9){cat("L83\n");browser()}
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
      ifelse(i<=nfish,1,2)
    }else{
      NA
    }
  #  if(i==9){cat("L110\n");browser()}
    if(version>2)tmp$Gender<-ifelse(fishSpPtr[i]==1,1,2)
    if(version>=2)tmp$RealFishery<-ifelse(i<=nfish,i,i-nfish)
    colnames(tmp)[1:3]<-c("Year","Month","Week")
    if(version>2)colnames(tmp)[4:5]<-c("Sp1","Sp2")
    tmp$Set<-"Obs"
 #   if(i==9){cat("L110\n") #;browser()}
    return(tmp)
  })
  if(verbose)cat("L119;") # ;browser()
  newdata.obs<-do.call("rbind",newdata.obs)
  if(verbose)cat("L121;") # ;browser()

###################################################
  newdata.pred<-lapply((1:nfish)[!sapply(dates,"is.null")],function(i){
    tmp<-if(version<=2){
      cbind(dates[[i]],predlf[[i]])
    }else{
    #  cbind(dates[[i]],spPtr[[i]],smplSz[[i]],predlf[[i]])
      if(dim(dates[[i]])[1]>1){
        cbind(dates[[i]],spPtr[[i]],smplSz[[i]],obslf[[i]])
      }else{
        cbind(dates[[i]],t(spPtr[[i]]),smplSz[[i]],obslf[[i]])
      }
    }
#    tmp<-cbind(dates[[i]],predlf[[i]])
#    if(version>=2)tmp<-cbind(tmp,spPtr[[i]],smplSz[[i]])
    tmp$Fishery<-i
    tmp$Sp<-if(version==1){
      NA
    }else if(version==2){
      ifelse(i<=nfish,1,2)
    }else{
      fishSpPtr[i]
    }
    if(version>2)tmp$Gender<-ifelse(fishSpPtr[i]==1,1,2)
    if(version>=2)tmp$RealFishery<-ifelse(i<=nfish,i,i-nfish)
        colnames(tmp)[1:3]<-c("Year","Month","Week")
    if(version>2)colnames(tmp)[4:5]<-c("Sp1","Sp2")
    tmp$Set<-"Pred"
    return(tmp)
  })
  if(verbose)cat("L152;") # ;browser()
  newdata.pred<-do.call("rbind",newdata.pred)
  if(verbose)cat("L154 in read.fit.r\n") #;browser()
  newdata<-rbind(newdata.obs,newdata.pred)
#  cat("L122\n");browser()
  colnames(newdata.pred)[1:3]<-colnames(newdata.obs)[1:3]<-colnames(newdata)[1:3]<-c("timeperiod","month","week")
  col.offset<-ifelse(version>2,3,1)
  if(version>2){
    if(nSp==2){
      colnames(newdata.pred)[4:6]<-colnames(newdata.obs)[4:6]<-colnames(newdata)[4:6]<-c("Male","Female","nsmpl")
    }else if(nSp==1){
      colnames(newdata.pred)[4:5]<-colnames(newdata.obs)[4:5]<-colnames(newdata)[4:5]<-c("Both","nsmpl")
    }
  }
 # cat("L132\n") #;browser()
  colnames(newdata.pred)[col.offset+1+nSp+1:nbins]<-
  colnames(newdata.obs)[col.offset+1+nSp+1:nbins]<-
  colnames(newdata)[col.offset+1+nSp+1:nbins]<-paste0(binfirst+(0:(nbins-1))*binwidth)
 # cat("L136\n")  #;browser()
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
                   } ->tmpx
 #             cat("L130 in read.fit.r");browser()
                   tmpx%>% gather(key=bin,value=frq,-United)->tmp2
#  cat("L154 in read.fit.r\n") # ;browser()
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
 # cat("L168\n") #;browser()

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
                   } ->tmpx
 #             cat("L130 in read.fit.r");browser()
                   tmpx%>% gather(key=bin,value=frq,-United)->tmp2
#  cat("L187 in read.fit.r\n") # ;browser()
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
# cat("L201\n")
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
                   } ->tmpx
 #             cat("L130 in read.fit.r");browser()
                   tmpx%>% gather(key=bin,value=frq,-United)->tmp2
 # cat("L219 in read.fit.r\n") # ;browser()
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
  return(list(dates=dates,obslf=obslf,predlf=predlf,version=version,
    header=list(nfish=nfish,nages=nages,nbins=nbins,binfirst=binfirst,binwidth=binwidth,nSp=nSp),longdata=longdata,longdata.obs=longdata.obs,longdata.pred=longdata.pred))
}
