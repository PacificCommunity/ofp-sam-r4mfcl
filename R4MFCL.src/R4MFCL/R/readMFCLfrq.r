#' read frq file for conversion to 2sex model
#' @param frqfile file name of frq file
#' @param verbose make verbose?
#' @param convert4to6 convert version 4 frq to version 6 frq
#'
#' @author Yukio Takeuchi
#' @export
readMFCLFrq<-function(frqfile,verbose=TRUE,convert4to6=TRUE){
  if(verbose)cat("running readMFCLFrq\n")
  dat.org<-readLines(frqfile,warn=FALSE)
  dat1<-sapply(dat.org,trimws)
  dat<-sapply(dat1,gsub,pattern="\032",replacement="")
  dat0<-dat
  #cat("L11\n");browser()
  # parse all the numeric values into a long vector (allnums)
  temp.x<-temp <- strsplit(dat[2]," ")[[1]][1]

  if(!is.na(temp) && temp=="Start_time:") dat <- dat[-(1:2)]
  allnums <- NULL
  for(i in 1:length(dat)){
    # split along blank spaces
    mysplit <- strsplit(dat[i],split="[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit!=""]
    # if final value is a number is followed immediately by a pound ("1#"),
    # this needs to be split
    nvals <- length(mysplit)
    if(nvals>0) mysplit[nvals] <- strsplit(mysplit[nvals],"#")[[1]][1]
    # convert to numeric
    nums <- suppressWarnings(as.numeric(mysplit))
    if(sum(is.na(nums)) > 0){ 
      maxcol <- min((1:length(nums))[is.na(nums)])-1
    }else maxcol <- length(nums)
    if(maxcol > 0){
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }

  ## utility function to read and store data (to be written later)

   # Function to add vector to datalist

  add_vec<-function(datalist,length,name,verbose=TRUE){
    i<-datalist$'.i'
    dat<-datalist$'.dat'
    datalist$temp<-dat[i+1:length-1]
    if(verbose){
      cat(name,",i=",datalist$'.i'," : ")
    }
    datalist$'.i'<-i+length
    if(!is.na(name))names(datalist)[names(datalist)=="temp"]<-name
    if(verbose){
      print(datalist[[which(names(datalist)==name)]])
    }
#    cat("L51\n");browser()
    return(datalist)
  }


  add_df<-function(datalist,name,nrow,ncol,col.names=NULL,verbose=TRUE,comments=NULL,headerComments=NULL){
    k<-nrow*ncol
    i<-datalist$'.i'
    dat<-datalist$'.dat'
    df0<-as.data.frame(matrix(dat[i+1:k-1],nrow=nrow,ncol=ncol,byrow=TRUE))
    if(!is.null(col.names))colnames(df0)<-col.names
    if(is.null(comments)){
      rownames(df0)<-paste0(paste0("#_",name,collapse=""),1:nrow)
    }else{
      rownames(df0)<-comments
    }
    if(verbose){
      cat(name,",i=",datalist$'.i',"\n")
    }
    i <- i+k
    datalist$temp<-df0
    datalist$'.i'<-i
    if(!is.na(name))names(datalist)[names(datalist)=="temp"]<-name
    if(verbose){
      print(datalist[[which(names(datalist)==name)]])
    }
    return(datalist)
  }



  ## function to add an element to datalist
  add_elem<-function(datalist=NA,name,verbose=TRUE){
    i<-datalist$'.i'
    dat<-datalist$'.dat'
    datalist$temp<-dat[i]
 #   cat("L86\n");browser()
    if(verbose)cat(name,",i=",datalist$'.i'," ;")
    datalist$'.i'<-i+1
    if(!is.na(name))names(datalist)[names(datalist)=="temp"]<-name
    if(verbose)cat(datalist[[which(names(datalist)==name)]],"\n")
    return(datalist)
  }

  ## function to add list  to datalist
  add_list<-function(datalist=NA,name,length,length_each,verbose=TRUE){
    i<-datalist$'.i'
    dat<-datalist$'.dat'
    if(verbose)cat(name,",i=",datalist$'.i',"\n")
     datalist$temp<-list()
    for(j in 1:length){
      datalist$temp[[j]]<-dat[i+1:length_each[j]-1]; i <- i+length_each[j]
    }
    datalist$'.i'<-i
    if(!is.null(name))names(datalist)[names(datalist)=="temp"]<-name
  #  if(verbose)cat(name,",i=",datalist$'.i',"\n")
    return(datalist)
  }

  ## function to add frq data to list and then to datalist
  add_list_frqdata<-function(datalist=NA,name,length,
    frqVersion,
    verbose=TRUE,
    NumSpecies=0,
    NLbin,
    NWbin,convert4to6=FALSE){
    i<-datalist$'.i'
    dat<-datalist$'.dat'
    datalist$temp<-list()
    if(frqVersion==8){
      Nheader<- 7+NumSpecies*2
    }else if(frqVersion==6){
      Nheader<- 7
    }else if(frqVersion==9){
      Nheader<- 7+NumSpecies*4
    }else{
      Nheader<-6
    }
    frqHeader<-list()
    frqCE<-list()
    frqData<-list()
    if(NumSpecies>1){
      spDat<-list()
      spIndex<-list()
      spFlag<-list()
      if(frqVersion==9){
        spLenFlag<-list()
        spWtFlag<-list()
      }

    }
#    cat("L127\n");browser()
    for(j in 1:length){
    #  if(j==length){
    #    cat("L137, j=",j,",length=",length,"\n");browser()
    #  }
      temp0<-dat[i+1:Nheader-1]
      year<-temp0[1]
      month<-temp0[2]
      week<-temp0[3]
      fish<-temp0[4]
 #     cat("j:",j,",frqVersion:",frqVersion,"\n")
      if(NumSpecies>1){
        if(frqVersion==8){
          spIndex[1:NumSpecies]<-temp0[5+1:NumSpecies-1]
          spFlag[1:NumSpecies]<-temp0[5+NumSpecies+1:NumSpecies-1]
          i<-i+Nheader
        }else if(frqVersion==9){
          spIndex[1:NumSpecies]  <-temp0[5+1:NumSpecies-1]
          spFlag[1:NumSpecies]   <-temp0[5+NumSpecies+1:NumSpecies-1]
          spLenFlag[1:NumSpecies]<-temp0[5+NumSpecies*2+1:NumSpecies-1]
          spWtFlag[1:NumSpecies] <-temp0[5+NumSpecies*3+1:NumSpecies-1]
          i<-i+Nheader
        }
      }else{
        i<-i+Nheader
      }
      if(frqVersion>=6){
        catch<-rev(temp0)[3]
        effort<-rev(temp0)[2]
        pen<-rev(temp0)[1]
      }else{
        catch<-rev(temp0)[2]
        effort<-rev(temp0)[1]
        pen<-ifelse(!convert4to6,NA,-1)
      }
      if(j==length){
        cat("L168\n");browser()
      }
      if(dat[i]!=-1){
        Lfrq<-dat[i+1:NLbin-1]
        i<-i+NLbin
      }else{
        Lfrq<-dat[i]
        i<-i+1
      }
      if(j==length){
        cat("L188\n");browser()
      }
      if(dat[i]!=-1){
        Wfrq<-dat[i+1:NWbin-1]
        i<-i+NWbin
      }else{
        Wfrq<-dat[i]
        i<-i+1
      }
      frqHeader[[j]]<-c(year,month,week,fish)
      frqCE[[j]]<-c(catch,effort,pen)
      frqData[[j]]<-list(Lfrq=Lfrq,Wfrq=Wfrq)
      if(NumSpecies>1){
        if(frqVersion==8){
          spDat[[j]]<-list(spIndex=spIndex,spFlag=spFlag)
        }else if(frqVersion==9){
          spDat[[j]]<-list(spIndex=spIndex,spFlag=spFlag,spLenFlag=spLenFlag,spWtFlag=spWtFlag)
        }
      }
      cat("L196,",j,", ",length, " :");print(frqHeader[[j]])
    #  if(j==1945){
    #    cat("j=1945\n");browser()
    #  }
      if(frqVersion==8)cat(unlist(spDat[[j]]),"\n")
      if(frqVersion==9)cat(unlist(spDat[[j]]),"\n")
      cat(unlist(frqData[[j]]),"\n")
 #     cat("L186:\n");browser()
    }

    datalist$frq<-if(frqVersion %in% c(8,9)){
      list(frqHeader=frqHeader,spDat=spDat,frqCE=frqCE,frqData=frqData)
    }else{
      list(frqHeader=frqHeader,frqCE=frqCE,frqData=frqData)
    }
    datalist$'.i'<-i
    if(!is.null(name))names(datalist)[names(datalist)=="frq"]<-name
    if(verbose)cat(name,",i=",datalist$'.i',"\n")
    return(datalist)
  }



  # set initial position in the vector of numeric values
  i <- 1
  # create empty list to store quantities
  frqlist <- list()
  frqlist$'.i'<-i
  frqlist$'.dat'<-allnums
  frqlist$sourcefile<-frqfile

  if(verbose)cat("Starting to interpret and store data in frq file\n")
  frqlist<-add_elem(frqlist,"Nregions")
  frqlist<-add_elem(frqlist,"Nfish")
  frqlist<-add_elem(frqlist,"GenericDifusion")
  frqlist<-add_elem(frqlist,"NtagGrp")
  frqlist<-add_elem(frqlist,"styr")

  frqlist<-add_elem(frqlist,"NumSpecies")   ## Number of species/sex (not used if frqVersion<8)

  frqlist<-add_elem(frqlist,"LenOrAge") ## flag to indicate whether the frequency data are length (0) or age (1) data
  frqlist<-add_elem(frqlist,"Nrec") ## number of recruitments per year assumed in the model
  frqlist<-add_elem(frqlist,"RecMonth")
  frqlist<-add_elem(frqlist,"frqVersion")
  frqVersion<-frqlist$frqVersion
  if(frqlist$NumSpecies==0 )frqlist$NumSpecies<-1
  NumSpecies<-frqlist$NumSpecies
# .frq file version number – indicates for which version of MULTIFAN-CL the data
#  files were constructed (intended for backward compatibility, but in practice causes
#  MULTIFAN-CL to announce “Die yuppie scum” and quit when it encounters a .frq
#  file that is incompatible with the MULTIFAN-CL code version)
  if(frqlist$frqVersion %in% c(8,9)){
    multisexsp<-TRUE
  }else{
    multisexsp<-FALSE
  }
#  cat("L252\n");browser()
  if(frqVersion %in% c(8,9) & NumSpecies>1){
    frqlist<-add_vec(frqlist,length=NumSpecies,"NtagGrpBySp")
    if(sum(frqlist$NtagGrpBySp)!=frqlist$NtagGrp)
      stop("sum(frqlist$NtagGrpBySp)!=frqlist$NtagGrp")
    frqlist<-add_df(frqlist,"spRegion",nrow=NumSpecies,ncol=frqlist$Nregions,col.names=paste0("Region",1:frqlist$Nregion))
  }
  frqlist<-add_vec(frqlist,"regionSize",length=frqlist$Nregions)
  frqlist<-add_vec(frqlist,"fishRegion",length=frqlist$Nfish)
  if(frqlist$Nregions>1){
    frqlist<-add_df(frqlist,name="IncidentMat",nrow=NumSpecies,ncol=frqlist$Nregions*(frqlist$Nregions-1)/2)
  }
#  cat("L264\n");browser()
  frqlist<-add_df(frqlist,"frqDatFlags",nrow=5,ncol=frqlist$Nfish,col.names=paste0("FL",1:frqlist$Nfish),
    headerComments="# Data flags (for records 1, 0=catch in number; 1=catch in weight)")
  if(frqVersion>=6){
    frqlist<-add_df(frqlist,"Season_Region_Flags",nrow=frqlist$Nrec*NumSpecies,ncol=frqlist$Nregions,col.names=paste0("Region",1:frqlist$Nregions),
    headerComments="# Season-region flags")
  }else{
    if(convert4to6){
      frqlist$Season_Region_Flags<-as.data.frame(matrix(rep(1,frqlist$Nrec*frqlist$Nregions),ncol=frqlist$Nregions))
      colnames(frqlist$Season_Region_Flags)<-paste0("Region",1:frqlist$Nregions)
      rownames(frqlist$Season_Region_Flags)<-paste0("# Season-region flags_",1:(frqlist$Nrec*NumSpecies))
    }
  }

  frqlist<-add_elem(frqlist,"NmovesPerYr") # Number of movement per year

  if(frqlist$NmovesPerYr==1){
    frqlist<-add_elem(frqlist,"WeeksMoves") # Weeks in which movement occurs
  }else{
    frqlist<-add_vec(frqlist,"WeeksMoves",length=frqlist$NmovesPerYr) # Weeks in which movement occurs
  }
  if(frqlist$frqVersion>=6){
    frqlist<-add_vec(frqlist,"FishDataStruct",length=11) # Fishery data structure
    Nfrq<-frqlist$Nfrq<-frqlist$FishDataStruct[1]
    NLbin<-frqlist$NLbin<-frqlist$FishDataStruct[2]
    Lbin1st<-frqlist$Lbin1st<-frqlist$FishDataStruct[3]
    LbinWidth<-frqlist$LbinWidth<-frqlist$FishDataStruct[4]
    LbinFactro<-frqlist$LbinFactor<-frqlist$FishDataStruct[5]
    NWbin<-frqlist$NWbin<-frqlist$FishDataStruct[6]
    Wbin1st<-frqlist$Wbin1st<-frqlist$FishDataStruct[7]
    WbinWidth<-frqlist$WbinWidth<-frqlist$FishDataStruct[8]
    WbinFactor<-frqlist$WbinFactor<-frqlist$FishDataStruct[9]
    age_nage<-frqlist$age_nage<-frqlist$FishDataStruct[10]
    age_age1<-frqlist$age_age1<-frqlist$FishDataStruct[11]
  }else{
    cat("frqlist$frqVersion=",frqlist$frqVersion,"\n")
    frqlist<-add_vec(frqlist,"FishDataStruct",length=9)
        Nfrq<-frqlist$Nfrq<-frqlist$FishDataStruct[1]
    NLbin<-frqlist$NLbin<-frqlist$FishDataStruct[2]
    Lbin1st<-frqlist$Lbin1st<-frqlist$FishDataStruct[3]
    LbinWidth<-frqlist$LbinWidth<-frqlist$FishDataStruct[4]
    LbinFactro<-frqlist$LbinFactor<-frqlist$FishDataStruct[5]
    NWbin<-frqlist$NWbin<-frqlist$FishDataStruct[6]
    Wbin1st<-frqlist$Wbin1st<-frqlist$FishDataStruct[7]
    WbinWidth<-frqlist$WbinWidth<-frqlist$FishDataStruct[8]
    WbinFactor<-frqlist$WbinFactor<-frqlist$FishDataStruct[9]
    if(convert4to6){
      age_nage<-frqlist$age_nage<-frqlist$FishDataStruct[10]<- 0
      age_age1<-frqlist$age_age1<-frqlist$FishDataStruct[11]<- -1
    }
#    browser()
#    stop("Currently this R code does not support this version of frq file.")
  }

#
  if(verbose)cat("Starting to read the catch/effort/sample data\n")
  cat("L328\n");browser()
  frqlist<-add_list_frqdata(frqlist,name="frq",length=Nfrq,frqVersion=frqVersion,verbose=TRUE,
                                    NumSpecies=NumSpecies,NLbin=NLbin,NWbin=NWbin,convert4to6=convert4to6)
  if(frqlist$frqVersion==4 && convert4to6)frqlist$frqVersion<-6
  return(frqlist)
}

